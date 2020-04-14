module Halogen.Aff.Driver
  ( RenderSpec
  , runUI
  , module Halogen
  ) where

import Prelude

import Control.Coroutine as CR
import Control.Monad.Fork.Class (fork)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Parallel (parSequence_)
import Data.Either (Either(..), either)
import Data.List ((:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, isJust, isNothing)
import Data.Traversable (for_, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, killFiber, launchAff_, runAff_, try)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Console (warn)
import Effect.Exception (error, throw, throwException)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (HalogenIO)
import Halogen.Aff.Driver.Eval as Eval
import Halogen.Aff.Driver.State (DriverState(..), DriverStateXRef(..), DriverStateX, LifecycleHandlers, RenderStateX, initDriverState, mapDriverState, renderStateX, renderStateX_, unDriverStateX)
import Halogen.Component (Component, ComponentSlot, ComponentSlotSpecX, unComponent, unComponentSlotSpecX)
import Halogen.Data.Slot as Slot
import Halogen.Query.EventSource as Halogen.Query.EventSource
import Halogen.Query.HalogenQ as Halogen.Query.HalogenQ
import Halogen.Query.Input (Input)
import Halogen.Query.Input as Input

-- | `RenderSpec` allows for alternative driver implementations without the need
-- | to provide all of the driver machinery again, focusing just on the code
-- | needed to render components.
-- |
-- | The type variables are as follows:
-- | - `h` is the type of value being rendered (`Halogen.HTML.Core.HTML`, for
-- |   example).
-- | - `r` is the type for the "render state" for the driver. This is a value
-- |   that is stored for each component, that allows the driver to persist
-- |   state between each rendering of a component. This will differ entirely
-- |   for each driver. `r` accepts a number of parameters that will be
-- |   explained below.
-- |
-- | The "inner" type variables, used by `r` and the other functions are as
-- | follows:
-- | - `s` is the state type for the component.
-- | - `act` is the action type for the component
-- | - `ps` is the set of slots for addressing child components
-- | - `o` is the output message type for the component
-- |
-- | Note that none of these variables can escape `RenderSpec`'s functions. They
-- | need to be instantiated with each function call, as the same `RenderSpec`
-- | is used to deal with all components in the hierarchy.
-- |
-- | The `render` function is the main part of the spec, it accepts:
-- | - A "handler" function, for evaluating component queries. This is used to
-- |   implement event handlers in HTML-based drivers.
-- | - A "child" function for dealing with the rendering of children, returning
-- |   the render state for the child component in an existentially hidden
-- |   package. This return value would commonly be used to extract the rendered
-- |   subtree for the child to graft it in place of the child slot. The
-- |   existential package can be unwrapped with `Halogen.Aff.Driver.State.unRenderStateX`.
-- | - The `h` value to render, parameterised by the slot type for the
-- |   component's children. This slot type is what the "child" function
-- |   accepts.
-- | - The previous render state for the component. If the component has not
-- |   previously been initalized, this will be `Nothing`.
-- |
-- | The render function then returns the updated (or initial) render state for
-- | the component, which will be fed back into `render` the next time the
-- | component needs to update.
-- |
-- | The `renderChild` function's behaviour will be highly dependant on the
-- | particular driver implementing `RenderSpec`. Its purpose is to take a
-- | driver render state for a component and produce a new one that may remap
-- | the rendered value to be something more suitable for grafting during
-- | `render` of the parent. For the built-in `halogen-vdom` driver this is
-- | just `identity`. For the `virtual-dom` driver it wraps the rendered HTML
-- | in a widget, to prevent the `virtual-dom` algorithm from re-diffing
-- | values that we know are unchanged.
-- |
-- | The `removeChild` function is for drivers that need to perform some special
-- | cleanup when removing a component from the hierarchy. In the `halogen-vdom`
-- | driver this actually performs the `removeChild` from the DOM. For the
-- | `virtual-dom` driver nothing needs to happen here, so it is
-- | `const (pure unit)`.
-- |
-- | The `dispose` function is called when the top level component is disposed of
-- | via `HalogenIO`.
type RenderSpec h r =
  { render
      :: forall s act ps o
      -- user provided, handle events
       . (Input act -> Effect Unit)
      -- child renderer, returns child component state
      -- RenderStateX means after child is rendered, we can no longer access it's state from this file
      -> (ComponentSlotSpecX h ps Aff act -> Effect (RenderStateX r))
      -- this is what passed to child renderer (spec or thunk that returns spec)
      -> h (ComponentSlot h ps Aff act) act
      -> Maybe (r s act ps o) -- Nothing if initial rendering
      -> Effect (r s act ps o)
  , renderChild :: forall s act ps o. r s act ps o -> r s act ps o
  , removeChild :: forall s act ps o. r s act ps o -> Effect Unit
  , dispose :: forall s act ps o. r s act ps o -> Effect Unit
  }

runUI
  :: forall h r f i o
   . RenderSpec h r
  -> Component h f i o Aff
  -> i
  -> Aff (HalogenIO f o Aff)
runUI renderSpec component i = do
  lchs <- liftEffect newLifecycleHandlers
  fresh <- liftEffect $ Ref.new 0
  disposed <- liftEffect $ Ref.new false
  Eval.runLifecycleAround lchs do -- TODO: for static render I'll have to run it without initializers, except there is no out queries
    listeners <- Ref.new M.empty
    dsx <- Ref.read =<< runComponent lchs (rootOutputHandler listeners) i component
    unDriverStateX (\st ->
      pure
        { query: evalDriver disposed st.selfRef
        , subscribe: rootSubscribe fresh listeners
        , dispose: rootDispose disposed lchs dsx listeners
        }) dsx

  where

  evalDriver -- runs evalQ if component is not yet disposed
    :: forall s f' act ps i' o'
     . Ref Boolean
    -> Ref (DriverState h r s f' act ps i' o')
    -> forall a. (f' a -> Aff (Maybe a))
  evalDriver disposed ref q =
    liftEffect (Ref.read disposed) >>=
      if _
        then pure Nothing
        else Eval.evalQ render ref q

  rootOutputHandler :: Ref (M.Map Int (AVar.AVar o)) -> o -> Aff Unit -- puts message to the listeners map
  rootOutputHandler ref message = do
    listeners <- liftEffect $ Ref.read ref
    traverse_ fork $ map (AVar.put message) listeners

  rootSubscribe -- send to HalogenIO subscriber
    :: Ref Int
    -> Ref (M.Map Int (AVar.AVar o))
    -> CR.Consumer o Aff Unit
    -> Aff Unit
  rootSubscribe fresh ref consumer = do
    inputVar <- AVar.empty
    listenerId <- liftEffect do
      listenerId <- Ref.read fresh
      Ref.modify_ (_ + 1) fresh
      Ref.modify_ (M.insert listenerId inputVar) ref
      pure listenerId
    let
      producer :: CR.Producer o Aff Unit
      producer = CR.producer $ either (const (Right unit)) Left <$> try (AVar.take inputVar) -- take and wait, map value to `Right Unit`, map error to `Left error`
    void $ fork do
      CR.runProcess (CR.connect producer consumer) -- run till it's finished, error is not possible in producer, but possible in consumer, TODO: what happens then
      liftEffect $ Ref.modify_ (M.delete listenerId) ref -- remove Avar
      AVar.kill (error "ended") inputVar -- kill in case of something, e.g. is not killed

  runComponent
    :: forall f' i' o'
     . Ref LifecycleHandlers
    -> (o' -> Aff Unit) -- how to handle output: IF this is root component THEN fill Avar, it will be handled to consumers ELSE if this is child THEN pass to parent
    -> i' -- current component input
    -> Component h f' i' o' Aff -- this is eq to ComponentSpec
    -> Effect (Ref (DriverStateX h r f' o'))
  runComponent lchsRef {- parent/global/rootLchsRef ??? -} outputHandler input = unComponent \componentSpec -> do
    lchsRef' <- newLifecycleHandlers -- new
    driverStateXRef <- initDriverState componentSpec input outputHandler lchsRef' -- driver with new
    lchsPrev <- Ref.read lchsRef
    Ref.write { initializers: L.Nil, finalizers: lchsPrev.finalizers } lchsRef -- remove initializers from old, WHY??
    unDriverStateX (render lchsRef <<< _.selfRef) =<< Ref.read driverStateXRef -- render thyself with old

    -- join onto lchsRef:
    --   this component lchsRef (was empty, populated during render)
    --   parentInitializer (provided by user, from driver)
    --   pendingQueries (driver)
    --   pendingOuts (driver)
    --   lchsPrev.initializers

    --   driverState.initializers (lchsRef') is ignored, WHY??
    squashChildInitializers lchsRef lchsPrev.initializers =<< Ref.read driverStateXRef -- lchsRef is populated again, append also old initializers
    pure driverStateXRef

  render
    :: forall s f' act ps i' o'
     . Ref LifecycleHandlers
    -> Ref (DriverState h r s f' act ps i' o')
    -> Effect Unit
  render lchsRef var = Ref.read var >>= \(DriverState ds) -> do
    shouldProcessHandlers <- isNothing <$> Ref.read ds.pendingHandlers
    when shouldProcessHandlers $ Ref.write (Just L.Nil) ds.pendingHandlers -- open
    Ref.write Slot.empty ds.childrenOut
    Ref.write ds.children ds.childrenIn -- ds.children is empty on first render
    let
      -- The following 3 defs are working around a capture bug, see #586
      -- identity here is to prevent accessors inlining back, and prevent memory leak
      pendingHandlers = identity ds.pendingHandlers
      pendingQueries = identity ds.pendingQueries -- TODO: panding messages???????
      selfRef = identity ds.selfRef

      inputHandler :: Input act -> Aff Unit
      inputHandler = Eval.queueIfOpenOrRunIfClosed pendingHandlers <<< void <<< Eval.evalInput render selfRef -- on input - add to queue

      childInputHandler :: act -> Aff Unit
      childInputHandler = Eval.queueIfOpenOrRunIfClosed pendingQueries <<< inputHandler <<< Input.Action -- messages from child to parent, reuse inputHandler machinery (TODO: we queue )
    (rendering :: r s act ps o') <-
      renderSpec.render -- this is when VDOM is rendered
        (handleAff <<< inputHandler) -- how to handle input
        (renderChild lchsRef childInputHandler ds.childrenIn ds.childrenOut) -- how to render child
        (ds.component.render ds.state) -- html
        ds.rendering -- current rendering state, Nothing if first time
    children <- Ref.read ds.childrenOut
    childrenIn <- Ref.read ds.childrenIn -- TODO: now populated with children to remove?

    -- remove removed children
    Slot.foreachSlot childrenIn \(DriverStateXRef childDriverStateXRef) -> do -- render each child TODO: wtf
      childDriverState <- Ref.read childDriverStateXRef
      renderStateX_ renderSpec.removeChild childDriverState -- remove all children from DOM
      collectFinalizersWithCleanSubsEffect lchsRef childDriverState -- TODO: shouldnt it execute finalizers???

    flip Ref.modify_ ds.selfRef $ mapDriverState \ds' ->
      ds' { rendering = Just rendering, children = children } -- update

    when shouldProcessHandlers do -- initial rendering = false, subsequent =
      flip tailRecM unit \_ -> do
        handlers <- Ref.read pendingHandlers
        Ref.write (Just L.Nil) pendingHandlers -- open state
        traverse_ (handleAff <<< traverse_ fork <<< L.reverse) handlers
        mmore <- Ref.read pendingHandlers
        if maybe false L.null mmore -- if nothing OR empty
          then Ref.write Nothing pendingHandlers $> Done unit -- put in closed state and quit
          else pure $ Loop unit -- try again

  renderChild
    :: forall ps act
     . Ref LifecycleHandlers
    -> (act -> Aff Unit)
    -> Ref (Slot.SlotStorage ps (DriverStateXRef h r)) -- in - on start - contains current children, on end - contains children that were not rendered and thus should be removed
    -> Ref (Slot.SlotStorage ps (DriverStateXRef h r)) -- out - on start - empty, on end - rendered children and thus should be preserved
    -> ComponentSlotSpecX h ps Aff act
    -> Effect (RenderStateX r)
  renderChild lchsRef outputHandler childrenInRef childrenOutRef = -- when vdom reached child widget to render
    unComponentSlotSpecX \componentSlotSpec -> do
      popChildResult <- componentSlotSpec.pop <$> Ref.read childrenInRef

      driverStateXRef <- case popChildResult of
        -- this is a patch phase and child driver is already in slot storage
        Just (Tuple (DriverStateXRef driverStateXRef) otherChildrenIn) -> do
          Ref.write otherChildrenIn childrenInRef
          dsx <- Ref.read driverStateXRef
          unDriverStateX (\st -> do
            -- update current handler ref
            flip Ref.write st.handlerRef $
              maybe (pure unit) outputHandler <<< componentSlotSpec.outputToAction

            handleAff $
              Eval.evalM render st.selfRef (st.component.eval componentSlotSpec.input)) dsx
          pure driverStateXRef

        -- child is new
        Nothing ->
          case componentSlotSpec.input of
            Halogen.Query.HalogenQ.Receive slotInput _ ->
              runComponent
                lchsRef
                (maybe (pure unit) outputHandler <<< componentSlotSpec.outputToAction) -- how to handle output: transform and handle with VDOM handler
                slotInput
                componentSlotSpec.component
            _ ->
              throw "Halogen internal error: slot input was not a Receive query" -- it should be here if it was created by `mkEval` function

      isDuplicate <- isJust <<< componentSlotSpec.get <$> Ref.read childrenOutRef

      when isDuplicate
        $ warn "Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"

      Ref.modify_ (componentSlotSpec.set $ DriverStateXRef driverStateXRef) childrenOutRef

      Ref.read driverStateXRef >>= (renderStateX (\rendering -> case rendering of
        Nothing -> throw "Halogen internal error: child was not initialized in renderChild" -- it should be initialized in runComponent above
        Just r -> pure (renderSpec.renderChild r)))

  squashChildInitializers
    :: forall f' o'
     . Ref LifecycleHandlers
    -> L.List (Aff Unit)
    -> DriverStateX h r f' o'
    -> Effect Unit
  squashChildInitializers lchsRef prevInits =
    unDriverStateX \st -> do
      let
        parentInitializer :: Aff Unit
        parentInitializer = Eval.evalM render st.selfRef (st.component.eval (Halogen.Query.HalogenQ.Initialize unit))
      Ref.modify_ (\lchs ->
        { initializers: (do
            parSequence_ (L.reverse lchs.initializers)
            parentInitializer
            liftEffect do
              handlePending st.pendingQueries
              handlePending st.pendingOuts) : prevInits
        , finalizers: lchs.finalizers
        }) lchsRef

  collectFinalizersWithCleanSubsEffect
    :: forall f' o'
     . Ref LifecycleHandlers
    -> DriverStateX h r f' o'
    -> Effect Unit
  collectFinalizersWithCleanSubsEffect lchsRef = do
    unDriverStateX \st -> do
      cleanupSubscriptionsAndForks (DriverState st)
      let thisComponentFinalizer = Eval.evalM render st.selfRef (st.component.eval (Halogen.Query.HalogenQ.Finalize unit))
      Ref.modify_ (\handlers ->
        { initializers: handlers.initializers
        , finalizers: thisComponentFinalizer : handlers.finalizers
        }) lchsRef
      Slot.foreachSlot st.children \(DriverStateXRef ref) -> do
        dsx <- Ref.read ref
        collectFinalizersWithCleanSubsEffect lchsRef dsx

  rootDispose :: forall f' o'
     . Ref Boolean
    -> Ref LifecycleHandlers
    -> DriverStateX h r f' o'
    -> Ref (M.Map Int (AVar.AVar o'))
    -> Aff Unit
  rootDispose disposed lchsRef dsx listenersRef = Eval.runLifecycleAround lchsRef do -- this is when collected finalizers are beign run
    Ref.read disposed >>=
      if _
        then pure unit
        else do
          Ref.write true disposed
          traverse_ (launchAff_ <<< AVar.kill (error "disposed")) =<< Ref.read listenersRef
          collectFinalizersWithCleanSubsEffect lchsRef dsx
          unDriverStateX (traverse_ renderSpec.dispose <<< _.rendering) dsx

newLifecycleHandlers :: Effect (Ref LifecycleHandlers)
newLifecycleHandlers = Ref.new { initializers: L.Nil, finalizers: L.Nil }

handlePending :: Ref (Maybe (L.List (Aff Unit))) -> Effect Unit -- runs all effects in the queue, sets it to Nothing
handlePending ref = do
  queue <- Ref.read ref
  Ref.write Nothing ref
  for_ queue (handleAff <<< traverse_ fork <<< L.reverse)

cleanupSubscriptionsAndForks
  :: forall h r s f act ps i o
   . DriverState h r s f act ps i o
  -> Effect Unit
cleanupSubscriptionsAndForks (DriverState ds) = do
  traverse_ (handleAff <<< traverse_ (fork <<< Halogen.Query.EventSource.finalize)) =<< Ref.read ds.subscriptions
  Ref.write Nothing ds.subscriptions
  traverse_ (handleAff <<< killFiber (error "finalized")) =<< Ref.read ds.forks
  Ref.write M.empty ds.forks

-- We could perhaps do something more intelligent now this isn't baked into
-- the virtual-dom rendering. It hasn't really been a problem so far though.
handleAff :: forall a. Aff a -> Effect Unit
handleAff = runAff_ (either throwException (const (pure unit)))
