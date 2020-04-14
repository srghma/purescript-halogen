module Halogen.Aff.Driver.Eval
  ( Renderer
  , evalInput
  , evalQ
  , evalM
  , runLifecycleAround
  , queueIfOpenOrRunIfClosed
  ) where

import Prelude

import Control.Applicative.Free (hoistFreeAp, retractFreeAp)
import Control.Coroutine as CR
import Control.Monad.Fork.Class (fork)
import Control.Monad.Free (foldFree)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parSequence_, parallel, sequential)
import Data.Coyoneda (Coyoneda, liftCoyoneda)
import Data.Foldable (traverse_)
import Data.List (List, (:))
import Data.List as Data.List
import Data.Map as Data.Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, error, finally, killFiber, ParAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Aff.Driver.State (DriverState(..), DriverStateXRef(..), LifecycleHandlers, mapDriverState, unDriverStateX)
import Halogen.Data.Slot as Halogen.Data.Slot
import Halogen.Query.ChildQuery as CQ
import Halogen.Query.EventSource as ES
import Halogen.Query.HalogenM (ForkId(..), HalogenAp(..), HalogenF(..), HalogenM(..), SubscriptionId(..))
import Halogen.Query.HalogenQ as HQ
import Halogen.Query.Input (Input)
import Halogen.Query.Input as Input
import Unsafe.Reference (unsafeRefEq)

-- used only within this file
type Renderer h r
  = forall state query act ps i o
   . Ref LifecycleHandlers -- TODO: global or per component
  -> Ref (DriverState h r state query act ps i o) -- Driver of a CURRENT compoenent
  -> Effect Unit

-- uses evalM
evalInput -- evalInput??? or maybe maker of `inputHandler :: Input act -> Aff Unit`???
  :: forall h r state query act ps i o
   . Renderer h r
  -> Ref (DriverState h r state query act ps i o)
  -> Input act
  -> Aff Unit
evalInput render ref = case _ of
  Input.RefUpdate (Input.RefLabel p) el -> do
    liftEffect $ flip Ref.modify_ ref $ mapDriverState \st ->
      st { refs = Data.Map.alter (const el) p st.refs }
  Input.Action act -> do
    DriverState st <- liftEffect (Ref.read ref)
    evalM render ref (st.component.eval (HQ.Action act unit)) -- unit - eval will return HalogenM that contains unit

-- uses evalM
-- send Query to `eval` function
evalQ
  :: forall h r state query act ps i o nextComputationFn
   . Renderer h r
  -> Ref (DriverState h r state query act ps i o)
  -> query nextComputationFn
  -> Aff (Maybe nextComputationFn)
evalQ render ref q = do
  DriverState st <- liftEffect (Ref.read ref)
  -- HOW QUERYING WORKS 3
  evalM render ref (
    st.component.eval
      (HQ.Query
        -- what is `i` for Coyoneda? `i` is `a`, because liftCoyoneda is just `coyoneda identity`.
        -- TODO: there is no reason to use Coyoneda, right? to hide fact that f is functor??? WHAT
        (Just <$> liftCoyoneda q :: Coyoneda query (Maybe nextComputationFn))
        (const Nothing) -- default value is nothing
      )
    )

evalM
  :: forall h r state query act ps i o
   . Renderer h r
  -> Ref (DriverState h r state query act ps i o)
  -> HalogenM state act ps o Aff
  ~> Aff
evalM render initRef (HalogenM hm) = foldFree (go initRef) hm
  where
  go
    :: forall s' f' act' ps' i' o'
     . Ref (DriverState h r s' f' act' ps' i' o')
    -> HalogenF s' act' ps' o' Aff
    ~> Aff
  go ref = case _ of
    State f -> do
      DriverState (st@{ state, lifecycleHandlers }) <- liftEffect (Ref.read ref) -- TODO:: lifecycleHandlers are global or too per component???
      case f state of
        Tuple a state'
          | unsafeRefEq state state' -> pure a -- if state was requested (f == indentity), do nothing
          | otherwise -> do -- if state was changed - update driver, rerender, run lifecycles, return
              liftEffect $ Ref.write (DriverState (st { state = state' })) ref
              runLifecycleAround lifecycleHandlers (render lifecycleHandlers ref)
              pure a
    Subscribe mkEventSource callback -> do
      sid <- fresh SubscriptionId ref
      let (ES.EventSource setup) = mkEventSource sid
      DriverState ({ subscriptions }) <- liftEffect (Ref.read ref)
      _ ← fork do
        { producer, finalizer } <- setup
        let
          done = ES.Finalizer do
            subs <- liftEffect $ Ref.read subscriptions
            liftEffect $ Ref.modify_ (map (Data.Map.delete sid)) subscriptions
            when (maybe false (Data.Map.member sid) subs) (ES.finalize finalizer)
          consumer = do
            act <- CR.await
            subs <- lift $ liftEffect (Ref.read subscriptions)
            when ((Data.Map.member sid <$> subs) == Just true) do
              _ <- lift $ fork $ evalInput render ref (Input.Action act)
              consumer
        liftEffect $ Ref.modify_ (map (Data.Map.insert sid done)) subscriptions
        CR.runProcess (consumer `CR.pullFrom` producer)
        ES.finalize done
      pure (callback sid)
    Unsubscribe sid next -> do
      unsubscribe sid ref
      pure next
    Lift aff ->
      aff
    ChildQuery cq ->
      evalChildQuery ref cq
    Raise o a -> do
      DriverState { handlerRef, pendingOuts } <- liftEffect (Ref.read ref)
      handler <- liftEffect (Ref.read handlerRef)
      queueIfOpenOrRunIfClosed pendingOuts (handler o)
      pure a
    Par (HalogenAp p) ->
      sequential $ retractFreeAp $ hoistFreeAp (parallel <<< evalM render ref) p
    Fork hmu k -> do
      fid <- fresh ForkId ref
      DriverState ({ forks }) <- liftEffect (Ref.read ref)
      doneRef <- liftEffect (Ref.new false)
      fiber <- fork $ finally
        (liftEffect do
          Ref.modify_ (Data.Map.delete fid) forks
          Ref.write true doneRef)
        (evalM render ref hmu)
      liftEffect $ unlessM (Ref.read doneRef) do
        Ref.modify_ (Data.Map.insert fid fiber) forks
      pure (k fid)
    Kill fid a -> do
      DriverState ({ forks }) <- liftEffect (Ref.read ref)
      forkMap <- liftEffect (Ref.read forks)
      traverse_ (killFiber (error "Cancelled")) (Data.Map.lookup fid forkMap)
      pure a
    GetRef (Input.RefLabel p) k -> do
      DriverState { component, refs } <- liftEffect (Ref.read ref)
      pure $ k $ Data.Map.lookup p refs

  evalChildQuery -- this is executed by parent container driver, it contains the `children :: SlotStorage`
    :: forall s' f' act' ps' i' o' a'
     . Ref (DriverState h r s' f' act' ps' i' o')
    -> CQ.ChildQueryX ps' a'
    -> Aff a'
  evalChildQuery ref cqb = do
    DriverState st <- liftEffect (Ref.read ref)
    CQ.unChildQueryX (
      let
        myfunc :: forall query'' o'' f'' nextComputationFn'' . CQ.ChildQuery ps' query'' o'' a' f'' nextComputationFn'' -> Aff a'
        myfunc (CQ.ChildQuery
          (execEvaluatorForSelectedChildren :: forall slot m. Applicative m => (slot query'' o'' -> m (Maybe nextComputationFn'')) -> Halogen.Data.Slot.SlotStorage ps' slot -> m (f'' nextComputationFn'')) -- `f` here is `Maybe` for `query` and `Map slotIndex` for `queryAll` (without slotIndexes that responded Nothing)
          (query :: query'' nextComputationFn'')
          (reply :: f'' nextComputationFn'' -> a')) -- identity
          = do
          let
            evalChild :: DriverStateXRef h r query'' o'' -> ParAff (Maybe nextComputationFn'') -- for each child executed in parallel, intil `Free.Pure` is not returned
            evalChild (DriverStateXRef var) = parallel do
              (dsxOfChild) <- liftEffect (Ref.read var)
              -- HOW QUERYING WORKS 2
              unDriverStateX (\ds -> evalQ (render :: Renderer h r) ds.selfRef query) dsxOfChild
          reply <$> sequential (execEvaluatorForSelectedChildren evalChild st.children) -- execEvaluatorForSelectedChildren is different for `query` or `queryAll`
      in
        myfunc
    ) cqb

unsubscribe
  :: forall h r s' f' act' ps' i' o'
   . SubscriptionId
  -> Ref (DriverState h r s' f' act' ps' i' o')
  -> Aff Unit
unsubscribe sid ref = do
  DriverState ({ subscriptions }) <- liftEffect (Ref.read ref)
  subs <- liftEffect (Ref.read subscriptions)
  traverse_ ES.finalize (Data.Map.lookup sid =<< subs)

runLifecycleAround :: Ref LifecycleHandlers -> Effect ~> Aff
runLifecycleAround lchs f = do
  liftEffect $ Ref.write { initializers: Data.List.Nil, finalizers: Data.List.Nil } lchs -- empty if not empty
  result <- liftEffect f -- populate, render
  { initializers, finalizers } <- liftEffect $ Ref.read lchs -- read again
  traverse_ fork finalizers -- run and dont wait
  parSequence_ initializers -- run and wait
  pure result

fresh
  :: forall h r state query act ps i o a
   . (Int -> a)
  -> Ref (DriverState h r state query act ps i o)
  -> Aff a
fresh f ref = do
  DriverState st <- liftEffect (Ref.read ref)
  liftEffect $ Ref.modify' (\i -> { state: i + 1, value: f i }) st.fresh -- modify state and produce value at once

queueIfOpenOrRunIfClosed
  :: Ref (Maybe (List (Aff Unit)))
  -> Aff Unit
  -> Aff Unit
queueIfOpenOrRunIfClosed ref au =
  liftEffect (Ref.read ref) >>= case _ of
    Nothing -> au -- closed - run immidiately
    Just p -> liftEffect $ Ref.write (Just (au : p)) ref -- open - queue
