module Halogen.VDom.Driver
  ( runUI
  , module Halogen.Aff.Driver
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried as EFn
import Halogen.Aff.Driver (HalogenIO)
import Halogen.Aff.Driver as HalogenAffDriver
import Halogen.Aff.Driver.State (RenderStateX, unRenderStateX)
import Halogen.Component (Component, ComponentSlot(..), ComponentSlotSpecX)
import Halogen.HTML.Core (HTML(..), Prop)
import Halogen.Query.Input (Input)
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as VP
import Halogen.VDom.Thunk (Thunk)
import Halogen.VDom.Thunk as Thunk
import Unsafe.Reference (unsafeRefEq)
import Web.DOM.Document (Document) as DOM
import Web.DOM.Element (Element) as DOM
import Web.DOM.Node (Node, appendChild, removeChild, parentNode, nextSibling, insertBefore) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (HTMLElement) as DOM
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document) as DOM

type VHTML action slots =
  V.VDom (Array (Prop (Input action))) (ComponentSlot HTML slots Aff action)

type ChildRenderer action slots
  = ComponentSlotSpecX HTML slots Aff action -> Effect (RenderStateX RenderState)

newtype RenderState state action slots output =
  RenderState
    { node :: DOM.Node
    , machine :: V.Step (VHTML action slots) DOM.Node
    , childRendererRef :: Ref (ChildRenderer action slots) -- comes from Halogen.Aff.Driver (TODO: can it be changed?)
    }

type HTMLThunk slots action =
  Thunk (HTML (ComponentSlot HTML slots Aff action)) action -- Thunk f i, where `f i` is a rendering result, `f` is `Html`, `i` is `action` (could be Unit)

type WidgetState slots action =
  Maybe (V.Step (HTMLThunk slots action) DOM.Node)

mkSpec
  :: forall action slots
   . (Input action -> Effect Unit)
  -> Ref (ChildRenderer action slots)
  -> DOM.Document
  -> V.VDomSpec
      (Array (VP.Prop (Input action)))
      (ComponentSlot HTML slots Aff action)
mkSpec handler childRendererRef document =
  V.VDomSpec { buildWidget, buildAttributes, document }
  where

  buildAttributes
    :: DOM.Element
    -> V.Machine (Array (VP.Prop (Input action))) Unit
  buildAttributes = VP.buildProp handler

  buildWidget
    :: V.VDomSpec
          (Array (VP.Prop (Input action)))
          (ComponentSlot HTML slots Aff action)
    -> V.Machine
          (ComponentSlot HTML slots Aff action)
          DOM.Node
  buildWidget spec = render
    where

    -- render widget
    render :: V.Machine (ComponentSlot HTML slots Aff action) DOM.Node
    render = EFn.mkEffectFn1 \slot ->
      case slot of
        ComponentSlot componentSlotSpecX ->
          EFn.runEffectFn1 renderComponentSlot componentSlotSpecX
        ThunkSlot t -> do
          nextThunkStep <- EFn.runEffectFn1 buildThunk t
          pure $ V.mkStep $ V.Step (V.extract nextThunkStep) (Just nextThunkStep :: WidgetState slots action) patch done

    -- patch widget
    patch
      :: EFn.EffectFn2 (WidgetState slots action)
            (ComponentSlot HTML slots Aff action)
            (V.Step (ComponentSlot HTML slots Aff action) DOM.Node)
    patch = EFn.mkEffectFn2 \st slot ->
      case st of
        Just thunkStep -> case slot of
          ComponentSlot componentSlotSpecX -> do
            EFn.runEffectFn1 V.halt thunkStep -- there was a thunk here before in place of this component, remove thunk, add slot
            EFn.runEffectFn1 renderComponentSlot componentSlotSpecX
          ThunkSlot t -> do
            thunkStep' <- EFn.runEffectFn2 V.step thunkStep t -- compare prev thunk with new, render if changed
            pure $ V.mkStep $ V.Step (V.extract thunkStep') (Just thunkStep') patch done
        _ -> EFn.runEffectFn1 render slot

    buildThunk :: V.Machine (HTMLThunk slots action) DOM.Node -- build a machine that takes a new thunk, compares with old (it's hidden in machine state), and outputs Node
    buildThunk = Thunk.buildThunk unwrap spec

    renderComponentSlot
      :: EFn.EffectFn1
            (ComponentSlotSpecX HTML slots Aff action)
            (V.Step (ComponentSlot HTML slots Aff action) DOM.Node)
    renderComponentSlot = EFn.mkEffectFn1 \componentSlotSpecX -> do
      childRenderer <- Ref.read childRendererRef
      rsx <- childRenderer componentSlotSpecX
      let node = getNode rsx
      pure $ V.mkStep $ V.Step node Nothing patch done

  -- halt widget
  done :: EFn.EffectFn1 (WidgetState slots action) Unit
  done = EFn.mkEffectFn1 \st ->
    case st of
      Just step -> EFn.runEffectFn1 V.halt step
      _ -> pure unit

  getNode :: RenderStateX RenderState -> DOM.Node
  getNode = unRenderStateX (\(RenderState { node }) -> node)

-- high level `runUI` function, uses low level `runUI` function
runUI
  :: forall query input output
   . Component HTML query input output Aff
  -> input
  -> DOM.HTMLElement
  -> Aff (HalogenIO query output Aff)
runUI component i element = do
  document <- liftEffect $ HTMLDocument.toDocument <$> (DOM.document =<< DOM.window)
  HalogenAffDriver.runUI (renderSpec document element) component i

renderSpec
  :: DOM.Document
  -> DOM.HTMLElement
  -> HalogenAffDriver.RenderSpec HTML RenderState
renderSpec document container =
    { render
    , renderChild: identity -- TODO: what could be there?
    , removeChild -- how widget removes itself
    , dispose: removeChild
    }
  where

  render
    :: forall state action slots output
     . (Input action -> Effect Unit) -- user provided input handler per input
    -> (ComponentSlotSpecX HTML slots Aff action -> Effect (RenderStateX RenderState))
    -> HTML (ComponentSlot HTML slots Aff action) action
    -> Maybe (RenderState state action slots output) -- Nothing if this is initial application render
    -> Effect (RenderState state action slots output)
  render inputHandler childRenderer (HTML vdom) =
    case _ of
      Nothing -> do
        childRendererRef <- Ref.new childRenderer
        let spec = mkSpec inputHandler childRendererRef document -- pass low level VdomSpec to intermediate level `runUI` function
        machine <- EFn.runEffectFn1 (V.buildVDom spec) vdom -- buildVDOM
        let node = V.extract machine
        void $ DOM.appendChild node (HTMLElement.toNode container)
        pure $ RenderState { machine, node, childRendererRef }
      Just (RenderState { machine, node, childRendererRef }) -> do
        Ref.write childRenderer childRendererRef
        parent <- DOM.parentNode node
        nextSib <- DOM.nextSibling node
        machine' <- EFn.runEffectFn2 V.step machine vdom -- runs machine
        let newNode = V.extract machine'
        when (not unsafeRefEq node newNode) do
          substInParent newNode nextSib parent
        pure $ RenderState { machine: machine', node: newNode, childRendererRef }

removeChild :: forall state action slots output. RenderState state action slots output -> Effect Unit
removeChild (RenderState { node }) = do
  npn <- DOM.parentNode node
  traverse_ (\pn -> DOM.removeChild node pn) npn

-- newNode nextSib parent
-- TODO: it doesnt substitue / removes old node from parent, in appends
substInParent :: DOM.Node -> Maybe DOM.Node -> Maybe DOM.Node -> Effect Unit
substInParent newNode (Just sib) (Just pn) = void $ DOM.insertBefore newNode sib pn
substInParent newNode Nothing (Just pn) = void $ DOM.appendChild newNode pn
substInParent _ _ _ = pure unit -- do nothing if parent doesnt exist
