module Halogen.Aff.Driver.Implementation.Types where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen.Aff.Driver.State (RenderStateX)
import Halogen.Component (ComponentSlot, ComponentSlotBox)
import Halogen.HTML.Core as HC
import Halogen.Query.Input (Input)
import Web.DOM.Node (Node) as DOM

-- | `RenderSpec` allows for alternative driver implementations without the need
-- | to provide all of the driver machinery again, focusing just on the code
-- | needed to render components.
-- |
-- | The type variables are as follows:
-- | - `surface` is the type of value being rendered (`Halogen.HTML.Core.HTML`, for
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
-- | - The `surface` value to render, parameterised by the slot type for the
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
type RenderSpec surface r =
  { render
      :: forall s act ps o
       . (Input act -> Effect Unit)
      -> (ComponentSlotBox surface ps Aff act -> Effect (RenderStateX r))
      -> surface (ComponentSlot surface ps Aff act) act
      -> Boolean
      -> Maybe (r s act ps o)
      -> Effect (r s act ps o)
  , renderChild :: forall s act ps o. r s act ps o -> r s act ps o
  , removeChild :: forall s act ps o. r s act ps o -> Effect Unit
  , dispose :: forall s act ps o. r s act ps o -> Effect Unit
  }

type RenderSpecWithHydration r =
  { renderSpec :: RenderSpec HC.HTML r
  , hydrate
      :: forall s act ps o
       . (Input act -> Effect Unit)
      -> (ComponentSlotBox HC.HTML ps Aff act -> Effect (RenderStateX r))
      -> (ComponentSlotBox HC.HTML ps Aff act -> DOM.Node -> Effect (RenderStateX r))
      -> HC.HTML (ComponentSlot HC.HTML ps Aff act) act
      -> DOM.Node
      -> Effect (r s act ps o)
  }