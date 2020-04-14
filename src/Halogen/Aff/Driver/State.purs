module Halogen.Aff.Driver.State
  ( LifecycleHandlers
  , DriverState(..)
  , mapDriverState
  , DriverStateXRef(..)
  , DriverStateRec
  , DriverStateX
  , unDriverStateX
  , mkDriverStateXRef
  , RenderStateX
  , renderStateX
  , renderStateX_
  , unRenderStateX
  , initDriverState
  ) where

import Prelude

import Data.List (List(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, Fiber)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Component (ComponentSpec)
import Halogen.Data.Slot (SlotStorage)
import Halogen.Data.Slot as SlotStorage
import Halogen.Query.EventSource (Finalizer)
import Halogen.Query.HalogenM (ForkId, SubscriptionId)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)

type LifecycleHandlers =
  { initializers :: List (Aff Unit)
  , finalizers :: List (Aff Unit)
  }

newtype DriverState h r state query act ps i o = DriverState (DriverStateRec h r state query act ps i o)

-- global driver state? per component?
type DriverStateRec h r state query act ps i o =
  { component :: ComponentSpec h state query act ps i o Aff
  , state :: state
  , refs :: M.Map String Element
  , children :: SlotStorage ps (DriverStateXRef h r)
  , childrenIn :: Ref (SlotStorage ps (DriverStateXRef h r))
  , childrenOut :: Ref (SlotStorage ps (DriverStateXRef h r))
  , selfRef :: Ref (DriverState h r state query act ps i o)
  , handlerRef :: Ref (o -> Aff Unit)
  , pendingQueries :: Ref (Maybe (List (Aff Unit)))
  , pendingOuts :: Ref (Maybe (List (Aff Unit)))
  , pendingHandlers :: Ref (Maybe (List (Aff Unit)))
  , rendering :: Maybe (r state act ps o) -- RenderStateX ???
  , fresh :: Ref Int
  , subscriptions :: Ref (Maybe (M.Map SubscriptionId (Finalizer Aff)))
  , forks :: Ref (M.Map ForkId (Fiber Unit))
  , lifecycleHandlers :: Ref LifecycleHandlers
  }

mapDriverState
  :: forall h r state query act ps i o
  . (DriverStateRec h r state query act ps i o -> DriverStateRec h r state query act ps i o)
  -> DriverState h r state query act ps i o
  -> DriverState h r state query act ps i o
mapDriverState f (DriverState ds) = DriverState (f ds)

newtype DriverStateXRef h r query o = DriverStateXRef (Ref (DriverStateX h r query o))

-- | A version of `DriverState` with the aspects relating to child components
-- | existentially hidden.
data DriverStateX
  (h :: Type -> Type -> Type)
  (r :: Type -> Type -> # Type -> Type -> Type)
  (query :: Type -> Type)
  (o :: Type)

mkDriverStateXRef
  :: forall h r state query act ps i o
   . Ref (DriverState h r state query act ps i o)
  -> Ref (DriverStateX h r query o)
mkDriverStateXRef = unsafeCoerce

unDriverStateX
  :: forall h r query i o x
   . (forall state act ps. DriverStateRec h r state query act ps i o -> x)
  -> DriverStateX h r query o
  -> x
unDriverStateX = unsafeCoerce

-- | A wrapper of `r` from `DriverState` with the aspects relating to child
-- | components existentially hidden.
data RenderStateX (r :: Type -> Type -> # Type -> Type -> Type)

mkRenderStateX
  :: forall r state query ps o m
   . m (r state query ps o)
  -> m (RenderStateX r)
mkRenderStateX = unsafeCoerce

unRenderStateX
  :: forall r x
   . (forall state query ps o. r state query ps o -> x)
  -> RenderStateX r
  -> x
unRenderStateX = unsafeCoerce

renderStateX
  :: forall m h r query o
   . Functor m
  => (forall s act ps. Maybe (r s act ps o) -> m (r s act ps o))
  -> DriverStateX h r query o
  -> m (RenderStateX r)
renderStateX f = unDriverStateX \st ->
  mkRenderStateX (f st.rendering)

renderStateX_
  :: forall m h r query o
   . Applicative m
  => (forall s act ps. r s act ps o -> m Unit)
  -> DriverStateX h r query o
  -> m Unit
renderStateX_ f = unDriverStateX \st -> traverse_ f st.rendering

initDriverState
  :: forall h r state query act ps i o
   . ComponentSpec h state query act ps i o Aff
  -> i
  -> (o -> Aff Unit)
  -> Ref LifecycleHandlers
  -> Effect (Ref (DriverStateX h r query o))
initDriverState component input handler lchs = do
  selfRef <- Ref.new (unsafeCoerce {})
  childrenIn <- Ref.new SlotStorage.empty
  childrenOut <- Ref.new SlotStorage.empty
  handlerRef <- Ref.new handler -- handles raises AND
  pendingQueries <- Ref.new (Just Nil)
  pendingOuts <- Ref.new (Just Nil) -- stores messages, TODO: always in open state
  pendingHandlers <- Ref.new Nothing -- default state is closed
  fresh <- Ref.new 1
  subscriptions <- Ref.new (Just M.empty)
  forks <- Ref.new M.empty
  let
    ds :: DriverStateRec h r state query act ps i o
    ds =
      { component
      , state: component.initialState input
      , refs: M.empty
      , children: SlotStorage.empty
      , childrenIn -- pop child from childrenIn -> process child -> put to childrenOut
      , childrenOut
      , selfRef
      , handlerRef
      , pendingQueries
      , pendingOuts
      , pendingHandlers
      , rendering: Nothing
      , fresh
      , subscriptions
      , forks
      , lifecycleHandlers: lchs
      }
  Ref.write (DriverState ds) selfRef
  pure $ mkDriverStateXRef selfRef
