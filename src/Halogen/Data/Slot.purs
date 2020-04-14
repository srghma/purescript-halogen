module Halogen.Data.Slot
  ( Slot
  , SlotStorage
  , empty
  , lookup
  , insert
  , pop
  , slots
  , foreachSlot
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (Tuple(..))
import Halogen.Data.OrdBox (OrdBox, mkOrdBox, unOrdBox)
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)

foreign import data AnySlot :: Type
foreign import data AnySlotIndex :: Type

-- query :: ChildQueryItCanAcceptFromParent restOfParentComputationsChildShouldExecute
-- output :: ChildMessageToParent
data Slot (query :: Type -> Type) output slotIndex

-- slots - record rows, e.g. ( output :: Slot query output slotIndex )
newtype SlotStorage
  -- phantom type, keys are Symbols, values are slot
  (slots :: # Type)
  -- Map value, this is what is stored in AnySlot??? `SlotStorage ps (DriverStateXRef h r)`
  (slot :: (Type -> Type) -- query
        -> Type -- output/message
        -> Type
  ) =
  SlotStorage
  (Map
    (Tuple
      String -- slot symbol
      (OrdBox AnySlotIndex) -- slotIndexEqFunction x slotIndexCompareFunction x slotIndex
    )
    AnySlot -- slot query output , partially applied????
  )

empty :: forall slots slot. SlotStorage slots slot
empty = SlotStorage Map.empty

-- lookup slot by symbol, index (if component is )
lookup
  :: forall sym otherSlots slots slot query output slotIndex
   . Row.Cons sym (Slot query output slotIndex) otherSlots slots
  => IsSymbol sym
  => Ord slotIndex
  => SProxy sym
  -> slotIndex
  -> SlotStorage slots slot
  -> Maybe (slot query output)
lookup sym slotIndex (SlotStorage m) =
  coerceSlot (Map.lookup (Tuple (reflectSymbol sym) (coerceBox (mkOrdBox slotIndex))) m)
  where
  coerceSlot :: Maybe AnySlot -> Maybe (slot query output)
  coerceSlot = unsafeCoerce

  coerceBox :: OrdBox slotIndex -> OrdBox AnySlotIndex
  coerceBox = unsafeCoerce

pop
  :: forall sym otherSlots slots slot query output slotIndex
   . Row.Cons sym (Slot query output slotIndex) otherSlots slots
  => IsSymbol sym
  => Ord slotIndex
  => SProxy sym
  -> slotIndex
  -> SlotStorage slots slot
  -> Maybe (Tuple (slot query output) (SlotStorage slots slot))
pop sym slotIndex (SlotStorage m) =
  coercePop (Map.pop (Tuple (reflectSymbol sym) (coerceBox (mkOrdBox slotIndex))) m)
  where
  coercePop :: Maybe (Tuple AnySlot (Map (Tuple String (OrdBox AnySlotIndex)) AnySlot)) -> Maybe (Tuple (slot query output) (SlotStorage slots slot))
  coercePop = unsafeCoerce

  coerceBox :: OrdBox slotIndex -> OrdBox AnySlotIndex
  coerceBox = unsafeCoerce

insert
  :: forall sym otherSlots slots slot query output slotIndex
   . Row.Cons sym (Slot query output slotIndex) otherSlots slots
  => IsSymbol sym
  => Ord slotIndex
  => SProxy sym
  -> slotIndex
  -> slot query output
  -> SlotStorage slots slot
  -> SlotStorage slots slot
insert sym slotIndex val (SlotStorage m) =
  SlotStorage (Map.insert (Tuple (reflectSymbol sym) (coerceBox (mkOrdBox slotIndex))) (coerceVal val) m)
  where
  coerceBox :: OrdBox slotIndex -> OrdBox AnySlotIndex
  coerceBox = unsafeCoerce

  coerceVal :: slot query output -> AnySlot
  coerceVal = unsafeCoerce

-- get all slots for the symbol
slots
  :: forall sym otherSlots slots slot query output slotIndex
   . Row.Cons sym (Slot query output slotIndex) otherSlots slots
  => IsSymbol sym
  => Ord slotIndex
  => SProxy sym
  -> SlotStorage slots slot
  -> Map slotIndex (slot query output)
slots sym (SlotStorage m) = Map.foldSubmap Nothing Nothing go m
  where
  slotIndex = reflectSymbol sym

  go (Tuple slotIndex' ob) val
    | slotIndex == slotIndex' = Map.singleton (unOrdBox (coerceBox ob)) (coerceVal val)
    | otherwise = mempty

  coerceBox :: OrdBox AnySlotIndex -> OrdBox slotIndex
  coerceBox = unsafeCoerce

  coerceVal :: AnySlot -> slot query output
  coerceVal = unsafeCoerce

foreachSlot
  :: forall m slots slot
   . Applicative m
  => SlotStorage slots slot
  -> (forall query output. slot query output -> m Unit)
  -> m Unit
foreachSlot (SlotStorage m) k = traverse_ (k <<< coerceVal) m
  where
  coerceVal :: forall query output. AnySlot -> slot query output
  coerceVal = unsafeCoerce
