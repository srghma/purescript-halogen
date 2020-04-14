module Halogen.Query.ChildQuery where

import Prelude

import Data.Maybe (Maybe)
import Halogen.Data.Slot (SlotStorage)
import Unsafe.Coerce (unsafeCoerce)

data ChildQueryX (slots :: # Type) a

data ChildQuery slots query o a map mapvalue =
  ChildQuery
    (forall slot m. Applicative m => (slot query o -> m (Maybe mapvalue)) -> SlotStorage slots slot -> m (map mapvalue)) -- unpack :: evalChild -> slotStorage ->
    (query mapvalue) -- query :: ChildQueryItCanAcceptFromParent restOfParentComputationsChildShouldExecute
    (map mapvalue -> a) -- reply

instance functorChildQuery :: Functor (ChildQueryX slots) where
  map f = unChildQueryX \(ChildQuery u q k) ->
    mkChildQueryX (ChildQuery u q (f <<< k))

mkChildQueryX
  :: forall slots query o a map mapvalue
   . ChildQuery slots query o a map mapvalue
  -> ChildQueryX slots a
mkChildQueryX = unsafeCoerce

unChildQueryX
  :: forall slots a r
   . (forall query o map mapvalue. ChildQuery slots query o a map mapvalue -> r)
  -> ChildQueryX slots a
  -> r
unChildQueryX = unsafeCoerce
