module Halogen.Store.Select where

import Prelude

newtype Selector store a = Selector { eq :: a -> a -> Boolean, select :: store -> a }

select :: forall store a. (a -> a -> Boolean) -> (store -> a) -> Selector store a
select eq = Selector <<< { eq, select: _ }

selectEq :: forall store a. Eq a => (store -> a) -> Selector store a
selectEq = Selector <<< { eq, select: _ }

selectAll :: forall store. Selector store store
selectAll = Selector { eq: \_ _ -> false, select: identity }
