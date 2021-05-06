module Basic.Store where

import Prelude

type Store = { count :: Int }

initialStore :: Store
initialStore = { count: 0 }

data Action
  = Increment
  | Decrement

reducer :: Store -> Action -> Store
reducer store = case _ of
  Increment -> store { count = store.count + 1 }
  Decrement -> store { count = store.count - 1 }
