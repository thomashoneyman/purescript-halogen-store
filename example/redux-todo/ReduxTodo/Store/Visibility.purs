module ReduxTodo.Store.Visibility
  ( Visibility(..)
  , Store
  , initialStore
  , Action
  , reduce
  , Action'
  , setVisibility
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Variant (Variant)
import Data.Variant as Variant
import Type.Proxy (Proxy(..))

data Visibility = All | Completed | Active

derive instance eqVisibility :: Eq Visibility
derive instance genericVisibility :: Generic Visibility _

instance showVisibility :: Show Visibility where
  show = genericShow

type Store =
  { visibility :: Visibility
  }

initialStore :: Store
initialStore =
  { visibility: All
  }

data Action
  = SetVisibility Visibility

reduce :: Store -> Action -> Store
reduce store = case _ of
  SetVisibility visibility ->
    store { visibility = visibility }

type Action' v = (visibility :: Action | v)

injAction :: forall v. Action -> Variant (Action' v)
injAction = Variant.inj (Proxy :: Proxy "visibility")

setVisibility :: forall v. Visibility -> Variant (Action' v)
setVisibility = injAction <<< SetVisibility
