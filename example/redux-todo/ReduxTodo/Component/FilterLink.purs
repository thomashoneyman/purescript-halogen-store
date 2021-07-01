module ReduxTodo.Component.FilterLink (filterLink) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (Selector, selectEq)
import ReduxTodo.Store as Store
import ReduxTodo.Store.Visibility (Visibility, setVisibility)
import Type.Proxy (Proxy(..))

type Slot id slots = ( filterLink :: H.Slot (Const Void) Void id | slots )

filterLink
  :: forall action id slots m
   . MonadStore Store.Action Store.Store m
  => Ord id
  => id
  -> Input
  -> H.ComponentHTML action (Slot id slots) m
filterLink id = HH.slot_ (Proxy :: Proxy "filterLink") id component

type Input = Visibility

type State =
  { filter :: Visibility
  , active :: Boolean
  }

selectState :: Selector Store.Store Visibility
selectState = selectEq _.visibility.visibility

deriveState :: Connected Visibility Input -> State
deriveState { context, input } =
  { filter: input
  , active: input == context
  }

data Action
  = HandleClick
  | Receive (Connected Visibility Visibility)

component
  :: forall q o m
   . MonadStore Store.Action Store.Store m
  => H.Component q Input o m
component = connect (\_ -> selectState) $ H.mkComponent
  { initialState: deriveState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  render { filter, active } =
    HH.button
      [ HE.onClick \_ -> HandleClick
      , HP.disabled active
      ]
      [ HH.text $ show filter ]

  handleAction = case _ of
    HandleClick -> do
      { filter } <- H.get
      updateStore $ setVisibility filter

    Receive input ->
      H.put $ deriveState input
