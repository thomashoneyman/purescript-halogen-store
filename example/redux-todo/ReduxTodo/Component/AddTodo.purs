module ReduxTodo.Component.AddTodo (addTodo) where

import Prelude

import Data.Const (Const)
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore, updateStore)
import ReduxTodo.Store as Store
import ReduxTodo.Store.Todos (createTodo)
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event, preventDefault)

type Slot id slots = (addTodo :: H.Slot (Const Void) Void id | slots)

addTodo
  :: forall act slots m
   . MonadStore Store.Action Store.Store m
  => H.ComponentHTML act (Slot Unit slots) m
addTodo = HH.slot_ (Proxy :: Proxy "addTodo") unit component unit

data Action
  = HandleInput String
  | HandleSubmit Event

component
  :: forall q i o m
   . MonadStore Store.Action Store.Store m
  => H.Component q i o m
component = H.mkComponent
  { initialState: const ""
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  render state =
    HH.div_
      [ HH.form
          [ HE.onSubmit HandleSubmit ]
          [ HH.input
              [ HE.onValueInput HandleInput
              , HP.type_ HP.InputText
              , HP.value state
              ]
          , HH.button
              [ HP.type_ HP.ButtonSubmit ]
              [ HH.text "Add Todo" ]
          ]
      ]

  handleAction = case _ of
    HandleSubmit event -> do
      H.liftEffect $ preventDefault event
      H.get >>= String.trim >>> case _ of
        "" ->
          pure unit
        value -> do
          H.put ""
          updateStore $ createTodo value

    HandleInput value ->
      H.put value
