module NoAction.Counter where

import Prelude

import NoAction.Store as NAS
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Monad as Store
import Halogen.Store.Select (Selector, selectEq)

type Input = Unit

type Context = Int

type State = Int

selectState :: Selector NAS.Store Context
selectState = selectEq _.count

deriveState :: Connected Context Input -> State
deriveState { context } = context

data Action
  = Increment
  | Decrement
  | Receive (Connected Context Input)

component
  :: forall query output m
   . MonadStore NAS.Action NAS.Store m
  => H.Component query Input output m
component = connect selectState $ H.mkComponent
  { initialState: deriveState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  render count =
    HH.div_
      [ HH.button
          [ HE.onClick \_ -> Increment ]
          [ HH.text "Increment" ]
      , HH.text $ " Count: " <> show count <> " "
      , HH.button
          [ HE.onClick \_ -> Decrement ]
          [ HH.text "Decrement" ]
      ]

  handleAction = case _ of
    Increment ->
      Store.updateStore \store -> store { count = store.count + 1 }

    Decrement ->
      Store.updateStore \store -> store { count = store.count - 1 }

    Receive input ->
      H.put $ deriveState input
