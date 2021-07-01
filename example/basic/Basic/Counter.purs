module Basic.Counter where

import Prelude

import Basic.Store as BS
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Monad as Store
import Halogen.Store.Select (Selector, selectEq)

type Input = Unit

type State = Int

data Action
  = Increment
  | Decrement
  | Receive (Connected Int Input)

deriveState :: Connected Int Input -> State
deriveState { context } = context

selectCount :: Selector BS.Store Int
selectCount = selectEq _.count

component :: forall q o m. MonadStore BS.Action BS.Store m => H.Component q Unit o m
component = connect (\_ -> selectCount) $ H.mkComponent
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
          [ HH.text "Increment"]
      , HH.text $ " Count: " <> show count <> " "
      , HH.button
          [ HE.onClick \_ -> Decrement ]
          [ HH.text "Decrement" ]
      ]

  handleAction = case _ of
    Increment ->
      Store.updateStore BS.Increment

    Decrement ->
      Store.updateStore BS.Decrement

    Receive input ->
      H.put $ deriveState input
