module Hooks.Counter where

import Prelude

import Hooks.Store as HS
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Halogen.Store.UseSelector (useSelector)

component
  :: forall query input output m
   . MonadStore HS.Action HS.Store m
  => H.Component query input output m
component = Hooks.component \_ _ -> Hooks.do
  count <- useSelector $ selectEq _.count
  Hooks.pure do
    case count of
      Nothing -> HH.text ""
      Just cnt ->
        HH.div_
          [ HH.button
              [ HE.onClick \_ -> updateStore HS.Increment ]
              [ HH.text "Increment" ]
          , HH.text $ " Count: " <> show cnt <> " "
          , HH.button
              [ HE.onClick \_ -> updateStore HS.Decrement ]
              [ HH.text "Decrement" ]
          ]
