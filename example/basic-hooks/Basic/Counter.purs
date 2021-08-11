module Hooks.Counter where

import Prelude

import Basic.Store as BS
import Data.Maybe (fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Halogen.Store.UseSelector (useSelector)

component
  :: forall query input output m
   . MonadStore BS.Action BS.Store m
  => H.Component query input output m
component = Hooks.component \_ _ -> Hooks.do
  count <- useSelector $ selectEq _.count
  Hooks.pure do
    let cnt = fromMaybe 0 count
    HH.div_
      [ HH.button
          [ HE.onClick \_ -> updateStore BS.Increment ]
          [ HH.text "Increment" ]
      , HH.text $ " Count: " <> show cnt <> " "
      , HH.button
          [ HE.onClick \_ -> updateStore BS.Decrement ]
          [ HH.text "Decrement" ]
      ]
