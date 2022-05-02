module ReduxTodo.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen.Aff as HA
import Halogen.Store.Monad (runStoreT)
import Halogen.VDom.Driver (runUI)
import ReduxTodo.Component.App (app)
import ReduxTodo.Store as Store

main :: Effect Unit
main = launchAff_ do
  body <- HA.awaitBody
  root <- runStoreT Store.initialStore Store.reduce app
  void $ runUI root unit body
