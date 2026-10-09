module Hooks.Main where

import Prelude

import Hooks.Counter as Counter
import Hooks.Store as HS
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen.Aff as HA
import Halogen.Store.Monad (runStoreT)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = launchAff_ do
  body <- HA.awaitBody
  root <- runStoreT HS.initialStore HS.reduce Counter.component
  void $ runUI root unit body
