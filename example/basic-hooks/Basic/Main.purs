module Hooks.Main where

import Prelude

import Basic.Counter as Counter
import Basic.Store as BS
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen.Aff as HA
import Halogen.Store.Monad (runStoreT)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = launchAff_ do
  body <- HA.awaitBody
  root <- runStoreT BS.initialStore BS.reduce Counter.component
  void $ runUI root unit body
