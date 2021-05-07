module NoAction.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen.Aff as HA
import Halogen.Store.Monad (runStoreT)
import Halogen.VDom.Driver (runUI)
import NoAction.Counter as Counter
import NoAction.Store as NAS

main :: Effect Unit
main = launchAff_ do
  body <- HA.awaitBody
  root <- runStoreT NAS.initialStore (#) Counter.component
  runUI root unit body
