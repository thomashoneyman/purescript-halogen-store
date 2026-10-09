module NoAction.Store where

type Store = { count :: Int }

initialStore :: Store
initialStore = { count: 0 }

type Action = Store -> Store
