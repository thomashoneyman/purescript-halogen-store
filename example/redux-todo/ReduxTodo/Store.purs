module ReduxTodo.Store where

import Data.Variant (Variant)
import Data.Variant as Variant
import ReduxTodo.Store.Todos as Todos
import ReduxTodo.Store.Visibility as Visibility

type Store =
  { todos :: Todos.Store
  , visibility :: Visibility.Store
  }

initialStore :: Store
initialStore =
  { todos: Todos.initialStore
  , visibility: Visibility.initialStore
  }

type Action = Variant
  ( todos :: Todos.Action
  , visibility :: Visibility.Action
  )

reduce :: Store -> Action -> Store
reduce store = Variant.match
  { todos: \action -> store { todos = Todos.reduce store.todos action }
  , visibility: \action -> store { visibility = Visibility.reduce store.visibility action }
  }
