module ReduxTodo.Store.Todos
  ( Todo
  , Store
  , initialStore
  , Action
  , reduce
  , createTodo
  , toggleTodo
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.Variant (Variant)
import Data.Variant as Variant
import Type.Proxy (Proxy(..))

type Todo =
  { text :: String
  , completed :: Boolean
  , id :: Int
  }

type Store =
  { todos :: Array Todo
  , nextId :: Int
  }

initialStore :: Store
initialStore = { todos: [], nextId: 1 }

data Action
  = CreateTodo String
  | ToggleTodo Int

reduce :: Store -> Action -> Store
reduce store = case _ of
  CreateTodo text ->
    store
      { todos = Array.snoc store.todos { text, completed: false, id: store.nextId }
      , nextId = store.nextId + 1
      }

  ToggleTodo id -> do
    let
      toggleCompleted todo = todo { completed = not todo.completed }
      newTodos = fromMaybe store.todos do
        index <- Array.findIndex (eq id <<< _.id) store.todos
        Array.modifyAt index toggleCompleted store.todos

    store { todos = newTodos }

type Action' v = (todos :: Action | v)

injAction :: forall v. Action -> Variant (Action' v)
injAction = Variant.inj (Proxy :: Proxy "todos")

createTodo :: forall v. String -> Variant (Action' v)
createTodo = injAction <<< CreateTodo

toggleTodo :: forall v. Int -> Variant (Action' v)
toggleTodo = injAction <<< ToggleTodo
