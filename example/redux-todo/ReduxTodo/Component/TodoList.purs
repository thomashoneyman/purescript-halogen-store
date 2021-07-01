module ReduxTodo.Component.TodoList (todoList) where

import Prelude

import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML as HP
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (Selector, selectEq)
import ReduxTodo.Store as Store
import ReduxTodo.Store.Todos (Todo, toggleTodo)
import ReduxTodo.Store.Visibility (Visibility(..))
import Type.Proxy (Proxy(..))

type Slot id slots = ( todoList :: H.Slot (Const Void) Void id | slots )

todoList
  :: forall action slots m
   . MonadStore Store.Action Store.Store m
  => H.ComponentHTML action (Slot Unit slots) m
todoList = HH.slot_ (Proxy :: Proxy "todoList") unit component unit

type State = Array Todo

selectState :: Selector Store.Store (Array Todo)
selectState = selectEq \store -> case store.visibility.visibility of
  All ->
    store.todos.todos
  Completed ->
    Array.filter _.completed store.todos.todos
  Active ->
    Array.filter (not _.completed) store.todos.todos

deriveState :: Connected (Array Todo) Unit -> State
deriveState { context: todos } = todos

data Action
  = ToggleTodo Int
  | Receive (Connected (Array Todo) Unit)

component
  :: forall q o m
   . MonadStore Store.Action Store.Store m
  => H.Component q Unit o m
component = connect (\_ -> selectState) $ H.mkComponent
  { initialState: deriveState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  render todos = do
    let
      mkTodo todo = Tuple (show todo.id) do
        HH.li
          [ HE.onClick \_ -> ToggleTodo todo.id
          , HP.attr (HH.AttrName "style") do
              if todo.completed then
                "text-decoration: line-through;"
              else
                "text-decoration: none;"
          ]
          [ HH.text todo.text ]

    HK.ul_ (map mkTodo todos)

  handleAction = case _ of
    ToggleTodo id ->
      updateStore $ toggleTodo id
    Receive input ->
      H.put $ deriveState input
