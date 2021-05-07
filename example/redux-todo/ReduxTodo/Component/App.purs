module ReduxTodo.Component.App where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Monad (class MonadStore)
import ReduxTodo.Component.AddTodo (addTodo)
import ReduxTodo.Component.FilterLink (filterLink)
import ReduxTodo.Component.TodoList (todoList)
import ReduxTodo.Store as Store
import ReduxTodo.Store.Visibility (Visibility(..))

app
  :: forall q i o m
   . MonadStore Store.Action Store.Store m
  => H.Component q i o m
app = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }
  where
  render _ =
    HH.div_
      [ addTodo
      , todoList
      , HH.div_
          [ HH.span_ [ HH.text "Show: " ]
          , filterLink 0 All
          , filterLink 1 Active
          , filterLink 2 Completed
          ]
      ]
