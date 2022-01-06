# Halogen Store

[![CI](https://github.com/thomashoneyman/purescript-halogen-store/workflows/CI/badge.svg?branch=main)](https://github.com/thomashoneyman/purescript-halogen-store/actions?query=workflow%3ACI+branch%3Amain)
[![Latest release](http://img.shields.io/github/release/thomashoneyman/purescript-halogen-store.svg)](https://github.com/thomashoneyman/purescript-halogen-store/releases)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-teal.svg)](http://github.com/thomashoneyman)

Global state management for Halogen.

## Installation

Install `halogen-store` with Spago:

```purs
spago install halogen-store
```

## Quick Start

This library provides global state management for Halogen applications. A global or central state can help when many components need access to the same information, and threading those values through components via their inputs is either tedious or leads to an explosion of unnecessary fields in state.

Writing applications with `halogen-store` comes down to three major steps, detailed in the next three sections:

1. [Creating the store](#creating-the-store)
2. [Using the store](#using-the-store)
3. [Running the application](#running-the-application)

### Setup

In order for the store to work a few data types and functions need to be defined.

First, we should create a central state for our application. This is called a "store" by convention.

```purs
module Basic.Store where

type Store = { count :: Int }

initialStore :: Store
initialStore = { count: 0 }
```

In the same module we'll create an action type that represents an update to our store. This action type is similar to the action type you define in each of your Halogen components.

```purs
data Action = Increment | Decrement
```

Finally, we should create a reducer: a function of type `store -> action -> store` that updates our central state when it receives an action. It's somewhat similar to the `handleAction` function you define in your Halogen components, but it can't perform effects.

```purs
reduce :: Store -> Action -> Store
reduce store = case _ of
  Increment -> store { count = store.count + 1 }
  Decrement -> store { count = store.count - 1 }
```

If you need to perform effects before or after updating the central state, then you can do that in the Halogen component which is performing the update.

As a brief aside: actions introduce some boilerplate to your application. If you want to stay bare-bones, then you can define your action type as a function `Store -> Store`, and then you can implement your reducer as function application:

```purs
type Action = Store -> Store

reduce :: Store -> Action -> Store
reduce store k = k store
```

This lets you write arbitrary `store -> store` functions and send them to your central state.

### Enabling the store for a halogen component

To use the store with a halogen component we need to do two things.
1. run the store monad using `runStoreT`, this will be discussed in the section "Running the application"
2. put the `MonadStore` constraint on our component

The `MonadStore` type class takes `Action`, `Store` and the underlying monad (such as `Aff`) as it's type parameters. Let's use the store and action types from the `Basic.Store` module which were previously defined.

```purs
import Basic.Store as BS
import Halogen.Store.Monad (class MonadStore)

component
  :: forall q i o m
   . MonadStore BS.Action BS.Store m
  => H.Component q i o m
```

### Letting a halogen component re-render on store changes

The `MonadStore` class provides three methods:

1. `getStore` retrieves the current value of the store.
2. `updateStore` applies an action to the store to produce a new store, using our reducer.
3. `emitSelected` produces an `Emitter` from the `halogen-subscriptions` library that will notify subscribers of the store's new value when it changes.

We can now use these methods anywhere we write `HalogenM` code -- for instance, in our `handleAction` function:

```purs
import Basic.Store as BS
import Halogen.Store.Monad (updateStore)

handleAction = case _ of
  Clicked ->
    -- This will increment our central store's count.
    updateStore BS.Increment
```

Remember from the [halogen guide](https://purescript-halogen.github.io/purescript-halogen/guide/05-Parent-Child-Components.html#input) we can use a component's input to automatically re-render the component. We could in theory change the input in a parent component after being notified by the store that there is a new value. However this library does not expose the store value outside of the `MonadStore` enabled code.

Instead the library provides a function `connect` which under the hood wraps the component in a another component that can access the store. This other component is transparent for the user of this library and provides automatic new input to the child component on store changes. Because of this automatic connection to input, the functions `emitSelected` and `getStore` are rarely used. You will be mostly working with `updateStore`.

The original input to your component is still available, but is wrapped into a record. 

```
{ context :: Maybe Action
, initialized :: Boolean
, input :: Input
}
```

Component input is handled by the `receive :: input -> Maybe action` function. In the example below the component will receive the store and original input on initialization and also when either the store or the original input changes.

```purs
import Basic.Store as BS
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Select (selectAll)

type Input = Unit

type State = { count :: Int }

deriveState :: Connected BS.Store Input -> State
deriveState { context, input } = { count: context.count }

data Action
  = Receive (Connected BS.Store Input)

component
  :: forall query input output m
   . MonadStore BS.Action BS.Store m
  => H.Component query input output m
component = connect selectAll $ H.mkComponent
  { initialState: deriveState
  , render: \{ count } -> ...
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  handleAction = case _ of
    Receive input ->
      H.put $ deriveState input
```

In the real world we can't afford to update every connected component any time the central state changes; this would be incredibly inefficient. Instead, we want to only updated connected components when the bit of state they are concerned with has changed.

We can use a `Selector` to retrieve part of our central state and only be notified when the state we've selected has changed. In the previous example we used `selectAll` to just grab the entire store, but usually we'd write our own selector.

Imagine that our store actually contained dozens of fields in addition to the `count` field we've implemented, but we only want to subscribe to that field. Let's do that by adjusting our component from the last section.

```purs
import Halogen.Store.Select (Selector, selectEq)

-- We are no longer connected to the entire store; we're only connected to
-- the `count` field, which is of type `Int` for our new context.
type Context = Int

deriveState :: Connected Context Input -> State
deriveState { context, input } = { count: context }

selectCount :: Selector BS.Store Context
selectCount = selectEq \store -> store.count

data Action
  = Receive (Connected Context Input)

component
  :: forall query input output m
   . MonadStore BS.Action BS.Store m
  => H.Component query input output m
component = connect selectCount $ H.mkComponent
  { initialState: deriveState
  , ...
  }
```

Now, even if other fields in our state are regularly changing, this component will only receive new input when the `count` field has changed.

### Running the application

When we run our application we'll need to satisfy our `MonadStore` constraints. Halogen components must always be run using the `Aff` monad, but our application needs to use a monad that supports `MonadStore`.

To solve this issue, we can use the `StoreT` transformer as the monad for our application, and then use `runStoreT` to transform it into `Aff`. (You're also welcome to define your own application monad, though I'd recommend defining it in terms of `StoreT`.)

We don't need to explicitly use `StoreT` in our component types; all we need to do is call `runStoreT` and supply an initial store, our reducer, and the component that requires the store. Let's see it in action:

```purs
module Main where

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
  runUI root unit body
```

### Using `halogen-store` with `halogen-hooks`

If you want to write your component with [Halogen Hooks](https://github.com/thomashoneyman/purescript-halogen-hooks) ,then you can use the `useSelector` hook to access the store.

```purs
module Main where

import Prelude

import Halogen.Hooks as Hooks
import Halogen.Store.Select (selectAll)
import Halogen.Store.UseSelector (useSelector)

component
  :: forall q i o m
   . MonadStore BS.Action BS.Store m
  => H.Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  context <- useSelector selectAll
  Hooks.pure do
    ...
```

Unlike `connect`, the context returned by `useSelector` has the type `Maybe store` because the hook does not have access to the store before it is initialized.
