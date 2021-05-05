module Halogen.Store.Connect
  ( Connected
  , connect
  , connectWith
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Monad (class MonadStore, emitSelected, getStore)
import Halogen.Store.Select (Selector(..), selectAll)
import Type.Proxy (Proxy(..))
import Unsafe.Reference (unsafeRefEq)

type Connected context input =
  { context :: context
  , input :: input
  }

data Action context input output
  = Initialize
  | Receive input
  | Update context
  | Raise output

connect
  :: forall action store query input output m
   . MonadEffect m
  => MonadStore action store m
  => H.Component query (Connected store input) output m
  -> H.Component query input output m
connect = connectWith selectAll

connectWith
  :: forall action store context query input output m
   . MonadEffect m
  => MonadStore action store m
  => Selector store context
  -> H.Component query (Connected context input) output m
  -> H.Component query input output m
connectWith (Selector selector) component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval
        { handleAction
        , handleQuery: H.query (Proxy :: Proxy "inner") unit
        , initialize: Just Initialize
        , finalize: Nothing
        , receive: Just <<< Receive
        }
    }
  where
  initialState input =
    { context: Nothing
    , initialized: false
    , input
    }

  render state = case state.context of
    Just context ->
      -- This should be using `HH.lazy2`, but that's prevented by a bug:
      -- https://github.com/purescript-halogen/purescript-halogen/issues/748
      --
      -- In the meantime, the equality checks have moved to the `Receive` and
      -- `Update` constructors.
      renderInner state.input context
    _ ->
      HH.text ""

  renderInner input context =
    HH.slot (Proxy :: Proxy "inner") unit component { input, context } Raise

  handleAction = case _ of
    Initialize -> do
      emitter <- emitSelected (Selector selector)
      void $ H.subscribe $ map Update emitter
      context <- map selector.select getStore
      H.modify_ _ { context = Just context }

    Receive newInput -> do
      oldInput <- H.gets _.input
      unless (unsafeRefEq oldInput newInput) do
        H.modify_ _ { input = newInput }

    Update newContext ->
      H.gets _.context >>= case _ of
        Just oldContext | unsafeRefEq oldContext newContext -> pure unit
        _ -> H.modify_ _ { context = Just newContext}

    Raise output ->
      H.raise output
