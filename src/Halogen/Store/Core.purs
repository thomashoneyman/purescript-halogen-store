module Halogen.Store.Core where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Store.Select (Selector(..))
import Halogen.Subscription (Emitter, Listener, makeEmitter)
import Halogen.Subscription as HS
import Unsafe.Coerce (unsafeCoerce)

type HalogenStore a s =
  { value :: Ref s
  , emitter :: Emitter s
  , listener :: Listener s
  , reducer :: s -> a -> s
  }

newStore :: forall a s. s -> (s -> a -> s) -> Effect (HalogenStore a s)
newStore initialStore reducer = do
  value <- Ref.new initialStore
  { emitter, listener } <- HS.create
  pure { value, emitter, listener, reducer }

getStore :: forall a s. HalogenStore a s -> Effect s
getStore store = Ref.read store.value

updateStore :: forall a s. HalogenStore a s -> a -> Effect Unit
updateStore store action = do
  current <- Ref.read store.value
  let new = store.reducer current action
  Ref.write new store.value
  HS.notify store.listener new

emitSelected :: forall a s s'. HalogenStore a s -> Selector s s' -> Effect (Emitter s')
emitSelected store (Selector selector) = do
  init <- Ref.read store.value
  prevRef <- Ref.new (selector.select init)
  pure $ filterEmitter store.emitter \new -> do
    prevDerived <- Ref.read prevRef
    let newDerived = selector.select new
    if selector.eq prevDerived newDerived then
      pure Nothing
    else do
      Ref.write newDerived prevRef
      pure (Just newDerived)
  where
  filterEmitter :: forall x y. Emitter x -> (x -> Effect (Maybe y)) -> Emitter y
  filterEmitter emitter predicate = do
    let
      -- Behaves as `coerce`, but because `Emitter` doesn't export its
      -- constructor we have to use `unsafeCoerce`. This isn't as unsafe as it
      -- appears at first: if the definition of `Emitter` changes, then the call
      -- to `makeEmitter` above will fail to compile.
      coerceEmitter :: Emitter x -> ((x -> Effect Unit) -> Effect (Effect Unit))
      coerceEmitter = unsafeCoerce

    makeEmitter \k -> (coerceEmitter emitter) \a -> predicate a >>= traverse_ k
