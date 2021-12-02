module Halogen.Store.Monad where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, lift, mapReaderT, runReaderT)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (HalogenM, hoist)
import Halogen as H
import Halogen.Hooks as Hooks
import Halogen.Store.Select (Selector(..))
import Halogen.Subscription (Emitter, Listener, makeEmitter)
import Halogen.Subscription as HS
import Unsafe.Coerce (unsafeCoerce)

-- | The `MonadStore` class captures monads which implement a stored value,
-- | along with methods to get, update (via an action type, `a`), or subscribe
-- | to changes in the stored value.
-- |
-- | An instance is provided for `StoreT`, which is the standard way to use
-- | the `MonadStore` class.
class MonadEffect m <= MonadStore a s m | m -> s a where
  getStore :: m s
  updateStore :: a -> m Unit
  emitSelected :: forall s'. Selector s s' -> m (Emitter s')

type HalogenStore a s =
  { value :: Ref s
  , emitter :: Emitter s
  , listener :: Listener s
  , reducer :: s -> a -> s
  }

-- | The `StoreT` monad transformer is the standard way to use the `MonadStore`
-- | class. It extends the base monad with a global action `a` used to update
-- | a global state `s`.
-- |
-- | The `MonadStore` type class describes the operations supported by this monad.
newtype StoreT :: Type -> Type -> (Type -> Type) -> Type -> Type
newtype StoreT a s m b = StoreT (ReaderT (HalogenStore a s) m b)

derive newtype instance functorStoreT :: Functor m => Functor (StoreT a s m)
derive newtype instance applyStoreT :: Apply m => Apply (StoreT a s m)
derive newtype instance applicativeStoreT :: Applicative m => Applicative (StoreT a s m)
derive newtype instance bindStoreT :: Bind m => Bind (StoreT a s m)
derive newtype instance monadStoreT :: Monad m => Monad (StoreT a s m)
derive newtype instance monadAskStoreT :: Monad m => MonadAsk (HalogenStore a s) (StoreT a s m)
derive newtype instance monadEffectStoreT :: MonadEffect m => MonadEffect (StoreT a s m)
derive newtype instance monadAffStoreT :: MonadAff m => MonadAff (StoreT a s m)
derive newtype instance monadThrowStoreT :: MonadThrow e m => MonadThrow e (StoreT a s m)
derive newtype instance monadErrorStoreT :: MonadError e m => MonadError e (StoreT a s m)

instance monadStoreStoreT :: MonadAff m => MonadStore a s (StoreT a s m) where
  getStore = StoreT do
    store <- ask
    liftEffect do
      Ref.read store.value

  updateStore action = StoreT do
    store <- ask
    liftEffect do
      current <- Ref.read store.value
      let newStore = store.reducer current action
      Ref.write newStore store.value
      HS.notify store.listener newStore

  emitSelected (Selector selector) = StoreT do
    store <- ask
    liftEffect do
      init <- Ref.read store.value
      prevRef <- Ref.new (selector.select init)
      pure $ filterEmitter store.emitter \new -> do
        prevDerived <- Ref.read prevRef
        let newDerived = selector.select new
        if selector.eq prevDerived newDerived then
          pure Nothing
        else do
          liftEffect $ Ref.write newDerived prevRef
          pure (Just newDerived)
    where
    filterEmitter :: forall x y. Emitter x -> (x -> Effect (Maybe y)) -> Emitter y
    filterEmitter emitter predicate = do
      let e = coerceEmitter emitter
      makeEmitter \k -> e \a -> predicate a >>= traverse_ k
      where
      -- Behaves as `coerce`, but because `Emitter` doesn't export its
      -- constructor we have to use `unsafeCoerce`. This isn't as unsafe as it
      -- appears at first: if the definition of `Emitter` changes, then the call
      -- to `makeEmitter` above will fail to compile.
      coerceEmitter :: Emitter x -> ((x -> Effect Unit) -> Effect (Effect Unit))
      coerceEmitter = unsafeCoerce

instance monadStoreHalogenM :: MonadStore a s m => MonadStore a s (HalogenM st act slots out m) where
  getStore = lift getStore
  updateStore = lift <<< updateStore
  emitSelected = lift <<< emitSelected

instance monadStoreHookM :: MonadStore a s m => MonadStore a s (Hooks.HookM m) where
  getStore = lift getStore
  updateStore = lift <<< updateStore
  emitSelected = lift <<< emitSelected

-- | Run a component in the `StoreT` monad.
-- |
-- | Requires an initial value for the store, `s`, and a reducer that updates
-- | the store in response to an action, `a`.
-- |
-- | This can be used directly on the root component of your application to
-- | produce a component that Halogen can run, so long as the base monad can
-- | be fixed to `Aff`.
-- |
-- | ```purs
-- | main = launchAff_ do
-- |   body <- Halogen.Aff.awaitBody
-- |   root <- runStoreT initialStore reducer rootComponent
-- |   runUI root unit body
-- | ```
runStoreT
  :: forall a s q i o m
   . Monad m
  => s
  -> (s -> a -> s)
  -> H.Component q i o (StoreT a s m)
  -> Aff (H.Component q i o m)
runStoreT initialStore reducer component = do
  hs <- liftEffect do
    value <- Ref.new initialStore
    { emitter, listener } <- HS.create
    pure { value, emitter, listener, reducer }
  pure $ hoist (\(StoreT m) -> runReaderT m hs) component

-- | Change the type of the result in a `StoreT` monad.
mapStoreT :: forall a s m1 m2 b c. (m1 b -> m2 c) -> StoreT a s m1 b -> StoreT a s m2 c
mapStoreT f (StoreT m) = StoreT (mapReaderT f m)
