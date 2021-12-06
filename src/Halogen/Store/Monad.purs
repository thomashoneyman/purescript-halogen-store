module Halogen.Store.Monad where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, lift, mapReaderT, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (HalogenM, hoist)
import Halogen as H
import Halogen.Hooks as Hooks
import Halogen.Store.Core (HalogenStore)
import Halogen.Store.Core as Core
import Halogen.Store.Select (Selector)
import Halogen.Subscription (Emitter)

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

-- | The `StoreT` monad transformer is the standard way to use the `MonadStore`
-- | class. It extends the base monad with a global action `a` used to update
-- | a global state `s`.
-- |
-- | The `MonadStore` type class describes the operations supported by this monad.
newtype StoreT :: Type -> Type -> (Type -> Type) -> Type -> Type
newtype StoreT a s m b = StoreT (ReaderT (HalogenStore a s) m b)

derive newtype instance Functor m => Functor (StoreT a s m)
derive newtype instance Apply m => Apply (StoreT a s m)
derive newtype instance Applicative m => Applicative (StoreT a s m)
derive newtype instance Bind m => Bind (StoreT a s m)
derive newtype instance Monad m => Monad (StoreT a s m)
derive newtype instance MonadEffect m => MonadEffect (StoreT a s m)
derive newtype instance MonadAff m => MonadAff (StoreT a s m)
derive newtype instance MonadThrow e m => MonadThrow e (StoreT a s m)
derive newtype instance MonadError e m => MonadError e (StoreT a s m)

instance MonadEffect m => MonadStore a s (StoreT a s m) where
  getStore = StoreT do
    store <- ask
    liftEffect $ Core.getStore store

  updateStore action = StoreT do
    store <- ask
    liftEffect $ Core.updateStore store action

  emitSelected selector = StoreT do
    store <- ask
    liftEffect $ Core.emitSelected store selector

instance MonadEffect m => MonadAsk s (StoreT a s m) where
  ask = getStore

instance MonadStore a s m => MonadStore a s (HalogenM st act slots out m) where
  getStore = lift getStore
  updateStore = lift <<< updateStore
  emitSelected = lift <<< emitSelected

instance MonadStore a s m => MonadStore a s (Hooks.HookM m) where
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
  hs <- liftEffect $ Core.newStore initialStore reducer
  pure $ hoist (\(StoreT m) -> runReaderT m hs) component

-- | Change the type of the result in a `StoreT` monad.
mapStoreT :: forall a s m1 m2 b c. (m1 b -> m2 c) -> StoreT a s m1 b -> StoreT a s m2 c
mapStoreT f (StoreT m) = StoreT (mapReaderT f m)
