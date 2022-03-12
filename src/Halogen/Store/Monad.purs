module Halogen.Store.Monad where

import Prelude

import Control.Monad.Cont (class MonadCont, ContT)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT)
import Control.Monad.Identity.Trans (IdentityT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT(..), ask, lift, local, mapReaderT, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, StateT)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer (class MonadTell, class MonadWriter, WriterT)
import Data.Distributive (class Distributive)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (HalogenM, hoist)
import Halogen as H
import Halogen.Hooks as Hooks
import Halogen.Store.Select (Selector(..))
import Halogen.Subscription (Emitter, Listener, makeEmitter, subscribe)
import Halogen.Subscription as HS

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

derive newtype instance Functor m => Functor (StoreT a s m)
derive newtype instance Apply m => Apply (StoreT a s m)
derive newtype instance Applicative m => Applicative (StoreT a s m)
derive newtype instance Bind m => Bind (StoreT a s m)
derive newtype instance Monad m => Monad (StoreT a s m)
derive newtype instance MonadEffect m => MonadEffect (StoreT a s m)
derive newtype instance MonadAff m => MonadAff (StoreT a s m)
derive newtype instance MonadThrow e m => MonadThrow e (StoreT a s m)
derive newtype instance MonadError e m => MonadError e (StoreT a s m)
derive newtype instance MonadTell w m => MonadTell w (StoreT a s m)
derive newtype instance MonadWriter w m => MonadWriter w (StoreT a s m)
derive newtype instance MonadState s m => MonadState s (StoreT a s m)
derive newtype instance MonadCont m => MonadCont (StoreT a s m)
derive newtype instance MonadRec m => MonadRec (StoreT a s m)
derive newtype instance Distributive g => Distributive (StoreT a s g)
derive newtype instance MonadTrans (StoreT a s)

instance MonadAsk r m => MonadAsk r (StoreT a s m) where
  ask = lift ask

instance MonadReader r m => MonadReader r (StoreT a s m) where
  local f (StoreT (ReaderT r)) = StoreT $ ReaderT $ local f <<< r

instance MonadEffect m => MonadStore a s (StoreT a s m) where
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
    -- For each new subscriber
    pure $ makeEmitter \subscriber -> do
      -- Read the current value from the store and project it
      init <- Ref.read store.value
      prevRef <- Ref.new (selector.select init)
      -- Return a subscription to the emitter's store such that for each new
      -- store value emitted...
      pure <<< HS.unsubscribe =<< subscribe store.emitter \new -> do
        -- we read the previous derived value,
        prevDerived <- Ref.read prevRef
        -- compute the new one,
        let newDerived = selector.select new
        -- and if they are equal,
        if selector.eq prevDerived newDerived then
          -- do nothing.
          pure unit
        else do
          -- otherwise, we update the ref for the next update
          liftEffect $ Ref.write newDerived prevRef
          -- and notify the subscriber.
          subscriber newDerived

instance monadStoreHalogenM :: MonadStore a s m => MonadStore a s (HalogenM st act slots out m) where
  getStore = lift getStore
  updateStore = lift <<< updateStore
  emitSelected = lift <<< emitSelected

instance monadStoreHookM :: MonadStore a s m => MonadStore a s (Hooks.HookM m) where
  getStore = lift getStore
  updateStore = lift <<< updateStore
  emitSelected = lift <<< emitSelected

instance MonadStore a s m => MonadStore a s (ContT r m) where
  getStore = lift getStore
  updateStore = lift <<< updateStore
  emitSelected = lift <<< emitSelected

instance MonadStore a s m => MonadStore a s (ExceptT e m) where
  getStore = lift getStore
  updateStore = lift <<< updateStore
  emitSelected = lift <<< emitSelected

instance MonadStore a s m => MonadStore a s (IdentityT m) where
  getStore = lift getStore
  updateStore = lift <<< updateStore
  emitSelected = lift <<< emitSelected

instance MonadStore a s m => MonadStore a s (MaybeT m) where
  getStore = lift getStore
  updateStore = lift <<< updateStore
  emitSelected = lift <<< emitSelected

instance (MonadStore a s m, Monoid w) => MonadStore a s (RWST r w s m) where
  getStore = lift getStore
  updateStore = lift <<< updateStore
  emitSelected = lift <<< emitSelected

instance MonadStore a s m => MonadStore a s (ReaderT r m) where
  getStore = lift getStore
  updateStore = lift <<< updateStore
  emitSelected = lift <<< emitSelected

instance MonadStore a s m => MonadStore a s (StateT s m) where
  getStore = lift getStore
  updateStore = lift <<< updateStore
  emitSelected = lift <<< emitSelected

instance (MonadStore a s m, Monoid w) => MonadStore a s (WriterT w m) where
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
