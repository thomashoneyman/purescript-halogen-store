module Halogen.Store.Effect
  ( STORE
  , Store
  , _store
  , emitSelected
  , getStore
  , updateStore
  , runStore
  ) where

import Prelude

import Halogen.Store.Core as Core
import Halogen.Store.Select (Selector)
import Halogen.Subscription (Emitter)
import Run (EFFECT, Run)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

data Store' a s s' b
  = GetStore (s -> b)
  | UpdateStore a b
  | EmitSelected (Selector s s') (Emitter s' -> b)

derive instance Functor (Store' a s s')

data Store :: Type -> Type -> Type -> Type
data Store a s b

instance Functor (Store a s) where
  map f = unStore (mkStore <<< map f)

mkStore :: forall a s s' b. Store' a s s' b -> Store a s b
mkStore = unsafeCoerce

unStore :: forall a s b r. (forall s'. Store' a s s' b -> r) -> Store a s b -> r
unStore = unsafeCoerce

type STORE a s r = (store :: Store a s | r)

_store :: Proxy "store"
_store = Proxy

getStore :: forall a s r. Run (STORE a s + r) s
getStore = Run.lift _store (mkStore (GetStore identity))

updateStore :: forall a s r. a -> Run (STORE a s + r) Unit
updateStore action = Run.lift _store (mkStore (UpdateStore action unit))

emitSelected :: forall a s s' r. Selector s s' -> Run (STORE a s + r) (Emitter s')
emitSelected selector = Run.lift _store (mkStore (EmitSelected selector identity))

runStore
  :: forall a s b r
   . Core.HalogenStore a s
  -> Store a s b
  -> Run (EFFECT + r) b
runStore store = unStore case _ of
  GetStore reply -> do
    value <- Run.liftEffect $ Core.getStore store
    pure $ reply value

  UpdateStore action next -> do
    Run.liftEffect $ Core.updateStore store action
    pure next

  EmitSelected selector reply -> do
    emitter <- Run.liftEffect $ Core.emitSelected store selector
    pure $ reply emitter
