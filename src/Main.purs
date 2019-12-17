module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Unsafe.Coerce (unsafeCoerce)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"

data IntString = I Int | S String

eqInt :: Int → Int → Boolean
eqInt a b = a === b

es :: String → String → Boolean
es a b = a === b

typeof = show

{--compare :: ∀ a. a → a → Boolean--}
{--compare a y =--}
  {--case [typeof a, typeof a] of--}
       {--["int"    , "int"]   → eqInt (unsafeCoerce a) (unsafeCoerce y)--}
       {--["string" , "string"] → es (unsafeCoerce a)  (unsafeCoerce y)--}
        {--_                    → true--}

infixr 5 eq as ===

class Random t where
  nothing :: t → String

class (Random t) <= Compare (t :: Type) where
  eq :: t → t → Boolean

ourCompare :: forall t. Compare t ⇒ t -> t -> Boolean
ourCompare = eq

instance randomStr :: Random String where
  nothing _ = "just giving constraint"

instance randomInt :: Random Int where
  nothing _ = "just giving constraint"

instance randomNum :: Random Number where
  nothing _ = "just giving constraint"

instance compareStr :: Compare String where
  eq = es

instance compareInt :: Compare Int where
  eq = eqInt

instance compareNum :: Compare Number where
  eq = (==)

map :: ∀ a b f. Functor f => (a → b) → f a → f b
map = (<$>)

mapForProtected
  :: forall a b
  . (a → b)
  → Protected a
  → Protected b
mapForProtected = map

data Protected a
  = Unsafe
  | Safe a

instance functorProtected :: Functor Protected where
  map f a = unsafeCoerce ""

a :: Boolean
a = ourCompare 1.0 1.0

b :: Array String
b = show    <$> [1,2,3]

shown :: String
shown = show 1.0

c :: Array String
c = [1,2,3] <#> show
{--
composeflip :: (a → b)   → (b → c)   → a   → c

pure        :: a         → f a
apply       :: f (a → b) → f a       → f b
bind        :: (a → m b) → m a       → m b
map         :: (a → b)   → f a       → f b
append      :: f a       → f a       → f a

if we have these 5 functions what is that?
                       MONAD
                       --}














