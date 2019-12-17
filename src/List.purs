module List where

import Data.Monoid
import Prelude

data List a = Nil | Cons a (List a)

con :: Array (Array (Array Int)) → Array Int
con = (_ >>= (_ >>= id))

infixr 5 Cons as :

instance showList :: Show a ⇒ Show (List a) where
  show xs = "(" <> inter ":" (show <$> xs) <> ")"
  {--show = foldRight (\a → (<>) $ show a <> ":") "Nil"--}

inter :: String → List String → String
inter sep = foldRight ((<>) <<< (_ <> sep)) "Nil"

foldRight :: ∀ a b. (a → b → b) → b → List a → b
foldRight _ b Nil = b
{--foldRight f b (Cons h t) = f h $ foldRight f b t--}
foldRight f b (h : t) = f h $ foldRight f b t

foldLeft :: ∀ a b. (b → a → b) → b → List a → b
foldLeft _ b Nil = b
foldLeft f b (h : t) = foldLeft f (f b h) t

headOr :: ∀ a. a → List a → a
headOr df Nil = df
headOr _ (h : _) = h

list = 1 : 2 : 3 : 4 : Nil

product :: List Int -> Int
product = foldLeft (*) 1

sum :: List Int → Int
sum = foldLeft (+) 0

length :: ∀ a. List a → Int
length = foldRight ((+) <<< const 1) 0

map :: ∀ a b. (a → b) → List a → List b
map f = foldRight ((:) <<< f) Nil
{--map = flip foldRight Nil <<< ((<<<) (:))--}

filter ::
  ∀ a. (a -> Boolean)
    → List a
    → List a
filter f = foldRight predicate Nil
  where predicate = liftA2 (bool id) (:) f

append ::
  ∀ a.
    List a
    → List a
    → List a
append a b = foldRight Cons b a

infixr 5 append as ++

flatten ::
  ∀ a.
    List (List a)
    → List a
flatten Nil = Nil
flatten (head:tail) = head ++ flatten tail

flatMap ::
  ∀ a b.
    (a → List b)
    → List a
    → List b
flatMap f = foldRight ((++) <<< f) Nil
{--flatMap = flip foldRight Nil <<< ((<<<) (++))--}

flattenAgain ::
  ∀ a.
    List (List a)
    → List a
flattenAgain = flatMap id

reverse :: List ~> List
reverse = foldLeft (flip Cons) Nil

--- infinity list fn dont use
produce ::
  ∀ a.
    (a → a)
    → a
    → List a
produce f x = x : produce f (f x)

c :: List (List (List Int)) → List Int
c = (_ >>= (_ >>= id))

instance functorList :: Functor List where
  map = map

instance semigroupList :: Semigroup (List a) where
  append xs ys = foldRight (:) ys xs

instance monoidList :: Monoid (List a) where
  mempty = Nil

instance applyList :: Apply List where
  apply lf xs = foldRight (append <<< (_ <$> xs)) mempty lf

instance bindList :: Bind List where
  bind list f = foldRight (append <<< f) mempty list

instance applicativeList :: Applicative List where
  pure = (_ : Nil)

instance monadList :: Monad List

doblock = do
  a ← list
  b ← list
  pure $ (a * b) : (a+b) : Nil


liftA2 h g f x = h (g x) (f x)

bool f _ false = f
bool _ t true  = t

headOrEq = headOr 4 list
