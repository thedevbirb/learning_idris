-- We'll use this reference for the definition of a monad:
-- https://en.wikipedia.org/wiki/Monad_(category_theory)
-- That is, we consider a monad as a monoid in the category of endofunctor in
-- the category `Type` of types that classify runtime values.
--
-- See personal notes on the background to arrive to the definition of monad
-- starting from monoidal categories, reporting it here is a bit long.
--
-- A monad is then an endofunctor of `Type` that satifies some properties.
--
-- In its definition we see a natural transformation `\eta : I => T` which has
-- components the morphisms `\eta_a : I(a) -> T(a)` so on values it is
-- `\eta_a x -> T(a)(x)`. `I` corresponds to the `id` functor in Idris.
-- In essence, it embeds a value `x` of type `a` in the right context. 
-- (Keep `T = Maybe` as example).
-- We've already seen `\eta`. This is `pure` (Idris) or the `return` (Haskell)
-- function in the `Applicative` interface / class.
--
-- The `pure` function in `Applicative` is `\eta : I -> T` in the def of a
-- monad.
--
-- How to interpret the `\mu : T \circ T => T` natural transformation?
-- It seems to "flatten" nested context, and this is what's for. Consider
-- the `Maybe` functor: we would to like to flatten the type
-- `Maybe Maybe Int` into just `Maybe Int`.
-- This is done by the `join` function in Idris / Haskell.
-- 
-- How do monad laws help? Remeber that `T` can also be seen as a natural
-- transformation `\dot{T} : T => T \circ T` with components
-- `\dot{T}(a) : T(a) => T(T(a))`. In short, we can use the `Maybe`
-- functor itself to create a natural transformation `Maybe => `Maybe Maybe`.
-- Monad laws enforces that flattening works you'd expect:
-- * `join T pure a = id a = a`;
-- * `join pure T a = id a = a`;
-- * associativity holds.
--
-- The interface `Monad` is defined (almost) as follows in Idris
-- inteface Applicative m => Monad m where
--   >>= : m a -> (a -> m b) -> m b
--
-- Where `>>=` is also called "bind". Where is `join`, then?
-- If the type `a` is nested, meaning it is actually of the form
-- `m a`, then:
-- join : m (m a) -> m a
-- join m (m a) = m (m a) >>= id
--
-- Viceversa, we can define `bind` in terms of `join`.
-- (>>=) (m a) f = join map f (m a)
--
-- Here, given f : a -> m b, `join` flattens
-- `m (m b)` into `m b`.
--
-- The intuition behind bind is that it provides a mechanism for chaining computations in a context:
-- unwraps a value from its context;
-- applies a function to produce a new value, potentially transforming the context;
-- rewraps the result back into the context.

interface Applicative m => MyMonad m where
  (*>>=) : m a -> (a -> m b) -> m b
  my_join : m (m a) -> m a

  (*>>=) x f = my_join (map f x)
  my_join x = (*>>=) x id

MyMonad Maybe where
  (*>>=) Nothing f = Nothing
  (*>>=) (Just x) f = f x

-- Equivalently,
-- MyMonad Maybe where
--   my_join (Nothing _) = Nothing
--   my_join (Just Nothing) = Nothing
--   my_join (Just (Just x)) = Just x
--
-- Because
-- (*>>=) Nothing f = my_join (map f Nothing) -- equals my_join (Nothing Nothing) since f : a -> m b
-- etc...

MyMonad List where
  (*>>=) [] f = []
  (*>>=) (x :: xs) f = (f x) ++ ((*>>=) xs f)

-- Know we explore the equivalent of Rust's `Result`
data MyEither e t = MyLeft e | MyRight t

-- While Either is a bifunctor, if we fix a type variable
-- it becomes a functor e.g. Either String : Type -> Type
-- This is implementing the functor interface via pattern matching
-- to every type constructor like `Either e`
Functor (MyEither e) where
  map f (MyLeft x) = MyLeft x
  map f (MyRight t) = MyRight (f t)

Applicative (MyEither e) where
  pure t = MyRight t
  (<*>) _ (MyLeft e) = MyLeft e
  (<*>) f (MyRight t) =  (f MyRight t)

Monad (MyEither e) where
  (>>=) (MyLeft e) _ = MyLeft e
  (>>=) (MyRight t) f = f t
