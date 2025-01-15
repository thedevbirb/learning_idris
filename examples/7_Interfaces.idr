-- Interfaces are equivalent to haskell typeclasses.
-- An interface takes a type parameter and defines a set of functions
-- that use such type parameter. It describes a contract or capability that a
-- type can fulfill by providing implementations for its specified functions.
-- Example (haskell):
-- class Eq a where
-- (==) :: a -> a -> Bool
-- (/=) :: a -> a -> Bool
--
-- Example (idris):
-- interface Eq a where
-- (==) : a -> a -> Bool
-- (/=) : a -> a -> Bool

-- Note MyShow : Type -> Type, because an interface is a function from a type
-- to type class dictionaries, which contains the functions that implement the
-- Show interface for that type.
-- By default, a parameter in an interface has type `Type`, but as we see
-- in the next example we can specify other types as well, 
-- like `(f : Type -> Type)`.
interface MyShow a where
  my_show : a -> String

interface MyJson a where
  parse : String -> Maybe a

-- By default parameters that are not explicitly ascribed a type in an
-- interface declaration (like `a`, above) are assigned the quantity 0. This
-- means that the parameter is not available to use at runtime in the methods’
-- definitions. This means the parameter is "erased" at runtime. It is a
-- compile-time construct to determine the right function to call when
-- compiling the code.

-- For instance, MyShow a gives rise to a 0-quantified type variable a in the
-- type of the show method:
--
-- Main> :set showimplicits
-- Main> :t my_show
-- MyShow : {0 a : Type} -> Show a => a -> String
--
-- We need :set showimplicits to see the implicit arguments in the type
-- which are normally inferred by the compiler and hidden from the user.

-- This is how you define instances of an interface.
-- Compared to Haskell, you don't need the `instance` keyword at the beginning.
MyShow Int where
  my_show a = show a
