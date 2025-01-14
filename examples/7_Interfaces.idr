-- Interfaces are equivalent to haskell typeclasses
-- They are a way to define a set of functions that a type must implement
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
-- to type class dictionaries, which contains the methods that implement the
-- Show interface for that type.
interface MyShow a where
  my_show : a -> String

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
