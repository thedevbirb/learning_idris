-- Defines a simple type for natural numbers and addition on them
-- In the most abstract sense, the collection of natural numbers consists of an
-- abstract symbol `Z` and a successor function `S` that takes a natural number
-- and returns another natural number.
--
-- The definition of `S` here is inferred from the definition of `MyNat` below.
-- Takes a `MyNat` and _must_ return a `MyNat` since that function is a constructor for `MyNat`.
-- Note that MyS is more "symbolic", meaning that `MyS MyNat` isn't a concrete
-- value, but a symbolic representation of a natural number.
--
-- "|" represents an alternative constructor for a type.
data MyNat = Z | MyS MyNat

-- We explicity say that the variable `zero` represents `Z`.
zero : MyNat
zero = Z

-- We explicity say that the variable `one` represents `S Z`.
one : MyNat
one = MyS Z

two : MyNat
two = MyS (MyS Z)

add : MyNat -> MyNat -> MyNat
-- Base case
add Z y = y
-- Recursive case
add (MyS x) y = MyS (add x y)

-- The way `MyNat` is defined is by specifying immediately the constructors for
-- it. However we can also define `MyNat2`, by saying that it is a type
-- (without parameters) with `data MyNat2 : Type` and then defining the
-- constructors for it using the `where` keyword.
data MyNat2 : Type where
  Z2 : MyNat2
  MyS2 : MyNat2 -> MyNat2
