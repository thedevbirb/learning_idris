-- Equalities can be expressed with a type. In particular,
-- Idris2 exposes an `Equal` data type, which can be created using
-- the `=` syntax that takes two type parameters and returns a type.
-- Remember that Idris supports dependent types, so `a` and `b` can be
-- values of a certain type.
data MyEqual : a -> b -> Type where
  -- We just impose reflexivity here. The type of `x` is inferred
  -- by the type definition itself.
  MyRefl : MyEqual x x

-- Equality is transitive. `MyRefl` works because all symbols are actually the
-- same.
my_trans : MyEqual x y -> MyEqual y z -> MyEqual x z
my_trans MyRefl MyRefl = MyRefl

-- We can also express that if you have a function and two equal
-- arguments, that the result is equal.
my_cong : (f : a -> b) -> MyEqual x y -> MyEqual (f x) (f y)
my_cong f MyRefl = MyRefl

-- Recall the definition of `plus` of natural numbers
my_plus : Nat -> Nat -> Nat
my_plus Z m = m
my_plus (S n) m = S (my_plus n m)

-- We now want to prove that the sum is commutative.
-- In mathematics, we would say: "for all n,m natural numbers, n + m = m + n".
-- This is indeed a function that takes two natural numbers and returns an `Equal`ity.
--
-- We never shown that `Z` is right identity for `plus`.
-- plus_is_commutative : (n, m : Nat) -> MyEqual (my_plus n m) (my_plus m n)
-- plus_is_commutative Z m = MyRefl
-- plus_is_commutative (S n) m = ?todo

-- Here we show that `Z` is the right identity for `plus`. That is,
-- we need to show that `plus n Z = n` for each natural.
plus_right_identity : (n : Nat) -> MyEqual (my_plus n Z) n
-- Base case. Here, `n = Z` so `my_plus Z Z = Z` and `MyRefl` is appropriate.
plus_right_identity Z = MyRefl
-- We need to return `my_plus (S k) Z = S k`. By definition of `plus`,
-- that is `S (my_plus k Z) = S k`. To return that congruence we can use
-- `my_cong` with `S`. We can use our assumption as a proof that `my_plus k Z = k`.
plus_right_identity (S k) = my_cong S (plus_right_identity k)
