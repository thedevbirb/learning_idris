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

-- What we've done above is a proof by induction of a proposition about natural
-- numbers. Each of these proofs look the same: it takes the form of a base
-- case and an inductive step. In the base case, we show that the proposition
-- holds for `Z`. In the inductive step, we assume the proposition holds for `k>=0`,
-- and if we can use it to prove that it holds for `k+1`, then we've proven it
-- for all natural numbers.
-- Idris type system is flexible enough that we can precisily encode inductive
-- definition and proofs:
nat_induction :
  (prop : Nat -> Type) ->                 -- Property to show
  (prop Z) ->                             -- Base case
  ((k : Nat) -> prop k -> prop (S k)) ->  -- Inductive step
  (x : Nat) ->                            -- The property for any `x`.
  (prop x)
-- The base case is with `x = Z` is `p_Z` itself.
nat_induction prop p_Z p_S Z = p_Z
-- If `x = S k`, then to return `prop (S k)` we can use `p_S`.
nat_induction prop p_Z p_S (S k) = p_S k (nat_induction prop p_Z p_S k) 

-- For example, let's redefine "plus" using this framework by induction on `n`. 
-- Note that in the definition above, if the return type is `Equal a b` 
-- then it's a proof, otherwise a definition.
--
-- The definition here says: "for each `x`, `plus x m` returns a `Nat`.
plus_ind : Nat -> Nat -> Nat
plus_ind n m = nat_induction 
   (\x => Nat)
   m                      -- Base case, plus_ind Z m
   (\k, k_rec => S k_rec) -- Inductive step: in each step we call the successor on the previous case until we are
                          -- in the base case with `m`. The sum `n + m` is calling `n` times the function `S`
                          -- on `m`.
   n                      -- How many times we should repeat.
