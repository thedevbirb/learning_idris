-- Equalities can be expressed with a type. In particular,
-- Idris2 exposes an `Equal` data type, which can be created using
-- the `=` syntax that takes two type parameters and returns a type.
-- Remember that Idris supports dependent types, so `a` and `b` can be
-- values of a certain type.
data MyEqual : a -> b -> Type where
  -- We just impose reflexivity here. The type of `x` is inferred
  -- by the type definition itself.
  MyRefl : MyEqual x x

-- Equality is transitive. `Refl` works because all symbols are actually the
-- same.
my_trans : x = y -> y = z -> x = z
my_trans Refl Refl = Refl

-- Equality is symmetric. If `x = y` then `y = x`.
my_sym : x = y -> y = x
my_sym Refl = Refl

-- We can also express that if you have a function and two equal
-- arguments, that the result is equal.
my_cong : (f : a -> b) -> x = y -> f x = f y
my_cong f Refl = Refl

-- Recall the definition of `plus` of natural numbers
my_plus : Nat -> Nat -> Nat
my_plus Z m = m
my_plus (S n) m = S (my_plus n m)

-- We now want to prove that the sum is commutative.
-- In mathematics, we would say: "for all n,m natural numbers, n + m = m + n".
-- This is indeed a function that takes two natural numbers and returns an `Equal`ity.
--
-- We never shown that `Z` is right identity for `plus`.
-- plus_commutes : (n, m : Nat) -> Equal (my_plus n m) (my_plus m n)
-- plus_commutes Z m = ?plus_commutes_Z
-- plus_commutes (S n) m = ?plus_commutes_S

-- Here we show that `Z` is the right identity for `plus`. That is,
-- we need to show that `plus n Z = n` for each natural.
plus_commutes_Z : (n : Nat) -> n = plus n Z
-- Base case. Here, `n = Z` so `Z = plus Z Z` and `Refl` is appropriate.
plus_commutes_Z Z = Refl
-- We need to return `S k = plus (S k) Z`. By definition of `plus`,
-- that is `S k = S (plus k Z)`. To return that congruence we can use
-- `cong` with `S`. We can use our assumption as a proof that `k = plus k Z`.
--
-- plus_commutes_Z (S k) = cong S (plus_commutes_Z k)
--
-- Another way is the following: `(S k) = plus (S k) Z` reduces to
-- `S k = S (plus k Z)`. Our induction hypothesis tells us that
-- `k = plus k Z`, so `sym (plus_commutes_Z k)` returns `plus k Z = k`.
--
-- `rewrite` allows us to replace equal terms in a proof, in our case
-- `(S k) = S (plus k Z)`. By providing an equality `plus k Z = k`,
-- we know have to prove `S k = S k`, which can be done by `Refl`.
-- NOTE: the syntax isn't super intuitive yet.
plus_commutes_Z (S k)
   = rewrite sym (plus_commutes_Z k) in Refl

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
