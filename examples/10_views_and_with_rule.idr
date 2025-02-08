bool_to_nat : Bool -> Nat
bool_to_nat True = S Z
bool_to_nat False = Z

-- The `with` rule allows to match on intermediate values. This
-- would "extend" the function definition and allow us to match
-- on the intermediate values during its definition.
--
-- In this example, we calculate (a && b). Then, in the definition
-- of `foo` we have two different behaviour of `foo a b` depending
-- on the result of `a && b`.
foo : Bool -> Bool -> Nat
foo a b with (a && b)
  foo a b | True = bool_to_nat a * bool_to_nat b
  -- We don't care about `a` and `b`, if `a && b == False` we just return `0`.
  _ | False = 0

-- This is an equivalent way to write is using `case` and `of`.
-- Depending on the use cases, one way can be more succint than the other.
bar : Bool -> Bool -> Nat
bar a b = case (a && b) of
  True => bool_to_nat a * bool_to_nat b
  False => 0

-- More `with` rule can be applied at the same time, like below
baz : Int -> Int -> Bool
baz n m with (n + 1) | (m + 1)
  _ | 2 | 3 = True
  _ | _ | _ = False

-- VIEWS
-- A view in Idris2 is a dependent type pattern that provides an alternative
-- structured representation of a value to facilitate pattern matching and
-- structured recursion.
-- 
-- A view consists of:
--  1.  A view type: A type that represents an alternative way to interpret
--      values of another type.
--  2.  A view function: A total function that maps values of the original type
--      into the view type, ensuring every case is covered.
-- 
-- This technique allows for case analysis and structured recursion without
-- relying on direct pattern matching on complex types.
--
-- More formally, a view is a dependent pair (T, V(T)) with a view function
-- v : T -> V(T).
--
-- Let's see an example with the natural numbers.

-- Every natural number is either even, and can be written by summing
-- a number twice; or is odd, and can be written as the successor of summing
-- the same number twice.
data Parity : Nat -> Type where
  -- NOTE: Idris does some internal symbolic manipulation here to infer `n`,
  -- by diving by 2.
  Even {n : _} = Parity (n + n)
  Odd {n : _} = Parity S (n + n)

-- Here `n == 2`.
even : Parity 4

-- Then, we create the view function `parity`.
-- Note that the type `(_ ** Parity _) would have been also an appropriate choice
--
-- The implementation of `parity` requires some concepts of "Theorem proving", that we'll
-- see in the next section.
parity : (n : Nat) -> Parity n
