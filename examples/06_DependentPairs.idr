import Data.Vect

-- Dependent pairs allow the type of the second element of a pair
-- to depend on the value of the first element:
data MyDPair : (a : Type) -> (p : a -> Type) -> Type where
  -- `MkDPair` is the constructor. We can directly use `p x` and Idris will be
  -- able to match it since `p` is explicitly defined
  MyMkDPair : (x : a) -> p x -> MyDPair a p

-- The second argument is an anonymous function that takes a natural
-- number and returns a type.
my_vec : MyDPair Nat (\n => Vect n Int)
my_vec = MyMkDPair 2 [3, 4]

-- The built-in version of DPair with syntax sugar
-- Note that `(a, b)` means either `Pair`/`DPair` and `MkPair`/`MkDPair`
-- depending on the context.
-- Moreover `(x : a ** p)` is the type of a pair of A and P,
-- where the name `x` can occur inside p. `( x ** p )` constructs a value of this type.
vec : (n : Nat ** Vect n Int)
vec = (2 ** [3, 4])

vec_2 : (n : Nat ** Vect n Int)
-- The underscore is used in place of values which we expect the
-- type checker / compiler to fill in. For example we can omit
-- `2` since it can be inferred by the length of the vector.
vec_2 = (_ ** [3, 4])

-- We can do even more: since a `Vect` can be indexed only by a
-- natural number, there isn't even need to specify the type
-- of the first argument of the dependent pair!
vec_3 : (n ** Vect n Int)
vec_3 = (_ ** [3, 4])

-- From the crash course:
--
-- One use for dependent pairs is to return values of dependent types where the index
-- is not necessarily known in advance. For example, if we filter elements out of a
-- Vect according to some predicate, we will not know in advance what
-- the length of the resulting vector will be.

-- We first look at the simplified version with a List
filter_list : (t -> Bool) -> List t -> List t
filter_list p Nil = Nil
filter_list p (x :: xs) =
  if p x then (x :: filter_list p xs) 
  else (filter_list p xs)

list : List Nat
list = filter_list (\x => x > 1) (1 :: 2 :: Nil)

-- filter_list implementation calls two times `filter_list`.
-- Here we can use pattern matching to temporarily hold the result of `filter p xs`,
-- then modifying it depending of `p x`.
filter_list_2 : (t -> Bool) -> List t -> List t
filter_list_2 p Nil = Nil
filter_list_2 p (x :: xs) = case filter p xs of
  xs' => if p x then x :: xs' else xs

list_2 : List Nat
list_2 = filter_list_2 (\x => x > 1) (1 :: 2 :: Nil)

-- We now look at a filter implementation for a Vect type.
-- Here `filter_dep` takes a predicate function, a vector of length `n`
-- and returns a dependent pair which consists of a `m : Nat` and
-- a vector whose length depends on it.
filter_dep : (t -> Bool) -> Vect n t -> (m ** Vect m t)
-- If the vector is Nil, infer that the length is zero from the empty response.
filter_dep p Nil = (_ ** [])
-- We need a recursive definition where we call filter on the shorter vector.
-- Using pattern matching here allows us to temporarily hold the length of the filtered vector.
filter_dep p (x :: xs) = case filter p xs of
  -- In practice here we can always omit the length `m` of the vector and use `_` instead.
  -- That is because the compiler is able to infer by the usage of `::` whether the length
  -- must be increased or left unchanged.
  (m ** xs') => if p x then (S m ** x :: xs')
                       else (m ** xs')

vect_filtered : (m ** Vect m Nat)
vect_filtered = filter_dep (\x => x > 1) (1 :: 2 :: Nil)
