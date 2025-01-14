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
--
-- NOTE: feels too little explicit as of now
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
