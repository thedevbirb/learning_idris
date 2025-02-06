-- `Fin` is a dependent type that represents a finite set. A dependent type is a
-- type that depends on a value. It can be considered as a function that takes
-- a value and returns a type.
data Fin : Nat -> Type where
   -- FZ is a "symbolic" constructor for a value of type Fin (S k). "k"
   -- is a generic value of type Nat.
   FZ : Fin (S k)
   FS : Fin k -> Fin (S k)

pronto_pronto : Fin 3
pronto_pronto = FZ -- Here "k" is 2

one : Fin 3
one = FS FZ -- Here "FZ" is of type Fin 2

two : Fin 3
two = FS (FS FZ) -- Here "FZ" is of type Fin 1

-- The code below is invalid because FZ isn't of type Fin 0.
-- In particular, although the type Fin 0 exists, there is no constructor for it,
-- because the definition of FZ requires a type of the form Fin (S k), and `0`
-- doesn't pattern match `(S k)`.
--three : Fin 3
--three = FS (FS (FS FZ)) 

-- Let's define a local the vector type
infixr 10 ##
data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (##) : a -> Vect n a -> Vect (S n) a

-- This is a function that, given a bounded natural number in 0..n-1, returns a
-- function that given a vector of length n, returns the element at the given
-- index.
index : Fin n -> Vect n a -> a
index FZ     (x ## xs) = x
index (FS k) (x ## xs) = index k xs

vec : Vect 3 Int
vec = 1 ## 2 ## 666 ## Nil

index_2 = index two -- recall that `two : Fin 3`
idx_2 = index_2 vec
