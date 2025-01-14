-- `Maybe` is like Rust's `Option<T>`.
data MyMaybe a = MyJust a | MyNothing
-- equivalently
-- data MyMaybe a where
--   Nothing : Maybe a
--   MyJust : a -> Maybe a

-- The `maybe` function type below is like Rust's `map_or`, which
-- provides a default value if `Nothing`, otherwise applies the function
-- if there is a value:
-- * `Maybe a` is the `Maybe` value we want to map
-- * `b` is the provided default value of type `b` if `Maybe a` is `Nothing`
-- * `(a -> b)` is the map to apply if `Maybe a` matches `Just a`.
my_maybe : b -> (a -> b) -> MyMaybe a -> b
my_maybe d f MyNothing = d
my_maybe d f (MyJust x) = f x

-- Example
just_x : MyMaybe Nat
just_x = MyJust 5

default_val : Int
default_val = 7

f : Nat -> Int
-- This is a built-in "cast" function
f x = cast x

-- Evalues to `True`
res = (my_maybe default_val f just_x) == 5
res_2 = (my_maybe default_val f MyNothing) == 7

