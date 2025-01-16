-- In Idris2, `Type` is the kind that represents all concrete types that can
-- classify values at runtime. These include types like Int, String, Bool, or
-- user-defined data types, which define the shape and behavior of runtime
-- data. Note that is also contain functions between these types.
--
-- A function from `Type` to `Type` is a type constructor, and it doesn't
-- belong to `Type`. It belongs to `Type -> Type` (an "higher kind"), which is
-- the kind of all type constructors.
--
-- `Type` is category, exactly following the definition of category in Category
-- Theory. More precisely, `Type` is a category whose objects are types like
-- `Int`, `String`, `Bool`, and all possible user-defined data types _which
-- classify runtime values_, and whose morphisms are functions between these
-- types, denoted by `->`.
-- The axioms of a category are satisfied because the language allows function
-- composition which is associative, and the identity function is defined for every type.
--
-- Like in Haskell, in Idris2 there are functors, and they are indeed functors
-- in the category of types. How are they defined? Let's try to build it together.
--
-- Let's recall the definition of a functor `F` from a category `C` to a category `D`.
-- It consists of the following data:
-- 1. for every `c` in `C`, an object `F c` in `D`,
-- 2. for every morphism `f : c -> c'` in `C`, a morphism
--   `F f : F c -> F c'` in `D`.
-- These data must satisfy the following laws:
-- * for every object `c` in `C`, `F id_c = id_{F c}`,
-- * for every morphisms `f : c -> c'` and `g : c' -> c''` in `C`,
--   `F (g . f) = F g . F f`.
--
-- We want to mainly in the category of `Type`, so we will now define
-- how endofunctors in `Type` could look like in Idris. Intuitively,
-- it should look like an interface or trait that functions
-- `Type -> Type` should adhere to.
-- Such functions already map objects of `Type` to objects of `Type`,
-- so we need to specify the action on morphisms, that is on
-- `map : a -> b`
interface MyEndoFunctor (f : Type -> Type) where
  my_map : (a -> b) -> f a -> f b

-- Example: List is a functor!
MyEndoFunctor List where
  my_map f [] = []
  my_map f (x::xs) = f x :: my_map f xs

-- Example: Maybe is a functor!
MyEndoFunctor Maybe where
  my_map f Nothing = Nothing
  my_map f (Just x) = Just (f x)

-- We should make sure that identities are preserved and functoriality works at the type level,
-- but this is a little bit advanced for now, we will encounter it later.

-- We can do a little bit more abstractions. Consider the following definitions below:
add_one : Int -> Int
add_one x = x + 1

-- We have to manually add `Just`.
x1 : Maybe Int
x1 = Just 5

y1 : Maybe Int
-- We have to manually invoke `map` from the `Functor` interface.
y1 = map add_one x1

-- The two operations above are a little redundant. In fact, the language
-- defines the interface "Applicative" that abstracts function application,
-- by "lifting" values in the right context
infixl 2 <**> -- should be `<*>` in `Applicative`

interface MyEndoFunctor f => MyApplicative f where
  -- `a` is a type parameter. Lifts the _value_ of type "a" in the right context. 
  -- Note that "a" can be a function / morphism
  my_pure : a -> f a
  -- take a lifted morphism, a lifted value and apply
  (<**>) : f (a -> b) -> f a -> f b

MyApplicative Maybe where
  my_pure a = Just a
  (<**>) Nothing _ = Nothing
  (<**>) (Just f) Nothing = Nothing
  (<**>) (Just f) (Just a) = Just (f a)

x2 : Maybe Int
x2 = my_pure 5 -- Just 5

add_two : Maybe (Int -> Int)
add_two = my_pure (\x => x + 2)

y2 : Maybe Int
y2 = add_two <**> x2 -- Just 7

-- A combination of the previous expressions:
res : Maybe Int
res = my_pure (+2) <**> my_pure 6 -- Just 8
