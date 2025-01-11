-- This type represents a dependent types system `MyLangTypes` for a language
-- `MyLang`, that can be manipulated inside Idris2 itself.
-- First, we define a type `MyLangTypes` that represents the types of the
-- language. `MyLangTypes` is an Idris datatype, used to represent the types of
-- MyLang.”
-- We don't specify the constructors for it yet.
data MyLangTypes : Type
-- This is a function that takes a `MyLangTypes` and returns a corresponding
-- Idris `Type`. It "Translates" the `MyLangTypes` to Idris types.
Translate : MyLangTypes -> Type

-- We know define the constructors for `MyLangTypes`.
data MyLangTypes : Type where
  -- The symbol `N` is a type in `MyLangTypes`.
  N : MyLangTypes
  -- The symbol `I` is a type in `MyLangTypes`.
  I : MyLangTypes
  -- `Pi` is a function that constructs a dependent function type.
  -- We start with a base MyLangTypes `a`, then since we need to create them
  -- in Idris we need a function that takes its translation and outputs another
  -- type in `MyLangTypes`.
  -- These two arguments allow us to create a dependent type in `MyLangTypes`.
  Pi : (a : MyLangTypes) -> (b : Translate a -> MyLangTypes) -> MyLangTypes

-- We now define the action of `Translate` on `MyLangTypes` to Idris types.
Translate N = Nat
Translate I = Int
-- If we have dependent type of the form `Pi a b`, then it is translated
-- to a dependent type indexed by `Translate a` and we translate
-- also the result of applying `b`.
Translate (Pi a b) = (x : Translate a) -> Translate (b x)

-- EXAMPLES

-- First, we need helpers "even" and "odd" functions for our examples The
-- "mutual" blocks help us consider different definitions that depend on each
-- other.
mutual
  even : Nat -> Bool
  even Z = True
  even (S k) = not (odd k)

  odd : Nat -> Bool
  odd Z = False
  odd (S k) = not (even k)

-- We now define `f` that returns two distinct types in `MyLangTypes`
-- depending on the input
f : Nat -> MyLangTypes
f x = if even x then N else I

-- We now create a dependent type `example` in `MyLang`
example : MyLangTypes
example = Pi N f

-- `example` is a dependent type in `MyLangTypes` of the form `Pi a b`.
-- If we translate it we get a `Type` in Idris, which is either a `Nat` 
-- or a `Int`
result : Type
result = Translate (Pi N f)
