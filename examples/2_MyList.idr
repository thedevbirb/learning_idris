-- We define a custom polymorphic list type
-- We define first a custom right-infix operator ## with precedence 10
-- Infix means that the operator can be placed between its arguments. Or can be
-- used like below as a function.
-- Right-infix means that the operator is right associative. That is, the
-- expression a ## b ## c is parsed as a ## (b ## c).
--
-- In the standard library, there is the `::` operator to create `List`s.
infixr 10 ##
-- Since we have a type parameter, we need to specify it in the definition
-- with `Type -> Type`, which makes us understand that `MyList` is a function
-- of `Type`s
data MyList : Type -> Type where
  Nil : MyList a
  (##) : a -> MyList a -> MyList a

-- equivalently
--data MyList a = Nil | (##) a (MyList a)

empty_list : MyList Int
empty_list = Nil

short_list : MyList Int
short_list = 1 ## 2 ## 3 ## Nil

-- We can also define concatenation between lists:
concat : List a -> List a -> List a
concat Nil ys = ys
concat (x :: xs) ys = x :: (concat xs ys)
