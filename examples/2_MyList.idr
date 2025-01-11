-- We define a custom polymorphic list type
-- We define first a custom right-infix operator ## with precedence 10
-- Infix means that the operator can be placed between its arguments. Or can be
-- used like below as a function.
-- Right-infix means that the operator is right associative. That is, the
-- expression a ## b ## c is parsed as a ## (b ## c).
--
-- In the standard library, there is the `::` operator to create `List`s.
infixr 10 ##
data MyList a = Nil | (##) a (MyList a)

empty_list : MyList Int
empty_list = Nil

short_list : MyList Int
short_list = 1 ## 2 ## 3 ## Nil
