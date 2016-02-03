module Operators where

type alias Operator = Float -> Float -> Float
plus : Operator
plus a b =
  a + b

minus : Operator
minus a b =
  a - b

multiply : Operator
multiply a b =
  a * b

divide : Operator
divide a b =
  a / b

equals : Operator
equals a b = a
