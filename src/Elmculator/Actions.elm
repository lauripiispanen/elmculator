module Elmculator.Actions where

import Elmculator.Operators exposing (Operator)

type Action =
  Nop |
  TypeNumber String |
  SelectOperator Operator |
  Clear
