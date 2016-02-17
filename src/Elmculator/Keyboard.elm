module Elmculator.Keyboard (input) where

import Elmculator.Actions exposing (Action)
import Keyboard
import Signal
import Char
import Elmculator.Operators exposing (plus, minus, divide, multiply, equals)

input: Signal Action
input =
  Signal.map keyPressToAction Keyboard.presses

keyPressToAction : Int -> Action
keyPressToAction keyCode =
  case (Char.fromCode keyCode) of
    '1' -> Elmculator.Actions.TypeNumber "1"
    '2' -> Elmculator.Actions.TypeNumber "2"
    '3' -> Elmculator.Actions.TypeNumber "3"
    '4' -> Elmculator.Actions.TypeNumber "4"
    '5' -> Elmculator.Actions.TypeNumber "5"
    '6' -> Elmculator.Actions.TypeNumber "6"
    '7' -> Elmculator.Actions.TypeNumber "7"
    '8' -> Elmculator.Actions.TypeNumber "8"
    '9' -> Elmculator.Actions.TypeNumber "9"
    '0' -> Elmculator.Actions.TypeNumber "0"
    '+' -> Elmculator.Actions.SelectOperator plus
    '-' -> Elmculator.Actions.SelectOperator minus
    '/' -> Elmculator.Actions.SelectOperator divide
    '*' -> Elmculator.Actions.SelectOperator multiply
    '=' -> Elmculator.Actions.SelectOperator equals
    'C' -> Elmculator.Actions.Clear
    'c' -> Elmculator.Actions.Clear
    _ -> Elmculator.Actions.Nop
