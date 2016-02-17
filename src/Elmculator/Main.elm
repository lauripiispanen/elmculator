module Elmculator where

import Html exposing (div, button, text, h1, h2)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import StartApp exposing (start)
import Effects exposing (none)
import String exposing (toFloat)
import Elmculator.Keyboard exposing (input)
import Elmculator.Actions exposing (Action)
import Elmculator.Operators exposing (..)

app =
  StartApp.start { init = (emptyModel, Effects.none), view = view, update = update, inputs = [ Elmculator.Keyboard.input ] }

main =
  app.html

type alias Model = { result: Float, text: String, operator: Operator, clearBuffer: Bool }
emptyModel = { result = 0, text = "", operator = plus, clearBuffer = False }
model : Model
model = emptyModel

view address model =
  div [ class "calculator" ] [
    h1 [ class "calculator__title" ] [ text "The Elmculator" ]
    , div [ ]
      [
      h2 [ class "calculator__resultsarea" ] [ text model.text ]
      , div []
        [
        numberButton address 1
        , numberButton address 2
        , numberButton address 3
        , button [ class "calculator__button__operator", onClick address (Elmculator.Actions.SelectOperator plus) ] [ text "+" ]
        ]
      , div []
        [
        numberButton address 4
        , numberButton address 5
        , numberButton address 6
        , button [ class "calculator__button__operator", onClick address (Elmculator.Actions.SelectOperator minus) ] [ text "-" ]
        ]
      , div []
        [
        numberButton address 7
        , numberButton address 8
        , numberButton address 9
        , button [ class "calculator__button__operator", onClick address (Elmculator.Actions.SelectOperator multiply) ] [ text "*" ]
        ]
      , div []
        [
        button [ class "calculator__button__action", onClick address (Elmculator.Actions.SelectOperator equals) ] [ text "=" ]
        , numberButton address 0
        , button [ class "calculator__button__action", onClick address Elmculator.Actions.Clear ] [ text "C" ]
        , button [ class "calculator__button__operator", onClick address (Elmculator.Actions.SelectOperator divide) ] [ text "/" ]
        ]
      ]
  ]

numberButton address number =
  button [ class "calculator__button", onClick address (Elmculator.Actions.TypeNumber (toString number)) ] [ text (toString number) ]

update: Action -> Model -> (Model, Effects.Effects Action)
update action model =
  let result =
    case action of
      Elmculator.Actions.Nop ->
        model
      Elmculator.Actions.TypeNumber numStr ->
        if model.clearBuffer then
          { model | text = numStr, clearBuffer = False }
        else
          { model | text = model.text ++ numStr }
      Elmculator.Actions.SelectOperator operator ->
        let
          result = applyOperator model.operator model
        in
          { model | result = result, text = toString result, clearBuffer = True, operator = operator }
      Elmculator.Actions.Clear -> emptyModel
    in
      (result, Effects.none)

applyOperator: Operator -> Model -> Float
applyOperator action model =
  let
    a = model.result
    result = String.toFloat model.text
  in
    case result of
      Ok b -> action a b
      Err e -> a
