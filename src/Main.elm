module Elmculator where

import Html exposing (div, button, text, h1, h2)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp
import String exposing (toFloat)

import Operators exposing (..)

main =
  StartApp.start { model = model, view = view, update = update }


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
        , button [ class "calculator__button__operator", onClick address (SelectOperator plus) ] [ text "+" ]
        ]
      , div []
        [
        numberButton address 4
        , numberButton address 5
        , numberButton address 6
        , button [ class "calculator__button__operator", onClick address (SelectOperator minus) ] [ text "-" ]
        ]
      , div []
        [
        numberButton address 7
        , numberButton address 8
        , numberButton address 9
        , button [ class "calculator__button__operator", onClick address (SelectOperator multiply) ] [ text "*" ]
        ]
      , div []
        [
        button [ class "calculator__button__action", onClick address (SelectOperator equals) ] [ text "=" ]
        , numberButton address 0
        , button [ class "calculator__button__action", onClick address Clear ] [ text "C" ]
        , button [ class "calculator__button__operator", onClick address (SelectOperator divide) ] [ text "/" ]
        ]
      ]
  ]


numberButton address number =
  button [ class "calculator__button", onClick address (TypeNumber (toString number)) ] [ text (toString number) ]

type ButtonPress = TypeNumber String | SelectOperator Operator | Clear


update: ButtonPress -> Model -> Model
update action model =
  case action of
    TypeNumber numStr ->
      if model.clearBuffer then
        { model | text = numStr, clearBuffer = False }
      else
        { model | text = model.text ++ numStr }
    SelectOperator operator ->
      let
        result = applyOperator model.operator model
      in
        { model | result = result, text = toString result, clearBuffer = True, operator = operator }
    Clear -> emptyModel

applyOperator: Operator -> Model -> Float
applyOperator action model =
  let
    a = model.result
    result = String.toFloat model.text
  in
    case result of
      Ok b -> action a b
      Err e -> a
