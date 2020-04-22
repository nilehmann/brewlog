module Measures exposing (Measure, format, parse, unparse)

import Dict
import Parser exposing (..)
import Parser.Dict exposing (fromDict)


type alias Measure =
    { value : Float
    , unit : Unit
    }


type Unit
    = Pounds
    | Kilograms
    | Grams
    | Teaspoons
    | Ounces
    | Gallons
    | Liters


parse : String -> Maybe Measure
parse s =
    Parser.run (succeed identity |. spaces |= measureParser |. spaces |. end) s
        |> Result.toMaybe


measureParser : Parser Measure
measureParser =
    succeed Measure
        |= float
        |. spaces
        |= unitParser


unitParser : Parser Unit
unitParser =
    [ ( "pounds", Pounds )
    , ( "pound", Pounds )
    , ( "kilograms", Kilograms )
    , ( "kilogram", Kilograms )
    , ( "grams", Grams )
    , ( "gram", Grams )
    , ( "teaspoons", Teaspoons )
    , ( "teaspoon", Teaspoons )
    , ( "ounces", Ounces )
    , ( "ounce", Ounces )
    , ( "gallons", Gallons )
    , ( "gallon", Gallons )
    , ( "liters", Liters )
    , ( "liter", Liters )
    ]
        |> Dict.fromList
        |> fromDict


unparse : Measure -> String
unparse measure =
    let
        unit =
            unitToString measure.unit
    in
    if measure.value == 1 then
        String.fromFloat measure.value ++ " " ++ String.dropRight 1 unit

    else
        String.fromFloat measure.value ++ " " ++ unit


format : Measure -> String
format =
    unparse


unitToString : Unit -> String
unitToString unit =
    case unit of
        Pounds ->
            "pounds"

        Kilograms ->
            "kilograms"

        Grams ->
            "grams"

        Teaspoons ->
            "teaspoons"

        Ounces ->
            "ounces"

        Gallons ->
            "gallons"

        Liters ->
            "liters"
