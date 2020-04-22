module Measures exposing (Measure, format, fromString, isError, parse, parseMeasure, toString, unparse, unparseMeasure)

import Dict
import Parseable exposing (Parseable)
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


isError : Parseable Measure -> Bool
isError =
    Parseable.isError parseMeasure


fromString : String -> Parseable Measure
fromString =
    Parseable.fromString parseMeasure


toString : Parseable Measure -> Maybe String
toString =
    Parseable.toString unparseMeasure


parse : Parseable Measure -> Parseable Measure
parse =
    Parseable.parse parseMeasure


unparse : Parseable Measure -> Parseable Measure
unparse =
    Parseable.unparse unparseMeasure


parseMeasure : String -> Maybe Measure
parseMeasure s =
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


unparseMeasure : Measure -> String
unparseMeasure measure =
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
    unparseMeasure


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
