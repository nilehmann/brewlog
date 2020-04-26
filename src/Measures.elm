module Measures exposing (AnyAmount, Mass, Volume, formatAny, formatVolume, parseAny, parseVolume, unparseAny, unparseVolume)

import Dict
import Parser exposing (..)
import Parser.Advanced as A
import Parser.Dict exposing (fromDict)


type AnyAmount
    = MassAmount Mass
    | VolumeAmount Volume


type alias Mass =
    { value : Float
    , unit : MassUnit
    }


type MassUnit
    = Pounds
    | Kilograms
    | Grams
    | Ounces


type alias Volume =
    { value : Float
    , unit : VolumeUnit
    }


type VolumeUnit
    = Gallons
    | Liters
    | Teaspoons


parseAny : String -> Maybe AnyAmount
parseAny s =
    Parser.run (succeed identity |. spaces |= anyAmountParser |. spaces |. end) s
        |> Result.toMaybe


anyAmountParser : Parser AnyAmount
anyAmountParser =
    A.oneOf
        [ map VolumeAmount (backtrackable volumeParser)
        , map MassAmount (backtrackable massParser)
        ]


parseVolume : String -> Maybe Volume
parseVolume s =
    Parser.run (succeed identity |. spaces |= volumeParser |. spaces |. end) s
        |> Result.toMaybe


volumeParser : Parser Volume
volumeParser =
    succeed Volume
        |= float
        |. spaces
        |= volumeUnitParser


massParser : Parser Mass
massParser =
    succeed Mass
        |= float
        |. spaces
        |= massUnitParser


massUnitParser : Parser MassUnit
massUnitParser =
    [ ( "pounds", Pounds )
    , ( "pound", Pounds )
    , ( "lbs", Pounds )
    , ( "lb", Pounds )
    , ( "kilograms", Kilograms )
    , ( "kilogram", Kilograms )
    , ( "kg", Kilograms )
    , ( "grams", Grams )
    , ( "gram", Grams )
    , ( "g", Grams )
    , ( "ounces", Ounces )
    , ( "ounce", Ounces )
    , ( "oz", Ounces )
    ]
        |> Dict.fromList
        |> fromDict


volumeUnitParser : Parser VolumeUnit
volumeUnitParser =
    [ ( "teaspoons", Teaspoons )
    , ( "teaspoon", Teaspoons )
    , ( "gallons", Gallons )
    , ( "gallon", Gallons )
    , ( "liters", Liters )
    , ( "liter", Liters )
    , ( "L", Liters )
    ]
        |> Dict.fromList
        |> fromDict


unparseAny : AnyAmount -> String
unparseAny measure =
    case measure of
        MassAmount m ->
            unparseMass m

        VolumeAmount v ->
            unparseVolume v


unparseMass : Mass -> String
unparseMass mass =
    String.fromFloat mass.value ++ " " ++ massUnitToString mass.unit


unparseVolume : Volume -> String
unparseVolume volume =
    String.fromFloat volume.value ++ " " ++ singularize volume.value (volumeUnitToString volume.unit)


singularize : Float -> String -> String
singularize n s =
    if n == 1 then
        String.dropRight 1 s

    else
        s


formatAny : AnyAmount -> String
formatAny =
    unparseAny


formatVolume : Volume -> String
formatVolume =
    unparseVolume


massUnitToString : MassUnit -> String
massUnitToString unit =
    case unit of
        Pounds ->
            "lbs"

        Kilograms ->
            "kg"

        Grams ->
            "g"

        Ounces ->
            "oz"


volumeUnitToString : VolumeUnit -> String
volumeUnitToString unit =
    case unit of
        Teaspoons ->
            "teaspoons"

        Gallons ->
            "gallons"

        Liters ->
            "liters"
