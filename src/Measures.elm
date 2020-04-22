module Measures exposing (AnyAmount, Mass, Volume, formatAny, formatVolume, parseAny, parseVolume, unparseAny, unparseVolume)

import Dict
import Parser exposing (..)
import Parser.Dict exposing (fromDict)


type alias AnyAmount =
    { value : Float
    , unit : AnyUnit
    }


type AnyUnit
    = MassUnit MassUnit
    | VolumeUnit VolumeUnit


type alias Mass =
    { value : Float
    , unit : MassUnit
    }


type MassUnit
    = Pounds
    | Kilograms
    | Grams


type alias Volume =
    { value : Float
    , unit : VolumeUnit
    }


type VolumeUnit
    = Ounces
    | Gallons
    | Liters
    | Teaspoons


parseAny : String -> Maybe AnyAmount
parseAny s =
    Parser.run (succeed identity |. spaces |= anyAmountParser |. spaces |. end) s
        |> Result.toMaybe


anyAmountParser : Parser AnyAmount
anyAmountParser =
    succeed AnyAmount
        |= float
        |. spaces
        |= anyUnitParser


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


anyUnitParser : Parser AnyUnit
anyUnitParser =
    oneOf [ map MassUnit massUnitParser, map VolumeUnit volumeUnitParser ]


massUnitParser : Parser MassUnit
massUnitParser =
    [ ( "pounds", Pounds )
    , ( "pound", Pounds )
    , ( "kilograms", Kilograms )
    , ( "kilogram", Kilograms )
    , ( "grams", Grams )
    , ( "gram", Grams )
    ]
        |> Dict.fromList
        |> fromDict


volumeUnitParser : Parser VolumeUnit
volumeUnitParser =
    [ ( "teaspoons", Teaspoons )
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


unparseAny : AnyAmount -> String
unparseAny measure =
    String.fromFloat measure.value ++ " " ++ singularize measure.value (anyUnitToString measure.unit)


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


anyUnitToString : AnyUnit -> String
anyUnitToString unit =
    case unit of
        MassUnit u ->
            massUnitToString u

        VolumeUnit u ->
            volumeUnitToString u


massUnitToString : MassUnit -> String
massUnitToString unit =
    case unit of
        Pounds ->
            "pounds"

        Kilograms ->
            "kilograms"

        Grams ->
            "grams"


volumeUnitToString : VolumeUnit -> String
volumeUnitToString unit =
    case unit of
        Teaspoons ->
            "teaspoons"

        Ounces ->
            "ounces"

        Gallons ->
            "gallons"

        Liters ->
            "liters"
