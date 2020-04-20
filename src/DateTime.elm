module DateTime exposing
    ( DateTime
    , Time
    , format
    , fromPosix
    , fromString
    , isError
    , parse
    , toString
    , unparse
    )

import Date
import Parseable exposing (Parseable)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , andThen
        , chompIf
        , end
        , getChompedString
        , problem
        , spaces
        , succeed
        , symbol
        )
import Time


type alias DateTime =
    { date : Date.Date
    , time : Time
    }


isError : Parseable DateTime -> Bool
isError =
    Parseable.isError parseDateTime


fromString : String -> Parseable DateTime
fromString =
    Parseable.fromString parseDateTime


toString : Parseable DateTime -> Maybe String
toString =
    Parseable.toString unparseDateTime


parseDateTime : String -> Maybe DateTime
parseDateTime str =
    Parser.run (succeed cap |. spaces |= parser |. spaces |. end) str
        |> Result.toMaybe


parse : Parseable DateTime -> Parseable DateTime
parse =
    Parseable.parse parseDateTime


fromPosix : Time.Zone -> Time.Posix -> DateTime
fromPosix zone posix =
    { date = Date.fromPosix zone posix
    , time =
        { hour = Time.toHour zone posix
        , minute = Time.toMinute zone posix
        }
    }


parser : Parser DateTime
parser =
    succeed DateTime
        |= Date.parser
        |. spaces
        |. symbol ","
        |. spaces
        |= timeParser


cap : DateTime -> DateTime
cap dateTime =
    { time = capTime dateTime.time
    , date = Date.cap dateTime.date
    }



-- TIME


type alias Time =
    { hour : Int
    , minute : Int
    }


capTime : Time -> Time
capTime t =
    { hour = t.hour, minute = clamp 0 59 t.minute }


timeParser : Parser Time
timeParser =
    succeed Time
        |= digit2 "hour"
        |. symbol ":"
        |= digit2 "minutes"



-- 2DIGIT


digit2 : String -> Parser Int
digit2 desc =
    succeed ()
        |. chompIf Char.isDigit
        |. chompIf Char.isDigit
        |> getChompedString
        |> andThen
            (\str ->
                case String.toInt str of
                    Just n ->
                        succeed n

                    Nothing ->
                        problem ("Invalid two digit " ++ desc)
            )



-- UNPARSE


unparse : Parseable DateTime -> Parseable DateTime
unparse =
    Parseable.unparse unparseDateTime


unparseDateTime : DateTime -> String
unparseDateTime dateTime =
    Date.unparseDate dateTime.date ++ ", " ++ unparseTime dateTime.time


unpackTime : Time -> ( String, String )
unpackTime t =
    ( String.fromInt t.hour, String.fromInt t.minute )


unparseTime : Time -> String
unparseTime t =
    let
        ( hour, minute ) =
            unpackTime t
    in
    String.padLeft 2 '0' hour ++ ":" ++ String.padLeft 2 '0' minute



-- FORMAT


format : Maybe Date.Date -> DateTime -> String
format reference dateTime =
    case reference of
        Just date ->
            if Date.compare date dateTime.date == EQ then
                unparseTime dateTime.time

            else
                Date.format True dateTime.date

        Nothing ->
            Date.format True dateTime.date
