module DateTime exposing
    ( Date
    , DateTime
    , Time
    , dateFromPosix
    , format
    , formatDate
    , fromPosix
    , parse
    , parseDate
    , unparse
    , unparseDate
    )

import Dict
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , andThen
        , chompIf
        , chompWhile
        , end
        , getChompedString
        , problem
        , spaces
        , succeed
        , symbol
        )
import Parser.Dict exposing (fromDict)
import Time


type alias DateTime =
    { date : Date
    , time : Time
    }


parse : String -> Maybe DateTime
parse str =
    Parser.run (succeed cap |. spaces |= parser |. spaces |. end) str
        |> Result.toMaybe


fromPosix : Time.Zone -> Time.Posix -> DateTime
fromPosix zone posix =
    { date = dateFromPosix zone posix
    , time =
        { hour = Time.toHour zone posix
        , minute = Time.toMinute zone posix
        }
    }


parser : Parser DateTime
parser =
    succeed DateTime
        |= dateParser
        |. spaces
        |. symbol ","
        |. spaces
        |= timeParser


cap : DateTime -> DateTime
cap dateTime =
    { time = capTime dateTime.time
    , date = capDate dateTime.date
    }



-- DATE


type alias Date =
    { day : Int
    , month : Time.Month
    , year : Int
    }


dateFromPosix : Time.Zone -> Time.Posix -> Date
dateFromPosix zone posix =
    { day = Time.toDay zone posix
    , month = Time.toMonth zone posix
    , year = Time.toYear zone posix
    }


capDate : Date -> Date
capDate d =
    { d | day = clamp 1 (daysInMonth d.month d.year) d.day }


parseDate : String -> Maybe Date
parseDate str =
    Parser.run (succeed capDate |. spaces |= dateParser |. spaces |. end) str
        |> Result.toMaybe


compareDate : Date -> Date -> Order
compareDate d1 d2 =
    let
        a =
            ( d1.year, monthToInt d1.month, d1.day )

        b =
            ( d2.year, monthToInt d2.month, d2.day )
    in
    compare a b


dateParser : Parser Date
dateParser =
    succeed Date
        |= dayParser
        |. spaces
        |= monthParser
        |. spaces
        |= yearParser


dayParser : Parser Int
dayParser =
    chompWhile Char.isDigit
        |> getChompedString
        |> andThen checkDay


checkDay : String -> Parser Int
checkDay str =
    case ( String.length str, String.toInt str ) of
        ( 1, Just n ) ->
            succeed n

        ( 2, Just n ) ->
            succeed n

        _ ->
            problem "Invalid day"


monthParser : Parser Time.Month
monthParser =
    [ ( "Jan", Time.Jan )
    , ( "Feb", Time.Feb )
    , ( "Mar", Time.Mar )
    , ( "Apr", Time.Apr )
    , ( "May", Time.May )
    , ( "Jun", Time.Jun )
    , ( "Jul", Time.Jul )
    , ( "Aug", Time.Aug )
    , ( "Sep", Time.Sep )
    , ( "Oct", Time.Oct )
    , ( "Nov", Time.Nov )
    , ( "Dec", Time.Dec )
    ]
        |> Dict.fromList
        |> fromDict


yearParser : Parser Int
yearParser =
    chompWhile Char.isDigit
        |> getChompedString
        |> andThen checkYear


checkYear : String -> Parser Int
checkYear str =
    case ( String.length str, String.toInt str ) of
        ( 2, Just n ) ->
            if n >= 50 then
                succeed (1900 + n)

            else
                succeed (2000 + n)

        ( 3, Just n ) ->
            succeed (1900 + n)

        ( 4, Just n ) ->
            succeed n

        _ ->
            problem "Invalid year"



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


unparse : DateTime -> String
unparse dateTime =
    unparseDate dateTime.date ++ ", " ++ unparseTime dateTime.time


unpackDate : Date -> ( String, String, String )
unpackDate d =
    ( String.fromInt d.day, monthToString d.month, String.fromInt d.year )


unparseDate : Date -> String
unparseDate d =
    let
        ( day, month, year ) =
            unpackDate d
    in
    day ++ " " ++ String.left 3 month ++ " " ++ year


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


format : Maybe Date -> DateTime -> String
format reference dateTime =
    case reference of
        Just date ->
            if compareDate date dateTime.date == EQ then
                unparseTime dateTime.time

            else
                formatDate True dateTime.date

        Nothing ->
            formatDate True dateTime.date


formatDate : Bool -> Date -> String
formatDate short date =
    let
        ( day, month, year ) =
            unpackDate date
    in
    if short then
        month ++ " " ++ day ++ ", " ++ "’" ++ String.right 2 year

    else
        month ++ " " ++ day ++ ", " ++ year



-- MISC


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            0

        Time.Feb ->
            1

        Time.Mar ->
            2

        Time.Apr ->
            3

        Time.May ->
            4

        Time.Jun ->
            5

        Time.Jul ->
            6

        Time.Aug ->
            7

        Time.Sep ->
            8

        Time.Oct ->
            9

        Time.Nov ->
            10

        Time.Dec ->
            11


daysInMonth : Time.Month -> Int -> Int
daysInMonth month year =
    case month of
        Time.Jan ->
            21

        Time.Feb ->
            if isLeap year then
                29

            else
                28

        Time.Mar ->
            31

        Time.Apr ->
            30

        Time.May ->
            31

        Time.Jun ->
            30

        Time.Jul ->
            31

        Time.Aug ->
            31

        Time.Sep ->
            30

        Time.Oct ->
            31

        Time.Nov ->
            30

        Time.Dec ->
            31


isLeap : Int -> Bool
isLeap year =
    if remainderBy year 4 /= 0 then
        False

    else if remainderBy year 100 /= 0 then
        True

    else
        remainderBy year 400 == 0


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"
