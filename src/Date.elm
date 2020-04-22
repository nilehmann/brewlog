module Date exposing
    ( Date
    , cap
    , compare
    , format
    , fromPosix
    , parse
    , parser
    , unparse
    )

import Dict
import Parseable exposing (Parseable)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , andThen
        , chompWhile
        , end
        , getChompedString
        , problem
        , spaces
        , succeed
        )
import Parser.Dict exposing (fromDict)
import Time



-- DATE


type alias Date =
    { day : Int
    , month : Time.Month
    , year : Int
    }


fromPosix : Time.Zone -> Time.Posix -> Date
fromPosix zone posix =
    { day = Time.toDay zone posix
    , month = Time.toMonth zone posix
    , year = Time.toYear zone posix
    }


cap : Date -> Date
cap d =
    { d | day = clamp 1 (daysInMonth d.month d.year) d.day }


compare : Date -> Date -> Order
compare d1 d2 =
    let
        a =
            ( d1.year, monthToInt d1.month, d1.day )

        b =
            ( d2.year, monthToInt d2.month, d2.day )
    in
    Basics.compare a b



-- PARSER


parse : String -> Maybe Date
parse str =
    Parser.run (succeed cap |. spaces |= parser |. spaces |. end) str
        |> Result.toMaybe


parser : Parser Date
parser =
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



-- UNPARSE


unparse : Date -> String
unparse d =
    let
        ( day, month, year ) =
            unpack d
    in
    day ++ " " ++ String.left 3 month ++ " " ++ year


unpack : Date -> ( String, String, String )
unpack d =
    ( String.fromInt d.day, monthToString d.month, String.fromInt d.year )



-- FORMAT


format : Bool -> Date -> String
format short date =
    let
        ( day, month, year ) =
            unpack date
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
