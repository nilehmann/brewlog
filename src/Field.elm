module Field exposing
    ( Field
    , asMaybe
    , date
    , dateTime
    , edit
    , format
    , fromData
    , fromString
    , isError
    , measure
    , parse
    , unwrap
    , update
    )

import Date exposing (Date)
import DateTime exposing (DateTime)
import Maybe.Extra as Maybe
import Measures exposing (Measure)



-- FIELD PARSER


type alias FieldParser a =
    { parse : String -> Maybe a
    , unparse : a -> String
    }


date : FieldParser Date
date =
    FieldParser Date.parse Date.unparse


measure : FieldParser Measure
measure =
    FieldParser Measures.parse Measures.unparse


dateTime : FieldParser DateTime
dateTime =
    FieldParser DateTime.parse DateTime.unparse



-- FIELD


type alias Field a =
    { parser : FieldParser a
    , state : State a
    }


type State a
    = Editing String
    | Parsed a
    | Error String


fromString : FieldParser a -> String -> Field a
fromString parser text =
    { parser = parser
    , state =
        case parser.parse text of
            Just a ->
                Parsed a

            Nothing ->
                Error text
    }


fromData : FieldParser a -> a -> Field a
fromData parser a =
    { parser = parser
    , state = Parsed a
    }


update : String -> Field a -> Field a
update text field =
    case field.state of
        Editing _ ->
            { field | state = Editing text }

        _ ->
            fromString field.parser text


edit : Field a -> Field a
edit field =
    let
        text =
            case field.state of
                Parsed a ->
                    field.parser.unparse a

                Error s ->
                    s

                Editing s ->
                    s
    in
    { field | state = Editing text }


format : (a -> String) -> Field a -> String
format fmt field =
    case field.state of
        Editing text ->
            text

        Parsed a ->
            fmt a

        Error text ->
            text


parse : Field a -> Field a
parse field =
    case field.state of
        Editing text ->
            fromString field.parser text

        _ ->
            field


asMaybe : Field a -> Maybe a
asMaybe field =
    case (parse field).state of
        Parsed a ->
            Just a

        _ ->
            Nothing


unwrap : Field a -> Maybe String
unwrap field =
    asMaybe field |> Maybe.map field.parser.unparse


isError : Field a -> Bool
isError field =
    case (parse field).state of
        Error _ ->
            True

        _ ->
            False
