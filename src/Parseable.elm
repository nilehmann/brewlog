module Parseable exposing
    ( Parseable
    , format
    , fromData
    , fromString
    , isError
    , parse
    , toMaybe
    , unparse
    , unparsed
    )


type Parseable a
    = Parsed a
    | Unparsed String
    | ParseError String


unparsed : String -> Parseable a
unparsed =
    Unparsed


fromData : a -> Parseable a
fromData =
    Parsed


fromString : (String -> Maybe a) -> String -> Parseable a
fromString parser s =
    case parser s of
        Just result ->
            Parsed result

        Nothing ->
            ParseError s


parse : (String -> Maybe a) -> Parseable a -> Parseable a
parse parser p =
    case p of
        Unparsed s ->
            fromString parser s

        _ ->
            p


unparse : (a -> String) -> Parseable a -> Parseable a
unparse unparser p =
    case p of
        Parsed s ->
            Unparsed (unparser s)

        _ ->
            p


format : (a -> String) -> Parseable a -> String
format formatter p =
    case p of
        Parsed a ->
            formatter a

        Unparsed s ->
            s

        ParseError s ->
            s


isError : Parseable a -> Bool
isError p =
    case p of
        ParseError s ->
            True

        _ ->
            False


toMaybe : Parseable a -> Maybe a
toMaybe p =
    case p of
        Parsed a ->
            Just a

        _ ->
            Nothing
