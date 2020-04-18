module Route exposing (Route(..), fromUrl, toString)

import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = Home
    | Beer String
    | NewBeer String


fromUrl : Url -> Maybe Route
fromUrl =
    parse parser


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map Beer (s "beer" </> string)
        , map NewBeer (s "beer" </> string </> s "new")
        ]


toString : Route -> String
toString route =
    case route of
        Home ->
            "/"

        Beer id ->
            "/beer" ++ id

        NewBeer id ->
            "/beer/" ++ id ++ "/new"
