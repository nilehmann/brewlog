module Route exposing (Route(..), fromUrl)

import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = Home
    | Beer String


fromUrl : Url -> Maybe Route
fromUrl =
    parse parser


parser =
    oneOf
        [ map Home top
        , map Beer (s "beer" </> string)
        ]
