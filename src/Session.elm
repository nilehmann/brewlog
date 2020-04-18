module Session exposing (Session)

import Browser.Navigation as Nav
import Time


type alias Session =
    { key : Nav.Key
    , zone : Time.Zone
    }
