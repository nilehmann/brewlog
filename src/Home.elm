module Home exposing (..)

import Api
import Browser
import DateTime
import Element exposing (..)
import Http
import Json.Decode as D



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    List Beer


type alias Beer =
    { id : String
    , rev : String
    , name : String
    , batchSize : String
    , date : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( [], Api.view GotBeers beerDecoder )


beerDecoder : D.Decoder Beer
beerDecoder =
    D.map5 Beer
        (D.field "id" D.string)
        (D.field "rev" D.string)
        (D.field "name" D.string)
        (D.field "batchSize" D.string)
        (D.field "date" D.string)



-- UPDATE


type Msg
    = GotBeers (Result Http.Error (Api.ViewResult Beer))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBeers result ->
            case Debug.log ">" result of
                Ok viewResult ->
                    ( List.map .value viewResult.rows, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- VIEW


view model =
    { title = "BrewLog"
    , body = [ layout [] (page model) ]
    }


page model =
    text "here"



-- SUBSCRIPTIONS


subscriptions model =
    Sub.none
