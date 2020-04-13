module Page.Home exposing (..)

import Api
import Browser
import DateTime
import Element exposing (..)
import Element.Font as Font
import Http
import Json.Decode as D



-- MODEL


type alias Model =
    List Beer


type alias Beer =
    { id : String
    , name : String
    , batchSize : String
    , date : String
    }


init : ( Model, Cmd Msg )
init =
    ( [], Api.view GotBeers beerDecoder )


beerDecoder : D.Decoder Beer
beerDecoder =
    D.map4 Beer
        (D.field "id" D.string)
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


view : Model -> Element Msg
view model =
    table []
        { data = model
        , columns =
            [ { header = tableHeader "Name"
              , width = fill
              , view =
                    \beer ->
                        link []
                            { url = "/beer/" ++ beer.id
                            , label = text beer.name
                            }
              }
            , { header = tableHeader "Batch Size"
              , width = fill
              , view = text << .batchSize
              }
            , { header = tableHeader "Date"
              , width = fill
              , view = text << .date
              }
            ]
        }


tableHeader t =
    el [ Font.bold ] (text t)



-- SUBSCRIPTIONS


subscriptions model =
    Sub.none
