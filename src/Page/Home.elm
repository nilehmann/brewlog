module Page.Home exposing (Model, Msg, init, update, view)

import Api
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font as Font
import Element.Input as I
import Http
import Json.Decode as D
import Random
import Random.Char as Random
import Random.Extra as Random
import Random.String as Random
import Route



-- MODEL


type alias Model =
    { key : Nav.Key
    , beers : List Beer
    }


type alias Beer =
    { id : String
    , name : String
    , batchSize : String
    , date : String
    }


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { key = key, beers = [] }, Api.view GotBeers beerDecoder )


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
    | NewBeer String
    | ClickedNewBeer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedNewBeer ->
            ( model, Random.generate NewBeer randomId )

        NewBeer id ->
            ( model, Nav.pushUrl model.key (Route.toString (Route.NewBeer id)) )

        GotBeers (Ok result) ->
            ( { model | beers = List.map .value result.rows }, Cmd.none )

        GotBeers (Err err) ->
            let
                _ =
                    Debug.log ">" err
            in
            ( model, Cmd.none )


randomId : Random.Generator String
randomId =
    Random.string 10 randomAlphaNum


randomAlphaNum : Random.Generator Char
randomAlphaNum =
    Random.choices Random.lowerCaseLatin [ Random.char 48 57 ]



-- VIEW


view : Model -> Element Msg
view model =
    column []
        [ I.button
            []
            { onPress = Just ClickedNewBeer
            , label = text "New"
            }
        , viewBeers model
        ]


viewBeers : Model -> Element Msg
viewBeers model =
    table []
        { data = model.beers
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


tableHeader : String -> Element Msg
tableHeader t =
    el [ Font.bold ] (text t)
