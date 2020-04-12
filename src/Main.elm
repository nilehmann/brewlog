module Main exposing (..)

import Array as Array exposing (Array)
import BasicInfo
import Browser
import Browser.Navigation as Nav
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as I
import Hops
import Html
import Html.Attributes as Html
import Http
import Ingredients
import Json.Decode as Json
import Logs
import Objecthash exposing (objecthash)
import Objecthash.Value as V
import Task
import Time
import Url



-- MAIN


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> DoNothing
        , onUrlChange = \_ -> DoNothing
        }



-- MODEL


type alias Model =
    { id : String
    , rev : Maybe String
    , basicInfo : BasicInfo.Model
    , ingredients : Ingredients.Model
    , hops : Hops.Model
    , logs : Logs.Model
    , zone : Time.Zone
    , savedHash : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { id = "490cd76e3ea843d596d09d2c68733862"
      , rev = Nothing
      , savedHash = ""
      , basicInfo = BasicInfo.init
      , ingredients = Ingredients.init
      , hops = Hops.init
      , logs = Logs.init
      , zone = Time.utc
      }
    , Task.perform AdjustTimeZone Time.here
    )


toObjecthashValue : Model -> Maybe V.Value
toObjecthashValue model =
    let
        f basicInfo ingredients hops logs =
            [ ( "basicInfo", basicInfo )
            , ( "ingredients", ingredients )
            , ( "hops", hops )
            , ( "logs", logs )
            ]
                |> Dict.fromList
                |> V.dict
    in
    Maybe.map4
        f
        (BasicInfo.toObjecthashValue model.basicInfo)
        (Ingredients.toObjecthashValue model.ingredients)
        (Just <| Hops.toObjecthashValue model.hops)
        (Logs.toObjecthashValue model.logs)



-- UPDATE


type Msg
    = AdjustTimeZone Time.Zone
    | Tick Time.Posix
    | GotIngredientsMsg Ingredients.Msg
    | GotHopsMsg Hops.Msg
    | GotLogsMsg Logs.Msg
    | GotBasicInfoMsg BasicInfo.Msg
    | SaveBeerResult (Result Http.Error PutResponse)
    | DoNothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )

        GotIngredientsMsg subMsg ->
            ( { model | ingredients = Ingredients.update subMsg model.ingredients }
            , Cmd.none
            )

        GotHopsMsg subMsg ->
            ( { model | hops = Hops.update subMsg model.hops }
            , Cmd.none
            )

        GotLogsMsg subMsg ->
            let
                ( subModel, subCmd ) =
                    Logs.update model.zone subMsg model.logs
            in
            ( { model | logs = subModel }
            , Cmd.map GotLogsMsg subCmd
            )

        GotBasicInfoMsg subMsg ->
            ( { model | basicInfo = BasicInfo.update subMsg model.basicInfo }
            , Cmd.none
            )

        SaveBeerResult result ->
            case Debug.log "save beer" result of
                Ok r ->
                    ( { model | rev = Just r.rev }, Cmd.none )

                Err r ->
                    ( model, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )

        Tick _ ->
            save model


type alias PutResponse =
    { id : String
    , ok : Bool
    , rev : String
    }


save model =
    case toObjecthashValue model of
        Just value ->
            let
                hash =
                    objecthash value
            in
            if hash /= model.savedHash then
                ( { model | savedHash = hash }
                , Http.request
                    { method = "PUT"
                    , headers = []
                    , url = saveUrl model.id model.rev
                    , body = Http.jsonBody (V.toJsonValue value)
                    , expect =
                        Http.expectJson
                            SaveBeerResult
                            (Json.map3 PutResponse
                                (Json.field "id" Json.string)
                                (Json.field "ok" Json.bool)
                                (Json.field "rev" Json.string)
                            )
                    , tracker = Nothing
                    , timeout = Nothing
                    }
                )

            else
                ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


saveUrl id rev =
    let
        base =
            "http://localhost:5984/brewlog/" ++ id
    in
    case rev of
        Just r ->
            base ++ "?rev=" ++ r

        Nothing ->
            base



-- VIEW


fontHref =
    "https://fonts.googleapis.com/css2?family=Indie+Flower&display=swap"


view : Model -> Browser.Document Msg
view model =
    { title = "BrewLog"
    , body =
        [ htmlLink
            [ Html.href fontHref
            , Html.rel "stylesheet"
            ]
        , layout
            [ Font.family [ Font.typeface "Indie Flower" ], Font.size 20 ]
            (page model)
        ]
    }


htmlLink a =
    Html.node "link" a []


page model =
    column [ width fill, spacing 20 ] [ header, body model ]


header =
    box (rgb 0 0 0) fill (px 100)


body model =
    column [ spacing 60, centerX, width (px 920) ]
        [ map GotBasicInfoMsg (BasicInfo.view model.basicInfo)
        , receipe model
        , map GotLogsMsg (Logs.view model.logs (BasicInfo.getDate model.basicInfo))
        ]


receipe model =
    row [ spacing 40, width fill ]
        [ el [ alignLeft, alignTop, width (fillPortion 1) ]
            (map GotIngredientsMsg (Ingredients.view model.ingredients))
        , el [ alignRight, alignTop, width (fillPortion 1) ]
            (map GotHopsMsg (Hops.view model.hops))
        ]


box color w h =
    el [ Background.color color, width w, height h ] (html (Html.div [] []))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 5000 Tick
