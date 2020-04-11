module Main exposing (..)

import Array as Array exposing (Array)
import BasicInfo
import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as I
import Hops
import Html
import Html.Attributes as Html
import Ingredients
import Logs
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
    { basicInfo : BasicInfo.Model
    , ingredients : Ingredients.Model
    , hops : Hops.Model
    , logs : Logs.Model
    , zone : Time.Zone
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { basicInfo = BasicInfo.init
      , ingredients = Ingredients.init
      , hops = Hops.init
      , logs = Logs.init
      , zone = Time.utc
      }
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = GotIngredientsMsg Ingredients.Msg
    | GotHopsMsg Hops.Msg
    | GotLogsMsg Logs.Msg
    | GotBasicInfoMsg BasicInfo.Msg
    | DoNothing
    | AdjustTimeZone Time.Zone
    | Tick Time.Posix


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

        DoNothing ->
            ( model, Cmd.none )

        Tick posix ->
            ( model, Cmd.none )



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
    Debug.log "1" Sub.none



-- Time.every 5000 Tick
