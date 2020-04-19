module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html
import Html.Attributes as Html
import Page.Beer as Beer
import Page.Home as Home
import Route exposing (Route)
import Session exposing (Session)
import Task
import Time
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type Model
    = Initializing
    | Initialized Session Page


type Page
    = Home Home.Model
    | Beer Beer.Model
    | Blank


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Initializing, Task.perform (Initialize url << Session key) Time.here )



-- changeRouteTo (Route.fromUrl url) { key = key, page = Blank }
-- UPDATE


type Msg
    = Initialize Url.Url Session
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotHomeMsg Home.Msg
    | GotBeerMsg Beer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Initialize url session, Initializing ) ->
            changeRouteTo (Route.fromUrl url) session

        ( LinkClicked urlRequest, Initialized session _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl session.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, Initialized session _ ) ->
            changeRouteTo (Route.fromUrl url) session

        ( GotBeerMsg subMsg, Initialized session (Beer subModel) ) ->
            Beer.update subMsg subModel
                |> updateWith Beer GotBeerMsg session

        ( GotHomeMsg subMsg, Initialized session (Home subModel) ) ->
            Home.update subMsg subModel
                |> updateWith Home GotHomeMsg session

        -- Ignore messages sent to the wrong page
        ( _, _ ) ->
            ( model, Cmd.none )


changeRouteTo : Maybe Route -> Session -> ( Model, Cmd Msg )
changeRouteTo route session =
    case route of
        Just Route.Home ->
            Home.init session.key
                |> updateWith Home GotHomeMsg session

        Just (Route.Beer id) ->
            Beer.init session id False
                |> updateWith Beer GotBeerMsg session

        Just (Route.NewBeer id) ->
            Beer.init session id True
                |> updateWith Beer GotBeerMsg session

        Nothing ->
            Home.init session.key
                |> updateWith Home GotHomeMsg session


updateWith :
    (subModel -> Page)
    -> (subMsg -> Msg)
    -> Session
    -> ( subModel, Cmd subMsg )
    -> ( Model, Cmd Msg )
updateWith toPage toMsg session ( subModel, subCmd ) =
    ( Initialized session (toPage subModel)
    , Cmd.map toMsg subCmd
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "BrewLog"
    , body =
        [ Html.node "link"
            [ Html.href fontHref
            , Html.rel "stylesheet"
            ]
            []
        , layout
            [ Font.family [ Font.typeface "Indie Flower" ], Font.size 24, width fill ]
            (column [ width fill, spacing 0, width (px 1024), centerX ]
                [ el
                    [ width fill
                    , height (px 126)
                    , Background.uncropped "/assets/background-top.jpg"
                    ]
                    none
                , el
                    [ width fill
                    , centerX
                    , Background.tiledY "/assets/background.jpg"
                    , paddingEach { top = 6, left = 100, right = 20, bottom = 100 }
                    ]
                    (bodyView model)
                ]
            )
        ]
    }


fontHref : String
fontHref =
    "https://fonts.googleapis.com/css2?family=Indie+Flower&display=swap"


bodyView : Model -> Element Msg
bodyView model =
    case model of
        Initialized _ (Home subModel) ->
            Element.map GotHomeMsg (Home.view subModel)

        Initialized _ (Beer subModel) ->
            Element.map GotBeerMsg (Beer.view subModel)

        Initialized _ Blank ->
            none

        Initializing ->
            none



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initialized _ (Beer _) ->
            Sub.map GotBeerMsg Beer.subscriptions

        _ ->
            Sub.none
