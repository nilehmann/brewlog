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
    | NotFound


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Initializing, Task.perform (Initialize url << Session key) Time.here )



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
            -- ( Initialized session NotFound, Cmd.none )
            changeRouteTo (Just Route.Home) session


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
    let
        { body, status } =
            viewPage model
    in
    { title = "BrewLog"
    , body =
        [ Html.node "link"
            [ Html.href fontHref
            , Html.rel "stylesheet"
            ]
            []
        , layout
            [ Font.family
                [ Font.typeface "Indie Flower"
                ]
            , Font.color (rgba255 34 34 34 1)
            , Font.size 24
            , Background.tiled "/assets/background.jpg"
            , width fill
            ]
            (column [ spacing 0, width (px 1024), height fill, centerX ]
                [ el
                    [ width fill
                    , height (px 126)
                    , Background.uncropped "/assets/notebook-top.jpg"
                    , paddingEach { top = 0, left = 100, right = 40, bottom = 0 }
                    ]
                    (el [ alignRight, alignBottom, zIndex 1 ] status)
                , el
                    [ width fill
                    , height fill
                    , centerX
                    , Background.tiledY "/assets/notebook-middle.jpg"
                    , paddingEach { top = 6, left = 100, right = 40, bottom = 100 }
                    ]
                    body
                ]
            )
        ]
    }


zIndex : Int -> Attribute msg
zIndex idx =
    htmlAttribute (Html.style "z-index" (String.fromInt idx))


fontHref : String
fontHref =
    "https://fonts.googleapis.com/css2?family=Indie+Flower&display=swap"


viewPage : Model -> { body : Element Msg, status : Element Msg }
viewPage model =
    case model of
        Initialized _ (Home subModel) ->
            viewWith GotHomeMsg (Home.view subModel)

        Initialized _ (Beer subModel) ->
            viewWith GotBeerMsg (Beer.view subModel)

        Initialized _ NotFound ->
            view404

        Initializing ->
            { body = none, status = none }


viewWith :
    (msg -> Msg)
    -> { body : Element msg, status : Element msg }
    -> { body : Element Msg, status : Element Msg }
viewWith toMsg subView =
    { body = map toMsg subView.body
    , status = map toMsg subView.status
    }


view404 : { body : Element Msg, status : Element Msg }
view404 =
    { body =
        el
            [ Font.family
                [ Font.typeface "Arial"
                ]
            , centerY
            , centerX
            , moveDown 8
            , Font.size 40
            , Font.bold
            ]
            (text "404")
    , status = none
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initialized _ (Beer _) ->
            Sub.map GotBeerMsg Beer.subscriptions

        _ ->
            Sub.none
