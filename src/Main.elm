module Main exposing (main)

import Beer
import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Home
import Html
import Html.Attributes as Html
import Route exposing (Route)
import Url


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


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = Home Home.Model
    | Beer Beer.Model
    | Blank


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    changeRouteTo (Route.fromUrl url) { key = key, page = Blank }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotHomeMsg Home.Msg
    | GotBeerMsg Beer.Msg
    | DoNothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( GotHomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel
                |> updateWith Home GotHomeMsg model

        ( DoNothing, _ ) ->
            ( model, Cmd.none )

        -- Ignore messages sent to the wrong page
        ( _, _ ) ->
            ( model, Cmd.none )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    case route of
        Just Route.Home ->
            Home.init
                |> updateWith Home GotHomeMsg model

        Just (Route.Beer id) ->
            Beer.init id
                |> updateWith Beer GotBeerMsg model

        Nothing ->
            Home.init
                |> updateWith Home GotHomeMsg model


updateWith :
    (subModel -> Page)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, Cmd subMsg )
    -> ( Model, Cmd Msg )
updateWith toPage toMsg model ( subModel, subCmd ) =
    ( { model | page = toPage subModel }
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
            [ Font.family [ Font.typeface "Indie Flower" ], Font.size 20 ]
            (column [ width fill, spacing 20 ]
                [ headerView
                , el [ width (px 920), centerX ] (bodyView model)
                ]
            )
        ]
    }


fontHref =
    "https://fonts.googleapis.com/css2?family=Indie+Flower&display=swap"


headerView =
    box (rgb 0 0 0) fill (px 100)


bodyView model =
    case model.page of
        Home subModel ->
            Element.map GotHomeMsg (Home.view subModel)

        Beer subModel ->
            Element.map GotBeerMsg (Beer.view subModel)

        Blank ->
            none


box color w h =
    el [ Background.color color, width w, height h ] (html (Html.div [] []))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
