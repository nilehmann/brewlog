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
import Ingredients
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
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { basicInfo = BasicInfo.init
      , ingredients = Ingredients.init
      , hops = Hops.init
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotIngredientsMsg Ingredients.Msg
    | GotHopsMsg Hops.Msg
    | GotBasicInfoMsg BasicInfo.Msg
    | DoNothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotIngredientsMsg subMsg ->
            ( { model | ingredients = Ingredients.update subMsg model.ingredients }
            , Cmd.none
            )

        GotHopsMsg subMsg ->
            ( { model | hops = Hops.update subMsg model.hops }
            , Cmd.none
            )

        GotBasicInfoMsg subMsg ->
            ( { model | basicInfo = BasicInfo.update subMsg model.basicInfo }
            , Cmd.none
            )

        DoNothing ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Brewing Day"
    , body = [ layout [ Background.color (rgb 1 0 0), Font.size 16 ] (page model) ]
    }


page model =
    column [ width fill, spacing 20 ] [ header, body model ]


header =
    box (rgb 0 0 0) fill (px 100)


body model =
    column [ spacing 20, centerX, width (px 920), Background.color (rgb 1 0 1) ]
        [ map GotBasicInfoMsg (BasicInfo.view model.basicInfo)
        , receipe model
        ]


receipe model =
    row [ spacing 100, width fill ]
        [ el [ alignLeft, alignTop, width (fillPortion 1) ]
            (map GotIngredientsMsg (Ingredients.view model.ingredients))
        , el [ alignRight, alignTop, width (fillPortion 1) ]
            (map GotHopsMsg (Hops.view model.hops))
        ]


box color w h =
    el [ Background.color color, width w, height h ] (html (Html.div [] []))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
