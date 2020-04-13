module Beer exposing (Model, Msg, init, update, view)

import Api
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


init : String -> ( Model, Cmd Msg )
init id =
    ( { id = id
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
    | SaveBeerResult (Result Http.Error Api.DocPutResult)
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


save model =
    case toObjecthashValue model of
        Just value ->
            let
                hash =
                    objecthash value
            in
            if hash /= model.savedHash then
                ( { model | savedHash = hash }
                , Api.documentPut SaveBeerResult
                    model.id
                    model.rev
                    (V.toJsonValue value)
                )

            else
                ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )



-- VIEW


fontHref =
    "https://fonts.googleapis.com/css2?family=Indie+Flower&display=swap"


view : Model -> Element Msg
view model =
    column [ spacing 60, width fill ]
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 5000 Tick
