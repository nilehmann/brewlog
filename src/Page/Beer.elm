module Page.Beer exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Date
import Dict
import Element exposing (..)
import Element.Font as Font
import Http
import Json.Decode as D
import Objecthash exposing (objecthash)
import Objecthash.Value as V
import Page.Beer.BasicInfo as BasicInfo
import Page.Beer.Hops as Hops
import Page.Beer.Ingredients as Ingredients
import Page.Beer.Logs as Logs
import Session exposing (Session)
import Task
import Time



-- MODEL


type alias Model =
    { zone : Time.Zone
    , state : State
    }


type State
    = Loading
    | Ready MetaData Beer


type alias MetaData =
    { id : String
    , rev : Maybe String
    , savedHash : String
    }


type alias Beer =
    { basicInfo : BasicInfo.Model
    , ingredients : Ingredients.Model
    , hops : Hops.Model
    , logs : Logs.Model
    }


init : Session -> String -> Bool -> ( Model, Cmd Msg )
init session id new =
    if new then
        ( { zone = session.zone, state = Loading }
        , Task.perform (NewBeer id) Time.now
        )

    else
        ( { zone = session.zone, state = Loading }
        , Api.document BeerFetched beerDecoder id
        )


initBeer : Time.Zone -> Time.Posix -> Beer
initBeer zone posix =
    { basicInfo = BasicInfo.init (Date.fromPosix zone posix)
    , ingredients = Ingredients.init
    , hops = Hops.init
    , logs = Logs.init
    }


beerHash : Beer -> String
beerHash beer =
    toObjecthashValue beer
        |> Maybe.map Objecthash.objecthash
        |> Maybe.withDefault ""


beerDecoder : D.Decoder Beer
beerDecoder =
    D.map4 Beer
        (D.field "basicInfo" BasicInfo.decoder)
        (D.field "ingredients" Ingredients.decoder)
        (D.field "hops" Hops.decoder)
        (D.field "logs" Logs.decoder)


toObjecthashValue : Beer -> Maybe V.Value
toObjecthashValue beer =
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
        (BasicInfo.toObjecthashValue beer.basicInfo)
        (Ingredients.toObjecthashValue beer.ingredients)
        (Just <| Hops.toObjecthashValue beer.hops)
        (Logs.toObjecthashValue beer.logs)



-- UPDATE


type Msg
    = Tick Time.Posix
    | NewBeer String Time.Posix
    | GotBeerMsg BeerMsg
    | BeerFetched (Result Http.Error (Api.DocResult Beer))
    | SaveBeerResult (Result Http.Error Api.DocPutResult)


type BeerMsg
    = GotIngredientsMsg Ingredients.Msg
    | GotHopsMsg Hops.Msg
    | GotLogsMsg Logs.Msg
    | GotBasicInfoMsg BasicInfo.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.state ) of
        ( NewBeer id posix, Loading ) ->
            let
                beer =
                    initBeer model.zone posix

                hash =
                    beerHash beer
            in
            ( { zone = model.zone
              , state = Ready (MetaData id Nothing hash) beer
              }
            , Cmd.none
            )

        ( GotBeerMsg subMsg, Ready metadata beer ) ->
            let
                ( newBeer, subCmd ) =
                    updateBeer model.zone subMsg beer
            in
            ( { model | state = Ready metadata newBeer }, subCmd )

        ( BeerFetched result, _ ) ->
            case Debug.log "beer fetched" result of
                Ok docResult ->
                    let
                        metadata =
                            { id = docResult.id
                            , rev = Just docResult.rev
                            , savedHash = beerHash docResult.doc
                            }
                    in
                    ( { model | state = Ready metadata docResult.doc }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ( SaveBeerResult result, Ready metadata beer ) ->
            case Debug.log "save beer" result of
                Ok r ->
                    ( { model
                        | state =
                            Ready
                                { metadata | rev = Just r.rev, savedHash = beerHash beer }
                                beer
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ( Tick _, Ready metadata beer ) ->
            ( model
            , saveBeer metadata beer
            )

        -- Ignore messages in the wrong state
        ( _, _ ) ->
            ( model, Cmd.none )


saveBeer : MetaData -> Beer -> Cmd Msg
saveBeer metadata beer =
    case toObjecthashValue beer of
        Just value ->
            let
                hash =
                    objecthash value
            in
            if hash /= metadata.savedHash then
                Api.documentPut SaveBeerResult
                    metadata.id
                    metadata.rev
                    (V.toJsonValue value)

            else
                Cmd.none

        Nothing ->
            Cmd.none


updateBeer : Time.Zone -> BeerMsg -> Beer -> ( Beer, Cmd Msg )
updateBeer zone msg beer =
    case msg of
        GotIngredientsMsg subMsg ->
            ( { beer | ingredients = Ingredients.update subMsg beer.ingredients }
            , Cmd.none
            )

        GotHopsMsg subMsg ->
            ( { beer | hops = Hops.update subMsg beer.hops }
            , Cmd.none
            )

        GotLogsMsg subMsg ->
            let
                ( subModel, subCmd ) =
                    Logs.update zone subMsg beer.logs
            in
            ( { beer | logs = subModel }
            , Cmd.map (GotBeerMsg << GotLogsMsg) subCmd
            )

        GotBasicInfoMsg subMsg ->
            ( { beer | basicInfo = BasicInfo.update subMsg beer.basicInfo }
            , Cmd.none
            )



-- VIEW


view : Model -> { body : Element Msg, status : Element Msg }
view model =
    case model.state of
        Loading ->
            { body = none, status = status model }

        Ready _ beer ->
            { body = map GotBeerMsg (viewReady beer)
            , status = status model
            }


status : Model -> Element Msg
status model =
    let
        ( message, color ) =
            case model.state of
                Ready metadata beer ->
                    case toObjecthashValue beer of
                        Just value ->
                            if Objecthash.objecthash value /= metadata.savedHash then
                                ( "Saving...", rgba255 119 119 119 1 )

                            else
                                ( "All changes saved", rgba255 119 119 119 1 )

                        Nothing ->
                            ( "The batch contains errors", rgba 1 0 0 1 )

                Loading ->
                    ( "Loading...", rgba255 119 119 119 1 )
    in
    el [ Font.size 20, Font.underline, Font.color color ] (text message)


viewReady : Beer -> Element BeerMsg
viewReady beer =
    column [ spacing 39, width fill ]
        [ map GotBasicInfoMsg (BasicInfo.view beer.basicInfo)
        , column [ spacing 49, width fill ]
            [ receipe beer
            , map GotLogsMsg (Logs.view beer.logs (BasicInfo.getDate beer.basicInfo))
            ]
        ]


receipe : Beer -> Element BeerMsg
receipe beer =
    row [ spacing 40, width fill ]
        [ el [ alignLeft, alignTop, width (fillPortion 1) ]
            (map GotIngredientsMsg (Ingredients.view beer.ingredients))
        , el [ alignRight, alignTop, width (fillPortion 1) ]
            (map GotHopsMsg (Hops.view beer.hops))
        ]



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Time.every 5000 Tick
