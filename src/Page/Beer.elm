module Page.Beer exposing (Model, Msg, init, subscriptions, update, view)

import Api
import Date
import Dict
import Element exposing (..)
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
    = Initializing
    | Fetched MetaData Beer


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
        ( { zone = session.zone, state = Initializing }
        , Task.perform (NewBeer id) Time.now
        )

    else
        ( { zone = session.zone, state = Initializing }
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
        ( NewBeer id posix, Initializing ) ->
            let
                beer =
                    initBeer model.zone posix

                hash =
                    beerHash beer
            in
            ( { zone = model.zone
              , state = Fetched (MetaData id Nothing hash) beer
              }
            , Cmd.none
            )

        ( GotBeerMsg subMsg, Fetched metadata beer ) ->
            let
                ( newBeer, subCmd ) =
                    updateBeer model.zone subMsg beer
            in
            ( { model | state = Fetched metadata newBeer }, subCmd )

        ( BeerFetched result, _ ) ->
            case Debug.log "beer fetched" result of
                Ok docResult ->
                    let
                        metadata =
                            { id = docResult.id
                            , rev = Just docResult.rev
                            , savedHash = ""
                            }
                    in
                    ( { model | state = Fetched metadata docResult.doc }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ( SaveBeerResult result, Fetched metadata beer ) ->
            case Debug.log "save beer" result of
                Ok r ->
                    ( { model | state = Fetched { metadata | rev = Just r.rev } beer }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ( Tick _, Fetched metadata beer ) ->
            let
                ( _, cmd ) =
                    saveBeer metadata beer
            in
            ( { model | state = Fetched metadata beer }
            , cmd
            )

        -- Ignore messages in the wrong state
        ( _, _ ) ->
            ( model, Cmd.none )


saveBeer : MetaData -> Beer -> ( MetaData, Cmd Msg )
saveBeer metadata beer =
    case toObjecthashValue beer of
        Just value ->
            let
                hash =
                    objecthash value
            in
            if hash /= metadata.savedHash then
                ( { metadata | savedHash = hash }
                , Api.documentPut SaveBeerResult
                    metadata.id
                    metadata.rev
                    (V.toJsonValue value)
                )

            else
                ( metadata, Cmd.none )

        Nothing ->
            ( metadata, Cmd.none )


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


view : Model -> Element Msg
view model =
    case model.state of
        Initializing ->
            none

        Fetched _ beer ->
            map GotBeerMsg (viewFetched beer)


viewFetched : Beer -> Element BeerMsg
viewFetched beer =
    column [ spacing 60, width fill ]
        [ map GotBasicInfoMsg (BasicInfo.view beer.basicInfo)
        , receipe beer
        , map GotLogsMsg (Logs.view beer.logs (BasicInfo.getDate beer.basicInfo))
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
