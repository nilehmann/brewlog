module Page.Beer.BasicInfo exposing
    ( Model
    , Msg
    , decoder
    , getDate
    , init
    , toObjecthashValue
    , update
    , view
    )

import Date exposing (Date)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as I
import Json.Decode as D
import Json.Decode.Extra as D
import Maybe.Extra as Maybe
import Measures exposing (Measure)
import Numeral
import Objecthash.Value as V
import Parseable exposing (Parseable)



-- MODEL


type alias Model =
    { date : Parseable Date
    , name : String
    , batchSize : Parseable Measure
    , originalGravity : String
    , finalGravity : String
    }


abv : Model -> Maybe Float
abv model =
    -- Maybe.map2 (\og fg -> (og – fg) * 131.25)
    Maybe.map2 (\og fg -> (76.08 * (og - fg) / (1.775 - og)) * (fg / 0.794))
        (String.toFloat model.originalGravity)
        (String.toFloat model.finalGravity)


init : Date.Date -> Model
init date =
    { date = Parseable.fromData date
    , name = ""
    , batchSize = Measures.fromString "5 gallons"
    , originalGravity = ""
    , finalGravity = ""
    }


decoder : D.Decoder Model
decoder =
    D.map5 Model
        (D.map Date.fromString (D.field "date" D.string))
        (D.field "name" D.string)
        (D.map Measures.fromString (D.field "batchSize" D.string))
        (D.withDefault "" (D.field "originalGravity" D.string))
        (D.withDefault "" (D.field "finalGravity" D.string))


toObjecthashValue : Model -> Maybe V.Value
toObjecthashValue model =
    Maybe.map2
        (\date batchSize ->
            [ ( "date", V.string date )
            , ( "name", V.string model.name )
            , ( "batchSize", V.string batchSize )
            , ( "originalGravity", V.string model.originalGravity )
            , ( "finalGravity", V.string model.finalGravity )
            ]
                |> Dict.fromList
                |> V.dict
        )
        (Date.toString (Date.parse model.date))
        (Measures.toString (Measures.parse model.batchSize))


getDate : Model -> Maybe Date.Date
getDate model =
    Parseable.getData model.date



-- UPDATE


type Msg
    = ChangeDate String
    | ChangeName String
    | ChangeBatchSize String
    | ChangeOG String
    | ChangeFG String
    | ParseDate
    | UnparseDate
    | ParseBatchSize
    | UnparseBatchSize


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeDate s ->
            { model | date = Parseable.unparsed s }

        ChangeOG s ->
            { model | originalGravity = s }

        ChangeFG s ->
            { model | finalGravity = s }

        ParseDate ->
            { model | date = Date.parse model.date }

        UnparseDate ->
            { model | date = Date.unparse model.date }

        ParseBatchSize ->
            { model | batchSize = Measures.parse model.batchSize }

        UnparseBatchSize ->
            { model | batchSize = Measures.unparse model.batchSize }

        ChangeName name ->
            { model | name = name }

        ChangeBatchSize batchSize ->
            { model | batchSize = Parseable.unparsed batchSize }



-- VIEW


view : Model -> Element Msg
view model =
    row [ spacing 36, Font.size 30, Font.bold, width fill ]
        [ viewLeft model
        , viewRight model
        ]


viewLeft : Model -> Element Msg
viewLeft model =
    column [ spacing -6, Font.size 30, Font.bold, width fill ]
        [ viewDate model.date
        , viewName model.name
        , viewBatchSize model.batchSize
        ]


viewRight : Model -> Element Msg
viewRight model =
    column [ spacing -6, Font.size 30, Font.bold, width fill ]
        [ I.text inputAttrs
            { onChange = ChangeOG
            , text = model.originalGravity
            , placeholder = Just (I.placeholder [ moveLeft 7 ] (text "1.070"))
            , label = I.labelLeft inputAttrs (text "OG:")
            }
        , I.text inputAttrs
            { onChange = ChangeFG
            , text = model.finalGravity
            , placeholder = Just (I.placeholder [ moveLeft 7 ] (text "1.010"))
            , label = I.labelLeft inputAttrs (text "FG:")
            }
        , row []
            [ el inputAttrs (text "ABV:")
            , el inputAttrs (text (abv model |> Maybe.unwrap "-" ((++) "%" << Numeral.format "0,0.00")))
            ]
        ]


viewDate : Parseable Date -> Element Msg
viewDate date =
    I.text
        (Events.onFocus UnparseDate :: Events.onLoseFocus ParseDate :: inputAttrs ++ checkDate date)
        { onChange = ChangeDate
        , text = Parseable.format (Date.format False) date
        , placeholder =
            Just (I.placeholder [ moveLeft 7 ] (text "January 1st, 1970 "))
        , label = I.labelHidden ""
        }


checkDate : Parseable Date -> List (Attribute Msg)
checkDate date =
    if Date.isError date then
        [ Font.color (rgb 1 0 0) ]

    else
        []


viewName : String -> Element Msg
viewName name =
    I.text
        inputAttrs
        { onChange = ChangeName
        , text = name
        , placeholder = Just (I.placeholder [ moveLeft 7 ] (text "Russian Imperial Stout"))
        , label = I.labelHidden ""
        }


viewBatchSize : Parseable Measure -> Element Msg
viewBatchSize batchSize =
    I.text
        (Events.onFocus UnparseBatchSize :: Events.onLoseFocus ParseBatchSize :: inputAttrs ++ checkBatchSize batchSize)
        { onChange = ChangeBatchSize
        , text = Parseable.format Measures.format batchSize
        , placeholder = Just (I.placeholder [ moveLeft 7 ] (text "5 gallons"))
        , label = I.labelHidden ""
        }


checkBatchSize : Parseable Measure -> List (Attribute Msg)
checkBatchSize batchSize =
    if Measures.isError batchSize then
        [ Font.color (rgb 1 0 0) ]

    else
        []


inputAttrs : List (Attribute Msg)
inputAttrs =
    [ Border.width 0
    , padding 7
    , moveLeft 7
    , Background.color (rgba 1 1 1 0)
    ]
