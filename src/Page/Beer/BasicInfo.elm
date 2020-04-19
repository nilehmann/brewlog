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
import Objecthash.Value as V
import Parseable exposing (Parseable)



-- MODEL


type alias Model =
    { date : Parseable Date
    , name : String
    , batchSize : String
    }


init : Date.Date -> Model
init date =
    { date = Parseable.fromData date
    , name = ""
    , batchSize = ""
    }


decoder : D.Decoder Model
decoder =
    D.map3 Model
        (D.map Date.fromString (D.field "date" D.string))
        (D.field "name" D.string)
        (D.field "batchSize" D.string)


toObjecthashValue : Model -> Maybe V.Value
toObjecthashValue model =
    Date.parse model.date
        |> Date.toString
        |> Maybe.map
            (\d ->
                [ ( "date", V.string d )
                , ( "name", V.string model.name )
                , ( "batchSize", V.string model.batchSize )
                ]
                    |> Dict.fromList
                    |> V.dict
            )


getDate : Model -> Maybe Date.Date
getDate model =
    Parseable.getData model.date



-- UPDATE


type Msg
    = ChangeDate String
    | ChangeName String
    | ChangeBatchSize String
    | ParseDate
    | UnparseDate


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeDate s ->
            { model | date = Parseable.unparsed s }

        ParseDate ->
            { model | date = Date.parse model.date }

        UnparseDate ->
            { model | date = Date.unparse model.date }

        ChangeName name ->
            { model | name = name }

        ChangeBatchSize batchSize ->
            { model | batchSize = batchSize }



-- VIEW


view : Model -> Element Msg
view model =
    column [ spacing -6, Font.size 30, Font.bold ]
        [ el [ moveUp 0 ] (viewDate model.date)
        , el [ moveUp 0 ] (viewInfo model.name "American Ale" ChangeName)
        , el [ moveUp 0 ] (viewInfo model.batchSize "5 gallons" ChangeBatchSize)
        ]


formatDate : Parseable Date -> String
formatDate =
    Parseable.format (Date.format False)


viewDate : Parseable Date -> Element Msg
viewDate date =
    I.text
        (Events.onFocus UnparseDate :: Events.onLoseFocus ParseDate :: inputAttrs ++ checkDate date)
        { onChange = ChangeDate
        , text = formatDate date
        , placeholder =
            Just
                (I.placeholder [ moveLeft 7 ]
                    (text "January 1st, 1970 ")
                )
        , label = I.labelHidden ""
        }


checkDate : Parseable Date -> List (Attribute Msg)
checkDate date =
    if Parseable.isError date then
        [ Font.color (rgb 1 0 0), Font.underline ]

    else
        []


viewInfo : String -> String -> (String -> Msg) -> Element Msg
viewInfo info placeholder onChange =
    I.text
        inputAttrs
        { onChange = onChange
        , text = info
        , placeholder = Just (I.placeholder [ moveLeft 7 ] (text placeholder))
        , label = I.labelHidden ""
        }


inputAttrs : List (Attribute Msg)
inputAttrs =
    [ Border.width 0
    , padding 7
    , moveLeft 7
    , Background.color (rgba 1 1 1 0)
    ]
