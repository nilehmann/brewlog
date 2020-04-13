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

import DateTime
import Dict
import Element exposing (..)
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as I
import Json.Decode as D
import Objecthash.Value as V



-- MODEL


type alias Model =
    { date : Date
    , name : String
    , batchSize : String
    }


type Date
    = ParsedDate DateTime.Date
    | UnparsedDate String
    | ParseErrorDate String


init : Model
init =
    { date = UnparsedDate ""
    , name = ""
    , batchSize = ""
    }


decoder : D.Decoder Model
decoder =
    D.map3 Model
        (D.map (parse << UnparsedDate) (D.field "date" D.string))
        (D.field "name" D.string)
        (D.field "batchSize" D.string)


toObjecthashValue : Model -> Maybe V.Value
toObjecthashValue model =
    case parse model.date of
        ParsedDate d ->
            [ ( "date", V.string (DateTime.unparseDate d) )
            , ( "name", V.string model.name )
            , ( "batchSize", V.string model.batchSize )
            ]
                |> Dict.fromList
                |> V.dict
                |> Just

        _ ->
            Nothing


getDate : Model -> Maybe DateTime.Date
getDate model =
    case model.date of
        UnparsedDate str ->
            DateTime.parseDate str

        ParsedDate date ->
            Just date

        ParseErrorDate _ ->
            Nothing


parse : Date -> Date
parse date =
    case date of
        UnparsedDate str ->
            DateTime.parseDate str
                |> Maybe.map ParsedDate
                |> Maybe.withDefault (ParseErrorDate str)

        _ ->
            date


unparse : Date -> Date
unparse date =
    case date of
        ParsedDate d ->
            UnparsedDate (DateTime.unparseDate d)

        _ ->
            date



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
            { model | date = UnparsedDate s }

        ParseDate ->
            { model | date = parse model.date }

        UnparseDate ->
            { model | date = unparse model.date }

        ChangeName name ->
            { model | name = name }

        ChangeBatchSize batchSize ->
            { model | batchSize = batchSize }



-- VIEW


view : Model -> Element Msg
view model =
    column [ spacing 4, Font.size 24, Font.bold ]
        [ dateView model.date
        , infoView model.name "American Ale" ChangeName
        , infoView model.batchSize "5 gallons" ChangeBatchSize
        ]


formatDate : Date -> String
formatDate date =
    case date of
        UnparsedDate s ->
            s

        ParseErrorDate s ->
            s

        ParsedDate d ->
            DateTime.formatDate False d


dateView date =
    I.text
        ([ alignTop
         , Border.width 0
         , padding 6
         , moveLeft 6
         , Events.onFocus UnparseDate
         , Events.onLoseFocus ParseDate
         ]
            ++ checkDate date
        )
        { onChange = ChangeDate
        , text = formatDate date
        , placeholder =
            Just
                (I.placeholder [ moveLeft 6 ]
                    (text "January 1st, 1970 ")
                )
        , label = I.labelHidden ""
        }


checkDate date =
    case date of
        ParseErrorDate _ ->
            [ Font.color (rgb 1 0 0), Font.underline ]

        _ ->
            []


infoView info placeholder onChange =
    I.text
        [ alignTop
        , Border.width 0
        , padding 6
        , moveLeft 6
        ]
        { onChange = onChange
        , text = info
        , placeholder = Just (I.placeholder [ moveLeft 6 ] (text placeholder))
        , label = I.labelHidden ""
        }
