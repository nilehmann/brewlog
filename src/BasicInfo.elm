module BasicInfo exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Element.Input as I



-- MODEL


type alias Model =
    { date : String
    , name : String
    , batchSize : String
    }


init : Model
init =
    { date = "February 30, 2018"
    , name = "Grizzly Beer Ale"
    , batchSize = "5 gallons (19l)"
    }



-- UPDATE


type Msg
    = ChangeDate String
    | ChangeName String
    | ChangeBatchSize String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeDate date ->
            { model | date = date }

        ChangeName name ->
            { model | name = name }

        ChangeBatchSize batchSize ->
            { model | batchSize = batchSize }



-- VIEW


view : Model -> Element Msg
view model =
    column [ spacing 10 ]
        [ infoView model.date ChangeDate
        , infoView model.name ChangeName
        , infoView model.batchSize ChangeBatchSize
        ]


infoView text onChange =
    I.multiline [ alignTop ]
        { onChange = onChange
        , text = text
        , placeholder = Nothing
        , label = I.labelHidden ""
        , spellcheck = True
        }
