module Page.Beer.Hops exposing
    ( Model
    , Msg
    , decoder
    , init
    , toObjecthashValue
    , update
    , view
    )

import Array exposing (Array)
import Array.Extra as Array
import Dict
import Element exposing (..)
import Element.Font as Font
import Element.Input as I
import Entry exposing (Entry)
import Json.Decode as D
import Objecthash.Value as V



-- INIT


init : Model
init =
    Array.fromList
        []



-- MODEL


type alias Model =
    Array Item


type alias Item =
    { time : String
    , descr : String
    }


decoder : D.Decoder Model
decoder =
    D.array
        (D.map2 Item
            (D.field "time" D.string)
            (D.field "descr" D.string)
        )


toObjecthashValue : Model -> V.Value
toObjecthashValue model =
    let
        f item =
            [ ( "time", V.string item.time )
            , ( "descr", V.string item.descr )
            ]
                |> Dict.fromList
                |> V.dict
    in
    V.list (Array.mapToList f model)



-- UPDATE


type Msg
    = ChangeTime Int String
    | ChangeDescr Int String
    | Remove Int
    | Add


update : Msg -> Model -> Model
update msg items =
    case msg of
        ChangeDescr idx descr ->
            Array.update idx
                (\item -> { item | descr = descr })
                items

        ChangeTime idx time ->
            Array.update idx
                (\item -> { item | time = time })
                items

        Add ->
            Array.push (Item "" "") items

        Remove idx ->
            Array.removeAt idx items



-- VIEW


view : Model -> Element Msg
view items =
    let
        entries =
            Array.mapToList itemToEntry items

        itemViews =
            Entry.viewEntries entries
    in
    column [ spacing 6, width fill ]
        [ viewHeader, itemViews, viewAddItem ]


itemToEntry : Item -> Entry Msg
itemToEntry item =
    { onRemove = Remove
    , left =
        { text = item.time
        , onChange = ChangeTime
        }
    , right =
        { text = item.descr
        , onChange = ChangeDescr
        }
    }


viewHeader : Element Msg
viewHeader =
    el [ Font.size 30, height (px 40) ] (text "Hop Schedule")


viewAddItem : Element Msg
viewAddItem =
    I.button [ centerX, height (px 40) ]
        { onPress = Just Add
        , label = text "Add"
        }
