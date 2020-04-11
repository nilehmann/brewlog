module Hops exposing (Model, Msg, init, toObjecthashValue, update, view)

import Array exposing (Array)
import Array.Extra as Array
import Dict
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as I
import Objecthash.Value as V



-- INIT


init : Model
init =
    Array.fromList
        [ Item "60 min" "1 ounce fuggels hops"
        , Item "30 min" "1 1/2 ounce hallertau hops"
        , Item "0 min" "Irish moss"
        ]



-- MODEL


type alias Model =
    Array Item


type alias Item =
    { time : String
    , descr : String
    }


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
        itemViews =
            Array.indexedMapToList itemView items
    in
    column [ spacing 6, width fill ]
        ([ headerView ] ++ itemViews ++ [ addItemView ])


headerView =
    el [ Font.size 30, height (px 40) ] (text "Hop Schedule")


addItemView =
    I.button [ centerX, height (px 40) ]
        { onPress = Just Add
        , label = text "Add"
        }


itemView : Int -> Item -> Element Msg
itemView idx ingredient =
    row [ spacing 10 ]
        [ I.button
            [ alignTop, moveDown 5 ]
            { onPress = Just (Remove idx)
            , label =
                image [ width (px 15), height (px 15) ]
                    { src = "/assets/delete-32x32.png"
                    , description = ""
                    }
            }
        , I.text
            [ width (px 120), alignTop, Border.width 0, padding 4, moveLeft 4 ]
            { onChange = ChangeTime idx
            , text = ingredient.time
            , placeholder = Nothing
            , label = I.labelHidden "Item time"
            }
        , I.multiline [ alignTop, Border.width 0, padding 4, moveLeft 4 ]
            { onChange = ChangeDescr idx
            , text = ingredient.descr
            , placeholder = Nothing
            , label = I.labelHidden "Item descr"
            , spellcheck = True
            }
        ]
