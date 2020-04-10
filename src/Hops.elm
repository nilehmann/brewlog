module Hops exposing (Model, Msg, init, update, view)

import Array exposing (Array)
import Element exposing (..)
import Element.Input as I



-- Init


init : Model
init =
    Array.fromList
        [ Item "60 min" "1 ounce fuggels hops"
        , Item "30 min" "1 1/2 ounce hallertau hops"
        , Item "0 min" "Irish moss"
        ]



-- Model


type alias Model =
    Array Item


type alias Item =
    { time : String
    , descr : String
    }



-- Update


type Msg
    = ChangeTime Int String
    | ChangeDescr Int String
    | Add


update : Msg -> Model -> Model
update msg items =
    case msg of
        ChangeDescr idx descr ->
            case Array.get idx items of
                Nothing ->
                    items

                Just item ->
                    Array.set idx { item | descr = descr } items

        ChangeTime idx time ->
            case Array.get idx items of
                Nothing ->
                    items

                Just item ->
                    Array.set idx { item | time = time } items

        Add ->
            Array.push (Item "" "") items



-- View


view : Model -> Element Msg
view items =
    let
        itemViews =
            List.map itemView (Array.toIndexedList items)
    in
    column [ spacing 10, width fill ]
        (itemViews ++ [ addItemView ])


addItemView =
    I.button [ centerX ] { onPress = Just Add, label = text "Add" }


itemView : ( Int, Item ) -> Element Msg
itemView ( idx, ingredient ) =
    row [ spacing 10 ]
        [ I.text [ width (px 120), alignTop ]
            { onChange = ChangeTime idx
            , text = ingredient.time
            , placeholder = Nothing
            , label = I.labelHidden "Item time"
            }
        , I.multiline [ alignTop ]
            { onChange = ChangeDescr idx
            , text = ingredient.descr
            , placeholder = Nothing
            , label = I.labelHidden "Item descr"
            , spellcheck = True
            }
        ]
