module Ingredients exposing (Model, Msg, init, update, view)

import Array exposing (Array)
import Element exposing (..)
import Element.Input as I



-- Init


init : Model
init =
    Array.fromList
        [ Ingredient "5 pounds" "American extract malt"
        , Ingredient "1 pounds" "crystal malt"
        , Ingredient "2 teaspoons" "gypsum"
        , Ingredient "1 1/2 ounces" "fuggles hops"
        , Ingredient "1 1/2 ounces" "Hallertau hops"
        , Ingredient "1 1/2 ounce" "Got ucovered brad ale yeast"
        ]



-- Model


type alias Model =
    Array Ingredient


type alias Ingredient =
    { amount : String
    , descr : String
    }



-- Update


type Msg
    = ChangeDescr Int String
    | ChangeAmount Int String
    | Add


update : Msg -> Model -> Model
update msg ingredients =
    case msg of
        ChangeDescr idx descr ->
            case Array.get idx ingredients of
                Nothing ->
                    ingredients

                Just ingredient ->
                    Array.set idx { ingredient | descr = descr } ingredients

        ChangeAmount idx amount ->
            case Array.get idx ingredients of
                Nothing ->
                    ingredients

                Just ingredient ->
                    Array.set idx { ingredient | amount = amount } ingredients

        Add ->
            Array.push (Ingredient "" "") ingredients



-- View


view : Model -> Element Msg
view ingredients =
    let
        ingredientViews =
            List.map ingredientView (Array.toIndexedList ingredients)
    in
    column [ spacing 10, width fill ]
        (ingredientViews ++ [ addIngredientView ])


addIngredientView =
    I.button [ centerX ] { onPress = Just Add, label = text "Add" }


ingredientView : ( Int, Ingredient ) -> Element Msg
ingredientView ( idx, ingredient ) =
    row [ spacing 10 ]
        [ I.text [ width (px 120), alignTop ]
            { onChange = ChangeAmount idx
            , text = ingredient.amount
            , placeholder = Just (I.placeholder [] (text "1 pounds"))
            , label = I.labelHidden "Ingredient amount"
            }
        , I.multiline [ alignTop ]
            { onChange = ChangeDescr idx
            , text = ingredient.descr
            , placeholder = Nothing
            , label = I.labelHidden "Ingredient descr"
            , spellcheck = True
            }
        ]
