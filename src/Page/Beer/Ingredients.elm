module Page.Beer.Ingredients exposing
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
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as I
import Json.Decode as D
import Maybe.Extra as Maybe
import Measures exposing (Measure)
import Objecthash.Value as V
import Parseable exposing (Parseable)



-- INIT


init : Model
init =
    Array.fromList
        []



-- MODEL


type alias Model =
    Array Ingredient


type alias Ingredient =
    { amount : Parseable Measure
    , descr : String
    }


decoder : D.Decoder Model
decoder =
    D.array
        (D.map2
            Ingredient
            (D.map Measures.fromString (D.field "amount" D.string))
            (D.field "descr" D.string)
        )


toObjecthashValue : Model -> Maybe V.Value
toObjecthashValue model =
    let
        f ingredient =
            Measures.parse ingredient.amount
                |> Measures.toString
                |> Maybe.map
                    (\measure ->
                        [ ( "descr", V.string ingredient.descr )
                        , ( "amount", V.string measure )
                        ]
                            |> Dict.fromList
                            |> V.dict
                    )
    in
    Array.mapToList f model
        |> Maybe.combine
        |> Maybe.map V.list



-- UPDATE


type Msg
    = ChangeDescr Int String
    | ChangeAmount Int String
    | Unparse Int
    | Parse Int
    | Add
    | Remove Int


update : Msg -> Model -> Model
update msg ingredients =
    case msg of
        ChangeDescr idx descr ->
            Array.update idx
                (\ingr -> { ingr | descr = descr })
                ingredients

        ChangeAmount idx s ->
            Array.update idx
                (\ingr -> { ingr | amount = Parseable.unparsed s })
                ingredients

        Parse idx ->
            Array.update idx
                (\ingr -> { ingr | amount = Measures.parse ingr.amount })
                ingredients

        Unparse idx ->
            Array.update idx
                (\ingr -> { ingr | amount = Measures.unparse ingr.amount })
                ingredients

        Add ->
            Array.push (Ingredient (Measures.fromString "") "") ingredients

        Remove idx ->
            Array.append
                (Array.slice 0 idx ingredients)
                (Array.slice (idx + 1) (Array.length ingredients) ingredients)



-- VIEW


view : Model -> Element Msg
view ingredients =
    let
        ingredientViews =
            List.map viewIngredient (Array.toIndexedList ingredients)
    in
    column [ spacing 6, width fill ]
        (viewHeader :: ingredientViews ++ [ viewAddIngredient ])


viewHeader : Element Msg
viewHeader =
    el [ Font.size 30, height (px 40) ] (text "Ingredients")


viewAddIngredient : Element Msg
viewAddIngredient =
    I.button
        [ centerX, height (px 40) ]
        { onPress = Just Add
        , label = text "Add"
        }


viewIngredient : ( Int, Ingredient ) -> Element Msg
viewIngredient ( idx, ingredient ) =
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
            ([ width (px 120)
             , alignTop
             , Border.width 0
             , padding 4
             , Events.onFocus (Unparse idx)
             , Events.onLoseFocus (Parse idx)
             ]
                ++ checkAmount ingredient.amount
            )
            { onChange = ChangeAmount idx
            , text = formatAmount ingredient.amount
            , placeholder = Just (I.placeholder [] (text "1.5 ounces"))
            , label = I.labelHidden "Ingredient amount"
            }
        , I.multiline
            [ alignTop
            , Border.width 0
            , width fill
            , padding 4
            , moveLeft 4
            , spacing 9
            ]
            { onChange = ChangeDescr idx
            , text = ingredient.descr
            , placeholder = Just (I.placeholder [] (text "Fuggles hops"))
            , label = I.labelHidden "Ingredient descr"
            , spellcheck = True
            }
        ]


checkAmount : Parseable Measure -> List (Attribute Msg)
checkAmount amount =
    if Parseable.isError amount then
        [ Font.color (rgb 1 0 0), Font.underline ]

    else
        []


formatAmount : Parseable Measure -> String
formatAmount amount =
    Parseable.format Measures.format amount
