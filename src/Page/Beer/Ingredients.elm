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
import Entry exposing (Entry)
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
    Entry.viewEntries
        "Ingredients"
        Add
        (Array.mapToList ingredientToEntry ingredients)


ingredientToEntry : Ingredient -> Entry Msg
ingredientToEntry ingredient =
    { onRemove = Remove
    , left =
        { text = formatAmount ingredient.amount
        , error = Parseable.isError ingredient.amount
        , onChange = ChangeAmount
        , placeholder = "1.5 ounces"
        , onFocus = Just Unparse
        , onLoseFocus = Just Parse
        }
    , right =
        { text = ingredient.descr
        , error = False
        , onChange = ChangeDescr
        , placeholder = "Fuggles hops"
        , onFocus = Nothing
        , onLoseFocus = Nothing
        }
    }


formatAmount : Parseable Measure -> String
formatAmount amount =
    Parseable.format Measures.format amount
