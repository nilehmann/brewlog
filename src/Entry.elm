module Entry exposing (Entry, EntryField, viewEntries)

import Element exposing (..)
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as I
import Maybe.Extra as Maybe


type alias Entry msg =
    { onRemove : Int -> msg
    , left : EntryField msg
    , right : EntryField msg
    }


type alias EntryField msg =
    { text : String
    , placeholder : String
    , error : Bool
    , onChange : Int -> String -> msg
    , onFocus : Maybe (Int -> msg)
    , onLoseFocus : Maybe (Int -> msg)
    }


viewEntries : List (Entry msg) -> Element msg
viewEntries entries =
    column [ spacing 6, width fill ]
        (List.indexedMap viewEntry entries)


viewEntry :
    Int
    -> Entry msg
    -> Element msg
viewEntry idx entry =
    row [ spacing 10 ]
        [ I.button
            [ alignTop, moveDown 5 ]
            { onPress = Just (entry.onRemove idx)
            , label =
                image [ width (px 15), height (px 15) ]
                    { src = "/assets/delete-32x32.png"
                    , description = ""
                    }
            }
        , I.text
            (width (px 120) :: inputAttributes idx entry.left)
            { onChange = entry.left.onChange idx
            , text = entry.left.text
            , placeholder = Just (I.placeholder [] (text entry.left.placeholder))
            , label = I.labelHidden ""
            }
        , I.multiline (inputAttributes idx entry.right)
            { onChange = entry.right.onChange idx
            , text = entry.right.text
            , placeholder = Just (I.placeholder [] (text entry.right.placeholder))
            , label = I.labelHidden ""
            , spellcheck = True
            }
        ]


inputAttributes : Int -> EntryField msg -> List (Attribute msg)
inputAttributes idx entryField =
    let
        onFocus =
            Just Events.onFocus
                |> Maybe.andMap (Maybe.andMap (Just idx) entryField.onFocus)

        onLoseFocus =
            Just Events.onLoseFocus
                |> Maybe.andMap (Maybe.andMap (Just idx) entryField.onLoseFocus)

        error =
            if entryField.error then
                [ Font.color (rgb 1 0 0), Font.underline ]

            else
                []
    in
    [ alignTop, Border.width 0, padding 4, moveLeft 4, spacing 9 ] ++ Maybe.toList onFocus ++ Maybe.toList onLoseFocus ++ error
