module Entry exposing (Entry, EntryField, viewEntries)

import Element exposing (..)
import Element.Border as Border
import Element.Input as I


type alias Entry msg =
    { onRemove : Int -> msg
    , left : EntryField msg
    , right : EntryField msg
    }


type alias EntryField msg =
    { text : String
    , onChange : Int -> String -> msg

    -- , onFocus : Int -> msg
    -- , onLooseFocus : Int -> msg
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
            [ width (px 120), alignTop, Border.width 0, padding 4, moveLeft 4 ]
            { onChange = entry.left.onChange idx
            , text = entry.left.text
            , placeholder = Nothing
            , label = I.labelHidden "Item time"
            }
        , I.multiline [ alignTop, Border.width 0, padding 4, moveLeft 4 ]
            { onChange = entry.right.onChange idx
            , text = entry.right.text
            , placeholder = Nothing
            , label = I.labelHidden "Item descr"
            , spellcheck = True
            }
        ]
