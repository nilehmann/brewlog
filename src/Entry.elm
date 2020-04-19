module Entry exposing (Entry, Events, viewEntries)

import Array.Extra as Array
import Element exposing (..)
import Element.Border as Border
import Element.Input as I


type alias Entry =
    { left : String
    , right : String
    }


type alias Events msg =
    { onRemove : Int -> msg
    , onChangeLeft : Int -> String -> msg
    , onChangeRight : Int -> String -> msg
    }


viewEntries : Events msg -> List Entry -> Element msg
viewEntries events entries =
    column [ spacing 6, width fill ]
        (List.indexedMap (viewEntry events) entries)


viewEntry :
    Events msg
    -> Int
    -> Entry
    -> Element msg
viewEntry events idx entry =
    row [ spacing 10 ]
        [ I.button
            [ alignTop, moveDown 5 ]
            { onPress = Just (events.onRemove idx)
            , label =
                image [ width (px 15), height (px 15) ]
                    { src = "/assets/delete-32x32.png"
                    , description = ""
                    }
            }
        , I.text
            [ width (px 120), alignTop, Border.width 0, padding 4, moveLeft 4 ]
            { onChange = events.onChangeLeft idx
            , text = entry.left
            , placeholder = Nothing
            , label = I.labelHidden "Item time"
            }
        , I.multiline [ alignTop, Border.width 0, padding 4, moveLeft 4 ]
            { onChange = events.onChangeRight idx
            , text = entry.right
            , placeholder = Nothing
            , label = I.labelHidden "Item descr"
            , spellcheck = True
            }
        ]
