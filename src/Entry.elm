module Entry exposing (Entry, EntryField, viewEntries)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as I
import IndieFlower
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


viewEntries : String -> msg -> List (Entry msg) -> Element msg
viewEntries title onAdd entries =
    column [ spacing -4, width fill ]
        [ el [ Font.size 30, height (px 40) ] (text title)
        , column [ spacing 9, width fill ]
            [ column [ spacing 2, width fill ]
                (List.indexedMap (viewEntry (maxWidth entries)) entries)
            , viewAddButton onAdd
            ]
        ]


maxWidth : List (Entry a) -> Int
maxWidth entries =
    -- List.maximum (List.map (IndieFlower.textWidth 24 << .text << .left) entries)
    -- List.maximum (List.map (.text << .left) entries ++ List.map (.placeholder << .left) entries)
    List.concatMap (\e -> [ e.left.text, e.left.placeholder ]) entries
        |> List.map (IndieFlower.textWidth 24)
        |> List.maximum
        |> Maybe.withDefault 0
        |> ceiling


viewEntry :
    Int
    -> Int
    -> Entry msg
    -> Element msg
viewEntry leftWidth idx entry =
    row [ spacing 10 ]
        [ I.button
            [ alignTop, moveDown 7 ]
            { onPress = Just (entry.onRemove idx)
            , label =
                image [ width (px 15), height (px 15) ]
                    { src = "/assets/delete-32x32.png"
                    , description = ""
                    }
            }
        , row
            [ spacing 32, width fill ]
            [ I.text
                (width (px leftWidth |> minimum 20) :: inputAttributes idx entry.left)
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
                [ Font.color (rgb 1 0 0) ]

            else
                []
    in
    [ alignTop
    , Border.width 0
    , paddingXY 0 6
    , spacing 14
    , Background.color (rgba 1 1 1 0)
    ]
        ++ Maybe.toList onFocus
        ++ Maybe.toList onLoseFocus
        ++ error


viewAddButton : msg -> Element msg
viewAddButton onAdd =
    I.button
        [ centerX ]
        { onPress = Just onAdd
        , label =
            image [ width (px 20), height (px 20) ]
                { src = "/assets/add-32.png"
                , description = ""
                }
        }
