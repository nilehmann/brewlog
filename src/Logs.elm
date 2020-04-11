module Logs exposing (Model, Msg, init, update, view)

import Array exposing (Array)
import Array.Extra as Array
import DateTime exposing (DateTime)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as I
import Task
import Time exposing (Posix)



-- INIT


init : Model
init =
    Array.fromList
        [ Entry (UnparsedTime "9:30") "Start Heating"
        , Entry (UnparsedTime "10:00") "Start Boil"
        , Entry (UnparsedTime "11:00") "Temp read 35"
        , Entry (UnparsedTime "21:44") "Pitch yeast"
        ]



-- MODEL


type alias Model =
    Array Entry


type alias Entry =
    { time : EntryTime
    , descr : String
    }


type EntryTime
    = ParsedTime DateTime
    | UnparsedTime String
    | ParseErrorTime String


unparse entryTime =
    case entryTime of
        ParsedTime dateTime ->
            UnparsedTime (DateTime.unparse dateTime)

        _ ->
            entryTime


parse : EntryTime -> EntryTime
parse entryTime =
    case entryTime of
        UnparsedTime str ->
            DateTime.parse str
                |> Maybe.map ParsedTime
                |> Maybe.withDefault (ParseErrorTime str)

        _ ->
            entryTime



-- UPDATE


type Msg
    = ChangeTime Int String
    | ChangeDescr Int String
    | Add (Maybe Posix)
    | UnparseTime Int
    | ParseTime Int
    | Remove Int


update : Time.Zone -> Msg -> Model -> ( Model, Cmd Msg )
update zone msg entries =
    case msg of
        ChangeDescr idx descr ->
            ( Array.update idx
                (\entry -> { entry | descr = descr })
                entries
            , Cmd.none
            )

        ChangeTime idx time ->
            ( Array.update idx
                (\entry -> { entry | time = UnparsedTime time })
                entries
            , Cmd.none
            )

        Add Nothing ->
            ( entries, Task.perform (Add << Just) Time.now )

        Add (Just time) ->
            ( Array.push
                (Entry (ParsedTime (DateTime.fromPosix zone time)) "")
                entries
            , Cmd.none
            )

        Remove idx ->
            ( Array.removeAt idx entries, Cmd.none )

        UnparseTime idx ->
            ( Array.update idx
                (\entry -> { entry | time = unparse entry.time })
                entries
            , Cmd.none
            )

        ParseTime idx ->
            ( Array.update idx
                (\entry -> { entry | time = parse entry.time })
                entries
            , Cmd.none
            )



-- VIEW


view : Model -> Maybe DateTime.Date -> Element Msg
view entries batchDate =
    let
        entryViews =
            List.map (entryView batchDate) (Array.toIndexedList entries)
    in
    column [ spacing 6, width fill ]
        ([ headerView ] ++ entryViews ++ [ addEntryView ])


headerView =
    el [ Font.size 30, height (px 40) ] (text "Logs")


addEntryView =
    I.button [ centerX, height (px 40) ]
        { onPress = Just (Add Nothing)
        , label = text "Add"
        }


entryView : Maybe DateTime.Date -> ( Int, Entry ) -> Element Msg
entryView batchDate ( idx, entry ) =
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
            (inputAttrs
                ++ [ width (px 0 |> minimum 120)
                   , Events.onFocus (UnparseTime idx)
                   , Events.onLoseFocus (ParseTime idx)
                   ]
                ++ checkTime entry.time
            )
            { onChange = ChangeTime idx
            , text = formatTime batchDate entry.time
            , placeholder = Nothing
            , label = I.labelHidden "Entry time"
            }
        , I.multiline inputAttrs
            { onChange = ChangeDescr idx
            , text = entry.descr
            , placeholder = Nothing
            , label = I.labelHidden "Entry descr"
            , spellcheck = True
            }
        ]


checkTime entryTime =
    case entryTime of
        ParseErrorTime _ ->
            [ Font.color (rgb 1 0 0), Font.underline ]

        _ ->
            []


inputAttrs =
    [ alignTop, Border.width 0, padding 4, moveLeft 4 ]


formatTime : Maybe DateTime.Date -> EntryTime -> String
formatTime batchDate entryTime =
    case entryTime of
        ParsedTime dateTime ->
            DateTime.format batchDate dateTime

        UnparsedTime str ->
            str

        ParseErrorTime str ->
            str
