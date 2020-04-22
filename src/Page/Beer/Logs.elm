module Page.Beer.Logs exposing
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
import Date exposing (Date)
import DateTime exposing (DateTime)
import Dict
import Element exposing (..)
import Entry exposing (Entry)
import Field exposing (Field)
import Json.Decode as D
import Maybe.Extra as Maybe
import Objecthash.Value as V
import Task
import Time exposing (Posix)



-- INIT


init : Model
init =
    Array.fromList
        []



-- MODEL


type alias Model =
    Array LogEntry


type alias LogEntry =
    { time : Field DateTime
    , descr : String
    }


decoder : D.Decoder Model
decoder =
    D.array
        (D.map2 LogEntry
            (D.map (Field.fromString Field.dateTime) (D.field "time" D.string))
            (D.field "descr" D.string)
        )


toObjecthashValue : Model -> Maybe V.Value
toObjecthashValue model =
    let
        f entry =
            Field.unwrap entry.time
                |> Maybe.map
                    (\time ->
                        [ ( "time", V.string time )
                        , ( "descr", V.string entry.descr )
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
                (\entry -> { entry | time = Field.update time entry.time })
                entries
            , Cmd.none
            )

        Add Nothing ->
            ( entries, Task.perform (Add << Just) Time.now )

        Add (Just time) ->
            ( Array.push
                (LogEntry (Field.fromData Field.dateTime (DateTime.fromPosix zone time)) "")
                entries
            , Cmd.none
            )

        Remove idx ->
            ( Array.removeAt idx entries, Cmd.none )

        UnparseTime idx ->
            ( Array.update idx
                (\entry -> { entry | time = Field.edit entry.time })
                entries
            , Cmd.none
            )

        ParseTime idx ->
            ( Array.update idx
                (\entry -> { entry | time = Field.parse entry.time })
                entries
            , Cmd.none
            )



-- VIEW


view : Model -> Maybe Date -> Element Msg
view logEntries batchDate =
    Entry.viewEntries
        "Logs"
        (Add Nothing)
        (Array.mapToList (logEntryToEntry batchDate) logEntries)


logEntryToEntry : Maybe Date -> LogEntry -> Entry Msg
logEntryToEntry batchDate logEntry =
    { onRemove = Remove
    , left =
        { text = formatTime batchDate logEntry.time
        , placeholder = "time"
        , error = Field.isError logEntry.time
        , onChange = ChangeTime
        , onFocus = Just UnparseTime
        , onLoseFocus = Just ParseTime
        }
    , right =
        { text = logEntry.descr
        , placeholder = "description"
        , error = False
        , onChange = ChangeDescr
        , onFocus = Nothing
        , onLoseFocus = Nothing
        }
    }


formatTime : Maybe Date -> Field DateTime -> String
formatTime batchDate entryTime =
    Field.format (DateTime.format batchDate) entryTime
