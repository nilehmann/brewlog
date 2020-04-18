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
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as I
import Json.Decode as D
import Maybe.Extra as Maybe
import Objecthash.Value as V
import Parseable exposing (Parseable)
import Task
import Time exposing (Posix)



-- INIT


init : Model
init =
    Array.fromList
        []



-- MODEL


type alias Model =
    Array Entry


type alias Entry =
    { time : Parseable DateTime
    , descr : String
    }


decoder : D.Decoder Model
decoder =
    D.array
        (D.map2 Entry
            (D.map DateTime.fromString (D.field "time" D.string))
            (D.field "descr" D.string)
        )


toObjecthashValue : Model -> Maybe V.Value
toObjecthashValue model =
    let
        f entry =
            DateTime.parse entry.time
                |> DateTime.toString
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
                (\entry -> { entry | time = Parseable.unparsed time })
                entries
            , Cmd.none
            )

        Add Nothing ->
            ( entries, Task.perform (Add << Just) Time.now )

        Add (Just time) ->
            ( Array.push
                (Entry (Parseable.fromData (DateTime.fromPosix zone time)) "")
                entries
            , Cmd.none
            )

        Remove idx ->
            ( Array.removeAt idx entries, Cmd.none )

        UnparseTime idx ->
            ( Array.update idx
                (\entry -> { entry | time = DateTime.unparse entry.time })
                entries
            , Cmd.none
            )

        ParseTime idx ->
            ( Array.update idx
                (\entry -> { entry | time = DateTime.parse entry.time })
                entries
            , Cmd.none
            )



-- VIEW


view : Model -> Maybe Date -> Element Msg
view entries batchDate =
    let
        entryViews =
            List.map (viewEntry batchDate) (Array.toIndexedList entries)
    in
    column [ spacing 6, width fill ]
        (viewHeader :: entryViews ++ [ viewAddEntry ])


viewHeader : Element Msg
viewHeader =
    el [ Font.size 30, height (px 40) ] (text "Logs")


viewAddEntry : Element Msg
viewAddEntry =
    I.button [ centerX, height (px 40) ]
        { onPress = Just (Add Nothing)
        , label = text "Add"
        }


viewEntry : Maybe Date -> ( Int, Entry ) -> Element Msg
viewEntry batchDate ( idx, entry ) =
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


checkTime : Parseable DateTime -> List (Attribute Msg)
checkTime entryTime =
    if Parseable.isError entryTime then
        [ Font.color (rgb 1 0 0), Font.underline ]

    else
        []


inputAttrs : List (Attribute Msg)
inputAttrs =
    [ alignTop, Border.width 0, padding 4, moveLeft 4 ]


formatTime : Maybe Date -> Parseable DateTime -> String
formatTime batchDate entryTime =
    Parseable.format (DateTime.format batchDate) entryTime
