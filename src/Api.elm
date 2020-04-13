module Api exposing
    ( DocPutResult
    , DocResult
    , ViewResult
    , document
    , documentPut
    , view
    )

import Http
import Json.Decode as D
import Json.Encode as E
import Maybe.Extra as Maybe
import Url
import Url.Builder exposing (crossOrigin, string)


baseUrl =
    "http://localhost:5984/brewlog"



-- VIEW


type alias ViewResult a =
    { offset : Int
    , totalRows : Int
    , rows : List (ViewRow a)
    }


type alias ViewRow a =
    { id : String
    , key : String
    , value : a
    }


view :
    (Result Http.Error (ViewResult a) -> msg)
    -> D.Decoder a
    -> Cmd msg
view f rowDecoder =
    Http.get
        { url = crossOrigin baseUrl [ "_design", "beers", "_view", "basicInfo" ] []
        , expect =
            Http.expectJson
                f
                (D.map3 ViewResult
                    (D.field "offset" D.int)
                    (D.field "total_rows" D.int)
                    (D.field "rows"
                        (D.list
                            (D.map3 ViewRow
                                (D.field "id" D.string)
                                (D.field "key" D.string)
                                (D.field "value" rowDecoder)
                            )
                        )
                    )
                )
        }


type alias DocPutResult =
    { id : String
    , ok : Bool
    , rev : String
    }


documentPut :
    (Result Http.Error DocPutResult -> msg)
    -> String
    -> Maybe String
    -> E.Value
    -> Cmd msg
documentPut f id rev body =
    let
        queryParams =
            rev |> Maybe.map (string "rev") |> Maybe.toList
    in
    Http.request
        { method = "PUT"
        , headers = []
        , url = crossOrigin baseUrl [ id ] queryParams
        , body = Http.jsonBody body
        , expect =
            Http.expectJson
                f
                (D.map3 DocPutResult
                    (D.field "id" D.string)
                    (D.field "ok" D.bool)
                    (D.field "rev" D.string)
                )
        , tracker = Nothing
        , timeout = Nothing
        }


type alias DocResult a =
    { id : String
    , rev : String
    , doc : a
    }


document :
    (Result Http.Error (DocResult a) -> msg)
    -> D.Decoder a
    -> String
    -> Cmd msg
document f docDecoder id =
    Http.get
        { url = crossOrigin baseUrl [ id ] []
        , expect =
            Http.expectJson f
                (D.map3 DocResult
                    (D.field "_id" D.string)
                    (D.field "_rev" D.string)
                    docDecoder
                )
        }
