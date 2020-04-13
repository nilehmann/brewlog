module Api exposing (DocPutResult, ViewResult, documentPut, view)

import Http
import Json.Decode as D
import Json.Encode as E
import Maybe.Extra as Maybe


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
        { url = baseUrl ++ "/_design/beers/_view/basicInfo"
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
    Http.request
        { method = "PUT"
        , headers = []
        , url = baseUrl ++ "/" ++ id ++ Maybe.unwrap "" (\r -> "?rev=" ++ r) rev
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
