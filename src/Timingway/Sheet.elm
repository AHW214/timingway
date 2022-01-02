module Timingway.Sheet exposing
    ( Row
    , decoder
    , displayError
    , makeQueryUrl
    )

import Array
import Http
import Json.Decode as Decode exposing (Decoder)
import Url.Builder as Url

type alias Row =
    { attack : String
    , notes : Maybe String
    , resolve : String
    , time : String
    }

decoder : Decoder (List Row)
decoder =
    Decode.map Array.toList
        <| Decode.array rowDecoder

rowDecoder : Decoder Row
rowDecoder =
    Decode.map4 Row
        (Decode.field "Attack" Decode.string)
        (Decode.maybe <| Decode.field "Notes" Decode.string)
        (Decode.field "Resolve" Decode.string)
        (Decode.field "Time" Decode.string)

makeQueryUrl : String -> String -> String
makeQueryUrl sheetId tabName =
    let
        domain = "https://opensheet.elk.sh"

        path = [ sheetId, tabName ]

        queryParams = []
    in
        Url.crossOrigin domain path queryParams

displayError : Http.Error -> String
displayError httpError =
    case httpError of
        Http.BadBody message ->
            "Response body was malformed: '" ++ message ++ "'"

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadUrl message ->
            "Request url was invalid: '" ++ message ++ "'"

        Http.NetworkError ->
            "Request failed due to a network error"

        Http.Timeout ->
            "Request failed timed out"
