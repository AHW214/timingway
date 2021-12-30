module Timingway.Board exposing (Board, new)

import List.Extra as List
import Timingway.Field as Field exposing (Field)
import Timingway.Util.Basic as Basic


type alias Board =
    { config : Config
    , fieldsFuture : List Field
    , fieldsPast : List (Maybe Field)
    , fieldsUpcoming : List Field
    }


type alias Config =
    { numFuture : Int
    , numPast : Int
    , numUpcoming : Int
    , totalMillis : Int
    }



-- tick : Int -> Board -> Board
-- tick delta board =
--     let
--         fields =
--     in


new : Config -> List Field -> Board
new config fields =
    let
        ( fieldsExpired, fieldsActive ) =
            List.partition Field.isExpired fields

        fieldsPast =
            fieldsExpired
                |> List.map Just
                |> Basic.padLeft config.numPast Nothing

        ( fieldsUpcoming, fieldsFuture ) =
            List.splitAt config.numUpcoming fieldsActive
    in
    Board config fieldsFuture fieldsPast fieldsUpcoming
