module Timingway.Board exposing (Board, new)

import List.Extra as List
import Timingway.Mech as Mech exposing (Mech)
import Timingway.Util.Basic as Basic


type alias Board =
    { config : Config
    , mechsFuture : List Mech
    , mechsPast : List (Maybe Mech)
    , mechsUpcoming : List Mech
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
--         mechs =
--     in


new : Config -> List Mech -> Board
new config mechs =
    let
        ( mechsExpired, mechsActive ) =
            List.partition Mech.isExpired mechs

        mechsPast =
            mechsExpired
                |> List.map Just
                |> Basic.padLeft config.numPast Nothing

        ( mechsUpcoming, mechsFuture ) =
            List.splitAt config.numUpcoming mechsActive
    in
    Board config mechsFuture mechsPast mechsUpcoming
