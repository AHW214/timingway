module Timingway.Config exposing (GroupConfig, ViewConfig)
import Css exposing (Color)

{-| GroupConfig is a record which determines the attributes of each group of mechanics.
-}
type alias GroupConfig =
    { amount : Int
    , barColor : Color
    , isFocus : Bool
    }

{-| ViewConfig is a record which determines each GroupConfig for the overall view.
-}
type alias ViewConfig =
    { future : GroupConfig
    , past : GroupConfig
    , present : GroupConfig
    , millisTotal : Int
    , backgroundColor : Color
    }