module Timingway.Field exposing
    ( Config
    , Field
    , decoder
    , isExpired
    , tick
    , view
    )

import Css exposing (Color)
import Css.Colors as Colors
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Json.Decode as Decode exposing (Decoder)
import Round
import Timingway.Util.List as List


type alias Field =
    { attackName : String
    , resolveType : String
    , millisLeft : Int
    }


tick : Int -> Field -> Field
tick delta field =
    { field | millisLeft = field.millisLeft - delta }


isExpired : Field -> Bool
isExpired { millisLeft } =
    millisLeft <= 0


decoder : Decoder Field
decoder =
    Decode.map3 Field
        (Decode.field "attackName" Decode.string)
        (Decode.field "resolveType" Decode.string)
        (Decode.field "millisLeft" Decode.int)



-- VIEW


type alias Config =
    { colorBackground : Color
    , colorBar : Color
    , isFocus : Bool
    , millisTotal : Int
    }


view : Config -> Field -> Html msg
view config field =
    let
        cssCommon =
            [ Css.marginBottom <| Css.rem 1
            , Css.marginLeft <| Css.rem 8
            ]

        cssOutline =
            if config.isFocus then
                [ Css.paddingTop <| Css.rem 1
                , Css.paddingBottom <| Css.rem 1
                ]
            else
                []
                
    in
        Html.div
            [ Html.css
                (cssCommon ++ cssOutline)
            ]
            [ viewName config field
            , viewBar config field
            ]


viewBar : Config -> Field -> Html msg
viewBar config { resolveType, millisLeft } =
    let
        barHeight =
            Css.rem <| List.choose config.isFocus 8 5
        
        barFont =
            Css.rem <| List.choose config.isFocus 3.5 2.5

        barMargin =
            Css.rem <| List.choose config.isFocus 1.5 1

    in
        Html.div
            [ Html.css
                [ Css.position Css.relative
                , Css.backgroundColor config.colorBackground
                , Css.borderRadius <| Css.rem 0.5
                , Css.width <| Css.rem 40
                , Css.height barHeight
                , Css.marginLeft <| Css.rem 10
                ]
            ]
            [ Html.div
                [ Html.css
                    [ Css.position Css.absolute
                    , Css.borderRadius <| Css.rem 0.5
                    , Css.backgroundColor config.colorBar
                    , let
                        percentLeft =
                            100 - computePercent config.millisTotal millisLeft
                    in
                    Css.width <| Css.rem <| 0.4 * percentLeft
                    , Css.height barHeight
                    ]
                ]
                []
            , Html.div
                [ Html.css
                    [ Css.position Css.absolute
                    , Css.width <| Css.pct 100
                    , Css.color Colors.white
                    , Css.textAlign Css.left
                    , Css.fontSize barFont
                    , Css.marginLeft <| Css.rem 1
                    , Css.marginTop barMargin
                    ]
                ]
                [ Html.div
                    []
                    [ 
                    Html.text <| resolveType
                    ]
                ]
                , Html.div
                [ Html.css
                    [ Css.position Css.absolute
                    , Css.width <| Css.rem 38
                    , Css.color Colors.white
                    , Css.textAlign Css.right
                    , Css.fontSize barFont
                    , Css.marginTop barMargin
                    ]
                ]
                [ Html.div
                    []
                    [ let
                        time =
                            displaySeconds millisLeft
                    in
                    Html.text <| time
                    ]
                ]
            ]


viewName : Config -> Field -> Html msg
viewName config { attackName } =
    let
        barFont =
            Css.rem <| List.choose config.isFocus 3 2.5
    in
        Html.div
            [ Html.css
                [ Css.color Colors.white
                , Css.textAlign Css.left
                , Css.fontSize barFont
                , Css.marginLeft <| Css.rem 10
                , Css.marginBottom <| Css.rem 1
                ]
            ]
            [ Html.text attackName
            ]


{-| Display the given number of milliseconds rounded down to the nearest number
of seconds.
-}
displaySeconds : Int -> String
displaySeconds millis =
    let
        seconds =
            max 0 (toFloat millis / 1000)
    in
    Round.round 1 seconds ++ "s"


{-| Compute the percentage `current` constitutes of `total`. The result is
clamped between 0 and 100.
-}
computePercent : Int -> Int -> Float
computePercent total current =
    100 * clamp 0 1 (toFloat current / toFloat total)
