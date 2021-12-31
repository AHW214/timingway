module Timingway.Mech exposing
    ( Mech
    , decoder
    , displaySeconds
    , isExpired
    , tick
    , view
    )

import Css
import Css.Colors as Colors
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Json.Decode as Decode exposing (Decoder)
import Round
import Timingway.Util.Basic as Basic
import Timingway.Config exposing (ViewConfig, GroupConfig)

type alias Mech =
    { attackName : String
    , resolveType : String
    , optionalNotes : Maybe String
    , millisLeft : Int
    }

tick : Int -> Mech -> Mech
tick delta mech =
    { mech | millisLeft = mech.millisLeft - delta }

barWidth : Float
barWidth = 45

isExpired : Mech -> Bool
isExpired { millisLeft } =
    millisLeft <= 0


decoder : Decoder Mech
decoder =
    Decode.map4 Mech
        (Decode.field "attackName" Decode.string)
        (Decode.field "resolveType" Decode.string)
        (Decode.field "notes" <| Decode.maybe Decode.string)
        (Decode.field "millisLeft" Decode.int)

-- VIEW

view : ViewConfig -> GroupConfig -> Mech -> Html msg
view viewConfig groupConfig mech =
    let
        cssCommon =
            [ Css.maxWidth Css.fitContent
            , Css.marginBottom <| Css.rem 1.5
            ]

        cssOutline =
            if groupConfig.isFocus then
                [ Css.paddingTop <| Css.rem 2
                , Css.paddingBottom <| Css.rem 2
                ]
            else
                []

        name = [ viewName groupConfig mech ]

        optionalNotes = Basic.maybe [] List.singleton <| maybeViewNotes mech
    in
        Html.div
            [ Html.css
                (cssCommon ++ cssOutline)
            ]
            [ Html.div
                [ Html.css
                    [ Css.displayFlex
                    , Css.justifyContent Css.spaceBetween
                    , Css.marginBottom <| Css.rem 1
                    ]
                ]
                (name ++ optionalNotes)
            , viewBar viewConfig groupConfig mech
            ]


viewBar : ViewConfig -> GroupConfig -> Mech -> Html msg
viewBar viewConfig groupConfig { resolveType, millisLeft } =
    let
        barHeight =
            Css.rem <| Basic.choose groupConfig.isFocus 8 5

        barFont =
            Css.rem <| Basic.choose groupConfig.isFocus 2.5 2

        barMargin =
            Css.rem <| Basic.choose groupConfig.isFocus 2 1

    in
        Html.div
            [ Html.css
                [ Css.position Css.relative
                , Css.backgroundImage <| Css.linearGradient2 Css.toBottom (Css.stop2 viewConfig.backgroundColor <| Css.pct 80) (Css.stop <| Css.rgba 55 55 55 0.5) []
                , Css.borderRadius <| Css.rem 0.5
                , Css.width <| Css.rem barWidth
                , Css.height barHeight
                ]
            ]
            [ Html.div
                [ Html.css
                    [ Css.position Css.absolute
                    , Css.borderRadius <| Css.rem 0.5
                    , Css.backgroundImage <| Css.linearGradient2 Css.toBottom (Css.stop2 groupConfig.barColor <| Css.pct 20) (Css.stop <| groupConfig.barGradient) []
                    , let
                        percentLeft =
                            100 - computePercent viewConfig.millisTotal millisLeft
                    in
                    Css.width <| Css.rem <| barWidth/100 * percentLeft
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
                    , Css.width <| Css.rem <| barWidth - 1
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
                            millisLeft
                                |> displaySeconds ( Basic.choose ( millisLeft < viewConfig.millisTotal ) 1 0 )
                    in
                    Html.text <| time
                    ]
                ]
            ]

maybeViewNotes : Mech -> Maybe (Html msg)
maybeViewNotes { optionalNotes } =
    let
        viewNotes notes =
            Html.div
                [ Html.css
                    [ Css.display Css.inlineBlock
                    , Css.alignSelf Css.flexEnd
                    , Css.color Colors.white
                    , Css.textAlign Css.left
                    , Css.fontSize <| Css.rem 1.5
                    , Css.maxWidth <| Css.rem 12
                    , Css.overflow Css.hidden
                    , Css.textOverflow Css.ellipsis
                    , Css.whiteSpace Css.noWrap
                    ]
                ]
                [ Html.text notes
                ]
    in
        Maybe.map viewNotes optionalNotes


viewName : GroupConfig -> Mech -> Html msg
viewName groupConfig { attackName, optionalNotes } =
    let
        barFont =
            Css.rem <| Basic.choose groupConfig.isFocus 3 2.5

        maxWidth =
            Css.rem <| Basic.maybe 40 (always 25) optionalNotes
    in
        Html.div
            [ Html.css
                [ Css.display Css.inlineBlock
                , Css.fontWeight Css.bold
                , Css.alignSelf Css.flexEnd
                , Css.color Colors.white
                , Css.textAlign Css.left
                , Css.fontSize barFont
                , Css.maxWidth maxWidth
                , Css.overflow Css.hidden
                , Css.textOverflow Css.ellipsis
                , Css.whiteSpace Css.noWrap
                ]
            ]
            [ Html.text attackName
            ]


{-| Display the given number of milliseconds rounded down to the nearest number
of seconds.
-}
displaySeconds : Int -> Int -> String
displaySeconds roundTo millis =
    let
        seconds =
            max 0 (toFloat millis / 1000)
        adjustedRoundTo = Basic.choose ( seconds == 0 ) 0 roundTo
        roundedString =
            case adjustedRoundTo of
               0 -> truncate seconds |> String.fromInt
               _ -> Round.round adjustedRoundTo seconds
    in
        roundedString ++ "s"


{-| Compute the percentage `current` constitutes of `total`. The result is
clamped between 0 and 100.
-}
computePercent : Int -> Int -> Float
computePercent total current =
    100 * clamp 0 1 (toFloat current / toFloat total)
