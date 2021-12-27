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
    { colorBackround : Color
    , colorBar : Color
    , colorOutline : Maybe Color
    , millisTotal : Int
    }


view : Config -> Field -> Html msg
view config field =
    let
        cssCommon =
            [ Css.width <| Css.pct 50
            , Css.marginBottom <| Css.pct 3
            ]

        cssOutline =
            case config.colorOutline of
                Nothing ->
                    []

                Just color ->
                    [ Css.border3 (Css.px 5) Css.outset color
                    , Css.paddingTop <| Css.pct 2
                    , Css.paddingBottom <| Css.pct 2
                    ]
    in
    Html.div
        [ Html.css
            (cssCommon ++ cssOutline)
        ]
        [ viewName field
        , viewBar config field
        ]


viewBar : Config -> Field -> Html msg
viewBar config { resolveType, millisLeft } =
    Html.div
        [ Html.css
            [ Css.position Css.relative
            , Css.backgroundColor config.colorBackround
            , Css.margin Css.auto
            , Css.borderRadius <| Css.rem 0.5
            , Css.border3 (Css.px 1) Css.solid Colors.white
            , Css.boxShadow3
                (Css.px 0.5)
                (Css.px 0.5)
                (Css.rgba 50 50 50 0.8)
            , Css.width <| Css.pct 90
            , Css.height <| Css.em 4
            , Css.paddingRight <| Css.rem 1
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
                  Css.width <| Css.pct percentLeft
                , Css.height <| Css.em 4
                ]
            ]
            []
        , Html.div
            [ Html.css
                [ Css.position Css.absolute
                , Css.width <| Css.pct 95
                , Css.color Colors.white
                , Css.textAlign Css.left
                , Css.fontSize <| Css.rem 2
                , Css.marginLeft <| Css.rem 1
                , Css.marginTop <| Css.rem 0.75
                ]
            ]
            [ Html.div
                []
                [ let
                    time =
                        "(" ++ displaySeconds millisLeft ++ ")"
                  in
                  Html.text <| resolveType ++ " " ++ time
                ]
            ]
        ]


viewName : Field -> Html msg
viewName { attackName } =
    Html.div
        [ Html.css
            [ Css.color Colors.white
            , Css.textAlign Css.left
            , Css.fontSize <| Css.pct 200
            , Css.marginLeft <| Css.pct 5
            , Css.marginBottom <| Css.pct 1
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
            max 0 (millis // 1000)
    in
    String.fromInt seconds ++ "s"


{-| Compute the percentage `current` constitutes of `total`. The result is
clamped between 0 and 100.
-}
computePercent : Int -> Int -> Float
computePercent total current =
    100 * clamp 0 1 (toFloat current / toFloat total)
