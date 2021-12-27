module Field exposing
  ( Field
  , decoder
  , decrement
  , view
  )

import Css exposing (Color)
import Css.Colors as Colors
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Json.Decode as Decode exposing (Decoder)

type alias Field =
  { attackName: String
  , resolveType: String
  , millisLeft : Int
  }

decrement : Int -> Field -> Field
decrement tick field =
  { field | millisLeft = field.millisLeft - tick }

decoder : Decoder Field
decoder =
  Decode.map3 Field
    (Decode.field "attackName" Decode.string)
    (Decode.field "resolveType" Decode.string)
    (Decode.field "millisLeft" Decode.int)

view : Int -> Color -> Field -> Html msg
view millisTotal backgroundColor { resolveType, millisLeft }  =
  Html.div
    [ Html.css
        [ Css.position Css.relative
        , Css.backgroundColor <| Css.rgba 50 50 50 0.8
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
            , Css.backgroundColor backgroundColor
            , let
                percentLeft = 100 - computePercent millisTotal millisLeft
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
                time = "(" ++ displaySeconds millisLeft ++ ")"
              in
                Html.text <| resolveType ++ " " ++ time
            ]
        ]
    ]

displaySeconds : Int -> String
displaySeconds millis =
  let
    seconds = max 0 (millis // 1000)
  in
    String.fromInt seconds ++ "s"

computePercent : Int -> Int -> Float
computePercent total current =
  100 * clamp 0 1 (toFloat current / toFloat total)
