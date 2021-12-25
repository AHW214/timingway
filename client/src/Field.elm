module Field exposing
  ( Field
  , decoder
  , decrement
  , view
  )

import Css
import Css.Colors as Colors
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Json.Decode as Decode exposing (Decoder)
import Round

type alias Field =
  { attackName: String
  , resolveType: String
  , millisLeft : Int
  }

decrement : Int -> Int -> Field -> Maybe Field
decrement tick grace field =
  let
    millisLeft = field.millisLeft - tick
  in
    if millisLeft > grace
      then Just { field | millisLeft = millisLeft }
      else Nothing

decoder : Decoder Field
decoder =
  Decode.map3 Field
    (Decode.field "attackName" Decode.string)
    (Decode.field "resolveType" Decode.string)
    (Decode.field "millisLeft" Decode.int)

view : Int -> Css.Color -> Field -> Html msg
view millisTotal backgroundColor { attackName, resolveType, millisLeft }  =
  let
    secondsLeft = Round.round 0 ((toFloat millisLeft) / 1000) ++ "s"
    percentLeft =
      if millisLeft > millisTotal then 0
      else if millisLeft > 0 then
        100 * (1 - (toFloat millisLeft) / (toFloat millisTotal))
      else 100
  in
    Html.div
      [ Html.css
          [ Css.position Css.relative
          , Css.backgroundColor <| Css.rgba 50 50 50 0.8
          , Css.margin Css.auto
          , Css.borderRadius <| Css.rem 0.5
          , Css.border3 (Css.px 1) Css.solid backgroundColor
          , Css.boxShadow3 (Css.px 0.5) (Css.px 0.5) backgroundColor
          , Css.marginBottom <| Css.pct 3
          , Css.width <| Css.pct 50
          , Css.height <| Css.rem 6
          , Css.paddingRight <| Css.rem 1
          ]
      ]
      [ Html.div
          [ Html.css
            [ Css.position Css.absolute
            , Css.borderRadius <| Css.rem 0.5
            , Css.backgroundColor backgroundColor
            , Css.width <| Css.pct percentLeft
            , Css.height <| Css.rem 6
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
              , Css.marginTop <| Css.rem 1.5
              ]
          ]
          [ Html.div [] [ Html.text resolveType ]
          , Html.div [ Html.css [
            Css.textAlign Css.right
            ]
          ] [
            Html.text (secondsLeft)
            ]
          ]
      ]
