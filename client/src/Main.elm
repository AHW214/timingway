module Main exposing (..)

import Browser
import Css
import Css.Colors as Colors
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Html.Events as Html
import List.Extra as List
import Time
import Round

-- MAIN

type alias Flags = ()

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = Html.toUnstyled << view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Field =
  { attackName: String
  , resolveType: String
  , millisLeft : Int
  }

type alias Model =
  { fields: List Field
  , graceMillis: Int
  , millisTotal : Int
  , millisPassed: Int
  }

timeToMillis : Int -> Int
timeToMillis time =
  let
    mins = time // 100
    seconds = remainderBy 100 time
  in
    (mins * 60 + seconds) * 1000

millisToClock : Int -> String
millisToClock millis =
  let
    totalSeconds = millis // 1000
    seconds = String.fromInt <| remainderBy 60 totalSeconds
    minutes = String.fromInt <| totalSeconds // 60
    minutesString =
      case String.length minutes of
        1 -> "0" ++ minutes
        _ -> minutes
    secondsString =
      case String.length seconds of
        1 -> "0" ++ seconds
        _ -> seconds
  in
     minutesString ++ " : " ++ secondsString

makeField : String -> String -> Int -> Field
makeField attackName resolveType millisLeft  =
  Field attackName resolveType millisLeft

init : Flags -> (Model, Cmd Msg)
init _ =
  ( Model
    [ makeField "Gaoler's Flail" "Left/right" (timeToMillis 13)
    , makeField "Gaoler's Flail" "Left/right" (timeToMillis 15)
    , makeField "Warder's Wrath" "Raidwide" (timeToMillis 17)
    , makeField "Pitiless Flail + True Holy" "KB into stack" (timeToMillis 24)
    , makeField "Gaoler's Flail" "Left/right" (timeToMillis 27)
    , makeField "Heavy Hand" "TB" (timeToMillis 28)
    ]
    -5000 20000 0
  , Cmd.none
  )

-- UPDATE

type Msg
  = Tick Time.Posix

tickTimeMillis : Int
tickTimeMillis = 100

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick _ ->
      ( increment tickTimeMillis (decrement tickTimeMillis model), Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Time.every (toFloat tickTimeMillis) Tick

decrementField : Int -> Int -> Field -> Maybe Field
decrementField tick grace field =
  let
    millisLeft = field.millisLeft - tick
  in
    if millisLeft > grace
      then Just { field | millisLeft = millisLeft }
      else Nothing

decrement : Int -> Model -> Model
decrement tick model =
  { model | fields =
    List.filterMap
      (decrementField tick model.graceMillis)
      model.fields
  }

increment : Int -> Model -> Model
increment tick model =
  { model | millisPassed = model.millisPassed + tick}

-- VIEW

viewField : Int -> Css.Color -> Field -> Html Msg
viewField millisTotal backgroundColor { attackName, resolveType, millisLeft }  =
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
          , Css.border3 (Css.px 1) Css.solid Colors.white
          , Css.boxShadow3 (Css.px 0.5) (Css.px 0.5) <| Css.rgba 50 50 50 0.8
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
            , Css.width <| Css.pct percentLeft
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
          [ Html.div []
            [ Html.text <| resolveType ++ " (" ++ secondsLeft ++ ")"
            ]
          ]
        ]

viewClock : Int -> Html Msg
viewClock millisPassed =
  Html.div [
     Html.css [
      Css.color Colors.white
    , Css.textAlign Css.center
    , Css.fontSize <| Css.em 3
    , Css.marginBottom <| Css.em 1
    ]
  ] [
    Html.text <| millisToClock millisPassed
  ]

viewName : Field -> Html Msg
viewName { attackName } =
  Html.div [
     Html.css [
      Css.color Colors.white
    , Css.textAlign Css.left
    , Css.fontSize <| Css.pct 200
    , Css.marginLeft <| Css.pct 5
    , Css.marginBottom <| Css.pct 1
    ]
  ] [
    Html.text attackName
  ]

viewCurrentBox : Int -> Field -> Html Msg
viewCurrentBox millisTotal field =
  Html.div [
    Html.css [
      Css.width <| Css.pct 50
    , Css.marginLeft <| Css.pct 25
    , Css.border3 (Css.px 5) Css.outset Colors.white
    , Css.marginBottom <| Css.pct 3
    , Css.paddingTop <| Css.pct 2
    , Css.paddingBottom <| Css.pct 2
    ]
  ] [
    viewName field, viewField millisTotal (Css.rgba 255 0 0 0.6) field
  ]

viewBox : Int -> Css.Color -> Field -> Html Msg
viewBox millisTotal boxColor field =
  Html.div [
    Html.css [
      Css.width <| Css.pct 50
    , Css.marginLeft <| Css.pct 25
    , Css.marginBottom <| Css.pct 3
    ]
  ] [
    viewName field, viewField millisTotal boxColor field
  ]


view : Model -> Html Msg
view { fields, millisPassed, millisTotal } =
  let
    clock = viewClock millisPassed
    current =
      case List.take 1 <| List.drop 2 fields of
        x :: _ -> viewCurrentBox millisTotal x
        [] -> Html.text ""
    past = List.map (viewBox millisTotal <| Css.rgba 0 200 0 0.6) <| List.take 2 fields
    future = List.map (viewBox millisTotal <| Css.rgba 0 0 255 0.6) <| List.drop 3 fields
  in
    Html.div [
      Html.css
        [
          Css.backgroundColor Colors.black
        , Css.paddingTop <| Css.em 5
        , Css.paddingBottom <| Css.em 5
        ]
    ] <| [clock] ++ past ++ [current] ++ future