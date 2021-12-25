module Main exposing (..)

import Browser
import Css
import Css.Colors as Colors
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Html.Styled.Events as Html
import Json.Decode as Decode
import Time

import Field exposing (Field)
import Array

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

init : Flags -> (Model, Cmd Msg)
init _ =
  ( Model
    [ Field "Gaoler's Flail" "Left/right" (timeToMillis 10)
    , Field "Gaoler's Flail" "Left/right" (timeToMillis 15)
    , Field "Warder's Wrath" "Raidwide" (timeToMillis 18)
    , Field "Pitiless Flail + True Holy" "KB into stack" (timeToMillis 24)
    , Field "Gaoler's Flail" "Left/right" (timeToMillis 37)
    , Field "Heavy Hand" "TB" (timeToMillis 115)
    ]
    -5000 20000 0
  , Cmd.none
  )

-- UPDATE

type Msg
  = Tick Time.Posix
  | Input String

tickTimeMillis : Int
tickTimeMillis = 100

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let newModel =
        case msg of
          Tick _ ->
            model
              |> decrement tickTimeMillis
              |> increment tickTimeMillis

          Input input ->
            case decodeFields input of
              Ok fields ->
                { model | fields = fields }

              _ ->
                model
  in
    ( newModel, Cmd.none )

decodeFields : String -> Result Decode.Error (List Field)
decodeFields =
  Result.map Array.toList
    << Decode.decodeString (Decode.array Field.decoder)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Time.every (toFloat tickTimeMillis) Tick

decrement : Int -> Model -> Model
decrement tick model =
  { model | fields =
      List.filterMap
        (Field.decrement tick model.graceMillis)
        model.fields
  }

increment : Int -> Model -> Model
increment tick model =
  { model | millisPassed = model.millisPassed + tick}

-- VIEW

viewClock : Int -> Html Msg
viewClock millisPassed =
  Html.div
    [ Html.css
        [ Css.color Colors.white
        , Css.textAlign Css.center
        , Css.fontSize <| Css.em 3
        , Css.marginBottom <| Css.em 1
        ]
    ]
    [ Html.text <| millisToClock millisPassed
    ]

viewName : String -> Html Msg
viewName attackName =
  Html.div
    [ Html.css
        [ Css.color Colors.white
        , Css.textAlign Css.left
        , Css.fontSize <| Css.pct 200
        , Css.marginLeft <| Css.pct 24
        ]
    ]
    [ Html.text attackName
    ]

viewCurrentBox : Int -> Field -> Html Msg
viewCurrentBox millisTotal field =
  Html.div
    [ Html.css
        [ Css.width <| Css.pct 75
        , Css.marginLeft <| Css.pct 10
        , Css.border3 (Css.px 1) Css.solid Colors.white
        ]
    ]
    [ viewName field.attackName
    , Field.view millisTotal (Css.rgba 255 0 0 0.6) field
    ]

view : Model -> Html Msg
view { fields, millisPassed, millisTotal } =
  let
    input = Html.input [ Html.onInput Input ] []
    clock = viewClock millisPassed
    countdowns = List.map (viewCurrentBox millisTotal) fields
  in
    Html.div
      [ Html.css
          [ Css.backgroundColor Colors.black
          , Css.paddingTop <| Css.em 5
          , Css.paddingBottom <| Css.em 5
          ]
      ] <| [input, clock] ++ countdowns
