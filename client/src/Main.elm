module Main exposing (..)

import Browser
import Css exposing (Color)
import Css.Colors as Colors
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Html.Styled.Events as Html
import Json.Decode as Decode
import List.Extra as List
import Time

import Field exposing (Field)
import Array

-- MAIN

type alias Flags = ()

main : Program Flags Model Msg
main =
  let
    config =
      { past =
          { amount = 2
          , color = Css.rgba 0 200 0 0.6
          }

      , present =
          { amount = 1
          , color = Css.rgba 255 0 0 0.6
          }

      , future =
          { amount = 2
          , color = Css.rgba 0 0 255 0.6
          }
      }
  in
    Browser.element
      { init = init
      , view = Html.toUnstyled << view config
      , update = update
      , subscriptions = subscriptions
      }

-- MODEL

type alias Model =
  { fields: List Field
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

    -- TODO - use library padding function
    padTime str =
      if String.length str == 1
       then "0" ++ str
       else str
  in
     padTime minutes ++ " : " ++ padTime seconds

makeField : String -> String -> Int -> Field
makeField attackName resolveType =
  Field attackName resolveType << timeToMillis

init : Flags -> (Model, Cmd Msg)
init _ =
  ( { fields =
        [ makeField "Floating Inaccuracy" "Low Precision" 10
        , makeField "Gaoler's Flail" "Left/right" 15
        , makeField "Warder's Wrath" "Raidwide" 18
        , makeField "Pitiless Flail + True Holy" "KB into stack" 24
        , makeField "Blase's Bombardment" "Classwide" 37
        , makeField "Heavy Hand" "TB" 115
        ]
    , millisTotal = 20000
    , millisPassed = 0
    }
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
              |> incrementTimer tickTimeMillis
              |> decrementFields tickTimeMillis
              |> advanceFields 2

          Input input ->
            case decodeFields input of
              Ok fields ->
                { model | fields = fields }

              _ ->
                model
  in
    ( newModel, Cmd.none )

decrementFields : Int -> Model -> Model
decrementFields tick model =
  { model | fields = List.map (Field.decrement tick) model.fields }

advanceFields : Int -> Model -> Model
advanceFields ix model =
  let
    fields =
      case List.getAt ix model.fields of
        Just { millisLeft } ->
          if millisLeft < 0
            then List.drop 1 model.fields
            else model.fields

        Nothing -> model.fields
  in
    { model | fields = fields }

incrementTimer : Int -> Model -> Model
incrementTimer tick model =
  { model | millisPassed = model.millisPassed + tick}

decodeFields : String -> Result Decode.Error (List Field)
decodeFields =
  Result.map Array.toList
    << Decode.decodeString (Decode.array Field.decoder)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Time.every (toFloat tickTimeMillis) Tick

-- VIEW

type alias FieldConfig =
  { amount : Int
  , color : Color
  }

type alias FieldsConfig =
  { future : FieldConfig
  , past : FieldConfig
  , present : FieldConfig
  }

view : FieldsConfig -> Model -> Html Msg
view config { fields, millisPassed, millisTotal } =
  let
    input = Html.input [ Html.onInput Input ] []

    clock = viewClock millisPassed

    fieldViews = viewFields
      millisTotal
      fields
      [ config.past
      , config.present
      , config.future
      ]

  in
    Html.div
      [ Html.css
          [ Css.backgroundColor Colors.black
          , Css.paddingTop <| Css.em 5
          , Css.paddingBottom <| Css.em 5
          ]
      ] <| [input, clock] ++ fieldViews

viewFields : Int -> List Field -> List FieldConfig -> List (Html Msg)
viewFields millisTotal fields =
  let
      f config ( fieldss, views ) =
        let
          ( toView, rest ) =
            List.splitAt config.amount fieldss

          newViews = List.map (viewBox millisTotal config.color) toView
        in
          ( rest, views ++ newViews )
  in
    Tuple.second << List.foldl f ( fields, [] )

viewName : Field -> Html Msg
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

viewCurrentBox : Int -> Field -> Html Msg
viewCurrentBox millisTotal field =
  Html.div
    [ Html.css
        [ Css.width <| Css.pct 50
        , Css.border3 (Css.px 5) Css.outset Colors.white
        , Css.marginBottom <| Css.pct 3
        , Css.paddingTop <| Css.pct 2
        , Css.paddingBottom <| Css.pct 2
        ]
    ]
    [ viewName field
    , Field.view millisTotal (Css.rgba 255 0 0 0.6) field
    ]

-- HTML for non-current attack box.
viewBox : Int -> Color -> Field -> Html Msg
viewBox millisTotal boxColor field =
  Html.div
    [ Html.css
        [ Css.width <| Css.pct 50
        , Css.marginBottom <| Css.pct 3
        ]
    ]
    [ viewName field
    , Field.view millisTotal boxColor field
    ]

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
