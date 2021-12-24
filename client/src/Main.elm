module Main exposing (..)

import Browser
import Html exposing (..)
import Time
import Round

-- MAIN

type alias Flags = ()

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
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
  }

init : Flags -> (Model, Cmd Msg)
init _ =
  ( Model
    [ Field "Levinstrike Pinax" "Proximity" 13000
    , Field "Acid Mekhane" "KB" 25000
    , Field "Elegant Evisceration" "TB" 49000
    ]
    -5000
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
      ( decrement tickTimeMillis model, Cmd.none )

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

-- VIEW

viewField : Field -> Html Msg
viewField { attackName, resolveType, millisLeft } =
  let
    secondsLeft = (toFloat millisLeft) / 1000
  in
    Html.text <|
      String.join " "
        [ Round.round 1 secondsLeft
        , attackName
        , resolveType
        ]

view : Model -> Html Msg
view { fields } =
  let
    countdowns = List.map viewField fields
  in
    div [] <| List.intersperse (br [] []) countdowns
