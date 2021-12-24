module Main exposing (..)

import Browser
import Css
import Css.Colors as Colors
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
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
  , millisTotal : Int
  }

type alias Model =
  { fields: List Field
  , graceMillis: Int
  }

makeField : String -> String -> Int -> Field
makeField attackName resolveType millisTotal =
  Field attackName resolveType millisTotal millisTotal

init : Flags -> (Model, Cmd Msg)
init _ =
  ( Model
    [ makeField "Levinstrike Pinax" "Proximity" 13000
    , makeField "Acid Mekhane" "KB" 25000
    , makeField "Elegant Evisceration" "TB" 49000
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
viewField { attackName, resolveType, millisLeft, millisTotal } =
  let
    ( percentLeft, secondsLeft, backgroundColor ) =
      if millisLeft > 0 then
        ( 100 * (toFloat millisLeft) / (toFloat millisTotal)
        , Round.round 1 ((toFloat millisLeft) / 1000)
        , Colors.lightgray
        )
      else
        ( 0
        , "Active!"
        , Colors.green
        )
  in
    Html.div
      [ Html.css
          [ Css.position Css.relative
          , Css.backgroundColor backgroundColor
          , Css.width <| Css.rem 30
          , Css.height <| Css.rem 3
          ]
      ]
      [ Html.div
          [ Html.css
            [ Css.position Css.absolute
            , Css.backgroundColor Colors.red
            , Css.width <| Css.pct percentLeft
            , Css.height <| Css.rem 3
            ]
          ]
          []
      , Html.div
          [ Html.css
              [ Css.position Css.absolute
              ]
          ]
          [ Html.div [] [ Html.text (attackName ++ " | " ++ resolveType ) ]
          , Html.div [] [ Html.text secondsLeft ]
          ]
      ]

view : Model -> Html Msg
view { fields } =
  let
    countdowns = List.map viewField fields
  in
    Html.div [] countdowns
