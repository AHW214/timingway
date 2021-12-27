module Main exposing (..)

import Array
import Browser
import Css exposing (Color)
import Css.Colors as Colors
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Html.Styled.Events as Html
import Json.Decode as Decode
import List.Extra as List
import Task
import Time
import Timingway.Clock as Clock
import Timingway.Field as Field exposing (Field)



-- MAIN


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    let
        config =
            { past =
                { amount = 2
                , colorBar = Css.rgba 0 200 0 0.6
                , colorOutline = Nothing
                }
            , present =
                { amount = 1
                , colorBar = Css.rgba 255 0 0 0.6
                , colorOutline = Just Colors.white
                }
            , future =
                { amount = 2
                , colorBar = Css.rgba 0 0 255 0.6
                , colorOutline = Nothing
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
    { fields : List Field
    , lastTick : Maybe Time.Posix
    , millisTotal : Int
    , millisPassed : Int
    }


timeToMillis : Int -> Int
timeToMillis time =
    let
        mins =
            time // 100

        seconds =
            remainderBy 100 time
    in
    (mins * 60 + seconds) * 1000


makeField : String -> String -> Int -> Field
makeField attackName resolveType =
    Field attackName resolveType << timeToMillis


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { fields =
            [ makeField "Floating Inaccuracy" "Low Precision" 10
            , makeField "Gaoler's Flail" "Left/right" 15
            , makeField "Warder's Wrath" "Raidwide" 18
            , makeField "Pitiless Flail + True Holy" "KB into stack" 24
            , makeField "Blase's Bombardment" "Classwide" 37
            , makeField "Heavy Hand" "TB" 115
            ]
      , lastTick = Nothing
      , millisTotal = 20000
      , millisPassed = 0
      }
    , Task.perform Init Time.now
    )



-- UPDATE


type Msg
    = Init Time.Posix
    | Tick Time.Posix
    | Input String
    | Reset


tickTimeMillis : Int
tickTimeMillis =
    100


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                Init time ->
                    setLastTick time model

                Tick time ->
                    let
                        delta =
                            case model.lastTick of
                                Nothing ->
                                    tickTimeMillis

                                Just lastTick ->
                                    Time.posixToMillis time - Time.posixToMillis lastTick
                    in
                    model
                        |> setLastTick time
                        |> incrementTimer delta
                        |> decrementFields delta
                        |> advanceFields 2

                Input input ->
                    case decodeFields input of
                        Ok fields ->
                            { model | fields = fields }

                        _ ->
                            model

                Reset ->
                    { model | millisPassed = 0 }
    in
    ( newModel, Cmd.none )


decrementFields : Int -> Model -> Model
decrementFields millis model =
    { model | fields = List.map (Field.tick millis) model.fields }


advanceFields : Int -> Model -> Model
advanceFields ix model =
    let
        fields =
            case List.getAt ix model.fields of
                Just { millisLeft } ->
                    if millisLeft < 0 then
                        List.drop 1 model.fields

                    else
                        model.fields

                Nothing ->
                    model.fields
    in
    { model | fields = fields }


incrementTimer : Int -> Model -> Model
incrementTimer millis model =
    { model | millisPassed = model.millisPassed + millis }


setLastTick : Time.Posix -> Model -> Model
setLastTick time model =
    { model | lastTick = Just time }


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
    , colorBar : Color
    , colorOutline : Maybe Color
    }


type alias FieldsConfig =
    { future : FieldConfig
    , past : FieldConfig
    , present : FieldConfig
    }


view : FieldsConfig -> Model -> Html Msg
view config { fields, millisPassed, millisTotal } =
    let
        input =
            Html.input [ Html.onInput Input ] []

        reset =
            Html.button
                [ Html.onClick Reset
                , Html.css
                    [ Css.backgroundColor Colors.whitesmoke
                    , Css.color Colors.black
                    , Css.textAlign Css.center
                    , Css.fontSize <| Css.rem 2
                    , Css.marginLeft <| Css.rem 5
                    , Css.marginBottom <| Css.rem 1
                    ]
                ]
                [ Html.text "Reset Timer" ]

        clock =
            Clock.view millisPassed

        fieldViews =
            viewFields
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
        ]
        ([ input, reset, clock ]
            ++ fieldViews
        )


viewFields : Int -> List Field -> List FieldConfig -> List (Html Msg)
viewFields millisTotal fields =
    let
        f config ( fieldss, views ) =
            let
                ( toView, rest ) =
                    List.splitAt config.amount fieldss

                conf =
                    { colorBackround = Css.rgba 50 50 50 0.8
                    , colorBar = config.colorBar
                    , colorOutline = config.colorOutline
                    , millisTotal = millisTotal
                    }

                newViews =
                    List.map (Field.view conf) toView
            in
            ( rest, views ++ newViews )
    in
    Tuple.second << List.foldl f ( fields, [] )
