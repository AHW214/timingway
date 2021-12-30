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
import Timingway.Util.Basic as Basic


-- MAIN


type alias Flags =
    ()

pastAmount : Int
pastAmount = 1

presentAmount : Int
presentAmount = 1

futureAmount : Int
futureAmount = 2

main : Program Flags Model Msg
main =
    let
        config =
            { past =
                { amount = pastAmount
                , colorBar = Css.rgba 0 200 0 0.6
                , isFocus = False
                }
            , present =
                { amount = presentAmount
                , colorBar = Css.rgba 255 0 0 0.6
                , isFocus = True
                }
            , future =
                { amount = futureAmount
                , colorBar = Css.rgba 0 100 255 0.6
                , isFocus = False
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


{-| Models have the following attributes:
    fields: A list of fields, each field representing a currently active timer
    lastTick: Posix time of the last tick recorded in the model
    millisTotal: Determines at how many milliseconds timer bars begin to decrement
    millisPassed: Determines the total duration the timer has been active for
-}
type alias Model =
    { fields : List Field
    , lastTick : Maybe Time.Posix
    , millisTotal : Int
    , millisPassed : Int
    , isTicking: Bool
    }

{-| Given time in MMSS format, returns the number of milliseconds.
-}
timeToMillis : Int -> Int
timeToMillis time =
    let
        mins =
            time // 100

        seconds =
            remainderBy 100 time
    in
    (mins * 60 + seconds) * 1000

{-| Constructs a field given each parameter.
    String: Attack name
    String: Resolution of attack
    Int: Time in MMSS format
-}
makeField : String -> String -> Int -> Field
makeField attackName resolveType =
    Field attackName resolveType << timeToMillis


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { fields =
            [ makeField "No Previous Mechanics" "" 0
            , makeField "Gaoler's Flail" "Left/right" 10
            , makeField "Prismatic Deception" "Sword up = in" 15
            , makeField "Akh Rhai" "Prepare to move" 18
            , makeField "Hell's Judgment" "HP to 1" 24
            , makeField "Decollation" "Raidwide" 37
            , makeField "Pitiless Rescue" "KB Immunity" 115
            ]
      , lastTick = Nothing
      , millisTotal = 15000
      , millisPassed = 0
      , isTicking = False
      }
    , Task.perform Init Time.now
    )

-- UPDATE


type Msg
    = Init Time.Posix
    | Tick Time.Posix
    | Input String
    | Reset
    | Continue
    | Pause

{-| Determines how often the model updates (in milliseconds).
-}
tickTimeMillis : Int
tickTimeMillis =
    100


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newCommand =
            if
                msg == Reset || msg == Continue
            then
                Task.perform Init Time.now
            else
                Cmd.none
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
                        |> advanceFields pastAmount

                Input input ->
                    case decodeFields input of
                        Ok fields ->
                            { model | fields = fields }

                        _ ->
                            model

                Reset ->
                    let
                        initModel = Tuple.first <| init ()
                    in
                        { initModel | isTicking = True }

                Continue ->
                    { model | isTicking = True }

                Pause ->
                    { model | isTicking = False }
    in
    ( newModel, newCommand )


decrementFields : Int -> Model -> Model
decrementFields millis model =
    { model | fields = List.map (Field.tick millis) model.fields }

{-| Updates the list of current timers, dependent on the timer of the given index.
    Int: index of the "upcoming attack" in the list.
    Model: model to be updated.
    If the timer for the given index reaches 0, then the model discards the earliest attack currently displayed.
-}
advanceFields : Int -> Model -> Model
advanceFields index model =
    let
        fields =
            case List.getAt index model.fields of
                Just { millisLeft } ->
                    if millisLeft < 0 then
                        List.drop presentAmount model.fields

                    else
                        model.fields

                Nothing ->
                    model.fields
    in
    { model | fields = fields }

{-| Increments the timer by a certain amount of milliseconds.
-}
incrementTimer : Int -> Model -> Model
incrementTimer millis model =
    { model | millisPassed = model.millisPassed + millis }

{-| Sets the last tick to be a certain Posix time for purposes of calculating delta.
-}
setLastTick : Time.Posix -> Model -> Model
setLastTick time model =
    { model | lastTick = Just time }

decodeFields : String -> Result Decode.Error (List Field)
decodeFields =
    Result.map Array.toList
        << Decode.decodeString (Decode.array Field.decoder)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { isTicking } =
    if
        isTicking
    then
        Time.every (toFloat tickTimeMillis) Tick
    else
        Sub.none



-- VIEW


type alias FieldConfig =
    { amount : Int
    , colorBar : Color
    , isFocus : Bool
    }


type alias FieldsConfig =
    { future : FieldConfig
    , past : FieldConfig
    , present : FieldConfig
    }


viewFields : Int -> List Field -> List FieldConfig -> List (Html Msg)
viewFields millisTotal fields =
    let
        f config ( fieldss, views ) =
            let
                ( toView, rest ) =
                    List.splitAt config.amount fieldss

                conf =
                    { colorBackground = Css.rgba 150 150 150 0.5
                    , colorBar = config.colorBar
                    , isFocus = config.isFocus
                    , millisTotal = millisTotal
                    }

                newViews =
                    List.map (Field.view conf) toView
            in
            ( rest, views ++ newViews )
    in
    Tuple.second << List.foldl f ( fields, [] )


view : FieldsConfig -> Model -> Html Msg
view config { isTicking, fields, millisPassed, millisTotal } =
    let
        reset =
            Html.button
                [ Html.onClick Reset
                , Html.css
                    [ Css.backgroundColor <| Css.rgba 50 50 50 0.8
                    , Css.position Css.absolute
                    , Css.color Colors.white
                    , Css.textAlign Css.center
                    , Css.marginBottom <| Css.rem 1
                    , Css.fontSize <| Css.rem 5
                    , Css.padding <| Css.rem 1
                    , Css.marginTop <| Css.rem 8
                    , Css.marginLeft <| Css.rem 2.5
                    , Css.height <| Css.rem 10
                    , Css.width <| Css.rem 10
                    , Css.borderRadius <| Css.rem 1
                    ]
                ] [
                    Html.text "\u{21BA}"
                ]
        pause = 
            Html.button
                [ Html.onClick
                    <| Basic.choose isTicking Pause Continue
                , Html.css
                    [ Css.backgroundColor <| Css.rgba 50 50 50 0.8
                    , Css.position Css.absolute
                    , Css.color Colors.white
                    , Css.textAlign Css.center
                    , Css.marginBottom <| Css.rem 1
                    , Css.fontSize <| Css.rem 5
                    , Css.padding <| Css.rem 1
                    , Css.marginTop <| Css.rem 28
                    , Css.marginLeft <| Css.rem 2.5
                    , Css.height <| Css.rem 10
                    , Css.width <| Css.rem 10
                    , Css.borderRadius <| Css.rem 1
                    ]
                ]
                [ Html.text
                    <| Basic.choose isTicking "\u{23FE}" "\u{25B6}"
                ]

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
            [ Css.paddingTop <| Css.em 2
            , Css.paddingBottom <| Css.em 1
            ]
        ]
        ([ reset, pause, clock ]
            ++ fieldViews
        )