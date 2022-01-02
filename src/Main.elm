module Main exposing (..)

import Array
import Browser
import Browser.Events as Events
import Browser.Navigation exposing (Key)
import Css
import Css.Global
import Css.Colors as Colors
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Html.Styled.Events as Html
import Http
import Json.Decode as Decode
import List.Extra as List
import Task
import Time
import Timingway.Clock as Clock
import Timingway.Config exposing (ViewConfig)
import Timingway.Mech as Mech exposing (Mech)
import Timingway.Overflow as Overflow
import Timingway.Sheet as Sheet exposing (Row)
import Timingway.Util.Basic as Basic
import Timingway.Overlay as Overlay
import Url as Url exposing (Url)
import Url.Parser as Url exposing ((</>), (<?>))
import Url.Parser.Query as Query

-- import FeatherIcons

-- MAIN

type alias Flags =
    ()

main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = always NoOp
        , onUrlRequest = always NoOp
        }

-- MODEL

{-| Models have the following attributes:
    mechs: A list of mechanics
    lastTick: Posix time of the last tick recorded in the model
    millisTotal: Determines at how many milliseconds timer bars begin to decrement
    millisPassed: Determines the total duration the timer has been active for
    viewConfig: A ViewConfig which determines its view.
-}
type alias Model =
    { mechs : List Mech
    , lastTick : Maybe Time.Posix
    , millisPassed : Int
    , isTicking : Bool
    , viewConfig : ViewConfig
    , route : Route
    , sheetId : String
    , keyMap : Dict String Bool
    }

type Route
    = Site
    | Overlay
    | NotFound

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

{-| Constructs a mechanic given each parameter.
    String: Attack name
    String: Resolution of attack
    Int: Time in MMSS format
-}
makeMech : String -> String -> Maybe String -> Int -> Mech
makeMech attackName resolveType optionalNotes millisLeft =
    Mech attackName resolveType optionalNotes ( timeToMillis millisLeft )


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url _ =
    let
        ( route, sheetId ) =
            parseRouteAndSheet url
    in
        ( { initModel | route = route, sheetId = sheetId }
        , Cmd.batch
            [ Task.perform Init Time.now
            , Http.get
                { url = Sheet.makeQueryUrl sheetId "App"
                , expect = Http.expectJson ( GetSheet 0 ) Sheet.decoder
                }
            ]
        )

prevMech : Mech
prevMech = makeMech "Countdown" "No Previous Mechanics" Nothing 0

initModel : Model
initModel =
    { mechs = List.singleton prevMech
    , lastTick = Nothing
    , millisPassed = 0
    , isTicking = False
    , viewConfig =
        { past =
            { amount = 1
            , barColor = Css.rgba 0 200 0 0.6
            , barGradient = Css.rgba 0 255 155 0.6
            , isFocus = False
            }
        , present =
            { amount = 1
            , barColor = Css.rgba 255 0 0 0.6
            , barGradient = Css.rgba 255 155 0 0.6
            , isFocus = True
            }
        , future =
            { amount = 2
            , barColor = Css.rgba 0 100 255 0.6
            , barGradient = Css.rgba 200 100 255 0.6
            , isFocus = False
            }
        , millisTotal = 15000
        , backgroundColor = Css.rgba 150 150 150 0.5
        }
    , route = Site
    , sheetId = sampleSheet
    , keyMap = Dict.empty
    }

-- UPDATE


type Msg
    = Init Time.Posix
    | Tick ViewConfig Time.Posix
    | GetSheet Int (Result Http.Error (List Row))
    | ToggleKey Bool String
    | Reset Int
    | Continue
    | Pause
    | NoOp


{-| Determines how often the model updates (in milliseconds).
-}
tickTimeMillis : Int
tickTimeMillis =
    100


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init time ->
            ( setLastTick time model
            , Cmd.none
            )

        Tick viewConfig time ->
            let
                delta =
                    case model.lastTick of
                        Nothing ->
                            tickTimeMillis

                        Just lastTick ->
                            Time.posixToMillis time - Time.posixToMillis lastTick
            in
                ( model
                    |> setLastTick time
                    |> incrementTimer delta
                    |> decrementMechs delta
                    |> advanceMechs viewConfig.past.amount
                , Cmd.none
                )

        GetSheet countdown result ->
            ( case result of
                Err _ -> model

                Ok rows ->
                    let
                        mechs = prevMech :: List.filterMap Mech.fromRow rows
                        addCountdown mech = { mech | millisLeft = mech.millisLeft + countdown }
                    in
                        { model | mechs = List.map addCountdown mechs }
            , Cmd.none
            )

        Reset countdown ->
            ( { initModel
                | isTicking = True
                , route = model.route
                , sheetId = model.sheetId
                }
            , Cmd.batch
                [ Task.perform Init Time.now
                , Http.get
                    { url = Sheet.makeQueryUrl model.sheetId "App"
                    , expect = Http.expectJson ( GetSheet countdown ) Sheet.decoder
                    }
                ]
            )

        Continue ->
            ( { model | isTicking = True }
            , Task.perform Init Time.now
            )

        Pause ->
            ( { model | isTicking = False }
            , Cmd.none
            )

        ToggleKey enabled key ->
            let
                newKeyMap =
                    Dict.insert key enabled model.keyMap

                isPressed k =
                    Maybe.withDefault False <| Dict.get k newKeyMap
            in
                if isPressed "1"
                    then ( { model | isTicking = not model.isTicking }
                            , Task.perform Init Time.now
                         )
                    else if isPressed "2"
                        then  ( { initModel
                                    | isTicking = True
                                    , route = model.route
                                    , sheetId = model.sheetId
                                    }
                                , Cmd.batch
                                    [ Task.perform Init Time.now
                                    , Http.get
                                        { url = Sheet.makeQueryUrl model.sheetId "App"
                                        , expect = Http.expectJson ( GetSheet 0 ) Sheet.decoder
                                        }
                                    ]
                                )
                        else ( model, Cmd.none)

        NoOp ->
            ( model
            , Cmd.none
            )

decrementMechs : Int -> Model -> Model
decrementMechs millis model =
    let
        decrementList = List.map ( Mech.tick millis )
    in
        { model | mechs = decrementList model.mechs }

{-| Updates the list of current timers, dependent on the timer of the given index.
    Int: index of the "upcoming attack" in the list.
    Int: index o
    Model: model to be updated.
    If the timer for the given index reaches 0, then the model discards the earliest attack currently displayed.
-}
advanceMechs : Int -> Model -> Model
advanceMechs index model =
    let
        newMechs =
            case List.getAt index model.mechs of
                Just { millisLeft } ->
                    if millisLeft < 0 then
                        List.drop 1 model.mechs

                    else
                        model.mechs

                Nothing ->
                    model.mechs
    in
    { model | mechs = newMechs }

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

decodeMechs : String -> Result Decode.Error (List Mech)
decodeMechs =
    Result.map Array.toList
        << Decode.decodeString (Decode.array Mech.decoder)

parseRouteAndSheet : Url -> ( Route, String )
parseRouteAndSheet url =
    Maybe.withDefault ( NotFound, "" ) ( Url.parse routeParser url )

sampleSheet : String
sampleSheet = "1TvSWwkIJMVdI0k6hoO8YXOeFu1jkyjAw1xy3DVloIJo"

routeParser : Url.Parser ( (Route, String) -> ( a, b ) ) ( a, b )
routeParser =
    let
        rootParser =
            -- todo: index.html only for development
            Url.oneOf [ Url.s "index.html", Url.top ]

        tupleParser = Query.map2 Tuple.pair ( Query.string "overlay" ) ( Query.string "url" )

        checkOverlay query =
            case query of
                Just overlay ->
                    if String.toLower overlay == "true" || overlay == "1"
                        then Overlay
                        else Site
                Nothing -> Site

        checkSheet =
            Maybe.withDefault sampleSheet
    in
        Url.map (Tuple.mapBoth checkOverlay checkSheet)
            <| rootParser <?> tupleParser


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { isTicking, viewConfig } =
    let
        keyDecoder toMsg =
            Decode.map2 toMsg
                (Decode.field "repeat" Decode.bool)
                (Decode.field "key" Decode.string)

        subKey event msg =
            event (keyDecoder <| \repeat ->
                if repeat then always NoOp else msg)

        subTick =
            if isTicking
                then Time.every ( toFloat tickTimeMillis ) ( Tick viewConfig )
                else Sub.none
    in
        Sub.batch
            [ subTick
            , subKey Events.onKeyDown <| ToggleKey True
            , subKey Events.onKeyUp <| ToggleKey False
            ]

-- VIEW

viewMechs : List Mech -> ViewConfig -> Html Msg
viewMechs mechs viewConfig =
    let
        groupsList = [ viewConfig.past, viewConfig.present, viewConfig.future ]
        viewNextGroup groupConfig ( viewList , mechList ) =
            let
                ( groupToView , restOfGroups ) =
                    List.splitAt groupConfig.amount mechList

                newViews =
                    List.map ( Mech.view viewConfig groupConfig ) groupToView
            in
                ( viewList ++ newViews , restOfGroups )

        groupViews =
            groupsList
                |> List.foldl viewNextGroup ( [] , mechs )
                |> Tuple.first
    in
        Html.div
            [ Html.css
                [ Css.marginLeft <| Css.rem 18
                ]
            ]
            groupViews


view : Model -> Browser.Document Msg
view model =
    let
        viewNotFound _ =
            Html.div [] [ Html.text "not found" ]

        viewRoute =
            case model.route of
                Site -> viewSite
                Overlay -> viewOverlay
                NotFound -> viewNotFound

        styles =
            Css.Global.body
                [ Css.backgroundImage <| Css.url "assets/darkmode.jpg"
                , Css.backgroundRepeat Css.noRepeat
                , Css.backgroundAttachment Css.fixed
                , Css.backgroundSize Css.cover
                , Css.fontFamilies [ "Lato", "sans-serif" ]
                ]
    in
        { title =
            "Timingway: a web application for raids."

        , body =
            List.map Html.toUnstyled
                [ viewRoute model
                , Css.Global.global [ styles ]
                ]
        }


viewOverlay : Model -> Html Msg
viewOverlay { mechs } =
    let

        overlay = Overlay.view mechs
    in
    Html.div
        [ Html.css
            [ Css.marginLeft <| Css.rem 1
            , Css.marginTop <| Css.rem 1
            , Css.displayFlex
            ]
        ]
        [ overlay
        ]

viewSite : Model -> Html Msg
viewSite { viewConfig, isTicking, mechs, millisPassed } =
    let
        buttonCss =
            [ Css.backgroundColor <| Css.rgba 50 50 50 0.8
            , Css.position Css.absolute
            , Css.color Colors.white
            , Css.textAlign Css.center
            , Css.marginBottom <| Css.rem 1
            , Css.fontSize <| Css.rem 2.5
            , Css.marginLeft <| Css.rem 2.5
            , Css.height <| Css.rem 10
            , Css.width <| Css.rem 10
            , Css.borderRadius <| Css.rem 1
            ]
        five =
            Html.button
                [ Html.onClick <| Reset 5000
                , Html.css <|
                    ( Css.marginTop <| Css.rem 12 ) :: buttonCss
                ] [Html.text "Five"]

        ten =
            Html.button
                [ Html.onClick <| Reset 10000
                , Html.css <|
                    ( Css.marginTop <| Css.rem 24 ) :: buttonCss
                ] [Html.text "Ten"]

        zero =
            Html.button
                [ Html.onClick <| Reset 0
                , Html.css <|
                    ( Css.marginTop <| Css.rem 0 ) :: buttonCss
                ] [Html.text "Zero"]
        pause =
            Html.button
                [ Html.onClick
                    <| Basic.choose isTicking Pause Continue
                , Html.css <|
                    ( Css.marginTop <| Css.rem 36 ) :: buttonCss
                ] [ Html.text
                    <| Basic.choose isTicking "Pause" "Play"
                ]

        clock =
            Clock.view millisPassed

        mechsView =
            viewMechs mechs viewConfig

        overflow =
            Overflow.view viewConfig mechs
    in
    Html.div
        [ Html.css
            [ Css.displayFlex
            , Css.paddingTop <| Css.em 2
            , Css.paddingBottom <| Css.em 1
            ]
        ]
        [ zero
        , five
        , ten
        , pause
        , clock
        , mechsView
        , overflow
        ]

hippokampos : List Mech
hippokampos =
    [ makeMech "Murky Depths" "Raidwide" Nothing 15
    , makeMech "Doubled Impact" "Tank Share" Nothing 27
    , makeMech "Spoken Cataract" "Head + Body" Nothing 44
    , makeMech "Spoken Cataract" "Head + Body" Nothing 58
    , makeMech "Murky Depths" "Raidwide" Nothing 112
    , makeMech "Sewage Deluge" "Raidwide" (Just "Waters rise") 125
    , makeMech "Tainted Flood" "Spread" Nothing 151
    , makeMech "Predatory Sight" "Stack + 1" Nothing 207
    , makeMech "Shockwave" "Jump + KB" Nothing 217
    , makeMech "Disassociation" "Head Dive" Nothing 246
    , makeMech "Coherence" "Flare + Stack" Nothing 258
    , makeMech "Murky Depths" "Raidwide" Nothing 315
    , makeMech "Sewage Deluge" "Raidwide" (Just "Waters rise") 329
    , makeMech "Tainted Flood" "Spread" Nothing 347
    , makeMech "Spoken Cataract" "Head + Body" Nothing 355
    , makeMech "Sewage Eruption" "3 Eruptions" Nothing 406
    , makeMech "Spoken Cataract" "Head + Body" Nothing 419
    , makeMech "Tainted Flood" "Spread" Nothing 427
    , makeMech "Predatory Sight" "Stack + 1" Nothing 440
    , makeMech "Murky Depths" "Raidwide" Nothing 447
    , makeMech "Disassociation + Shockwave" "Head Dive + KB" Nothing 511
    , makeMech "Disassociation + Sewage Eruption" "Head Dive + 3 Eruptions" Nothing 542
    , makeMech "Coherence" "Flare + Stack" Nothing 547
    , makeMech "Murky Depths" "Raidwide" Nothing 601
    , makeMech "Murky Depths" "Raidwide" Nothing 612
    , makeMech "Doubled Impact" "Tank Share" Nothing 623
    , makeMech "Sewage Deluge" "Raidwide" (Just "Waters rise") 640
    , makeMech "Tainted Flood" "Spread" Nothing 658
    , makeMech "Spoken Cataract" "Head + Body" Nothing 706
    ]