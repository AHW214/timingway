module Main exposing (..)

import Array
import Browser
import Browser.Navigation exposing (Key)
import Css
import Css.Global
import Css.Colors as Colors
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Html.Styled.Events as Html
import Json.Decode as Decode
import List.Extra as List
import Task
import Time
import Timingway.Clock as Clock
import Timingway.Config exposing (ViewConfig)
import Timingway.Mech as Mech exposing (Mech)
import Timingway.Overflow as Overflow
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
    , isTicking: Bool
    , viewConfig: ViewConfig
    , route: Route
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
    ( { initModel | route = parseRoute url }
    , Task.perform Init Time.now
    )

initModel : Model
initModel =
    { mechs =
        [ makeMech "No Previous Mechanics" "" Nothing 0
        , makeMech "Murky Depths" "Raidwide" Nothing 15
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
    }

-- UPDATE


type Msg
    = Init Time.Posix
    | Tick ViewConfig Time.Posix
    | Input String
    | Reset
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
    let
        newModel =
            case msg of
                Init time ->
                    setLastTick time model

                Tick viewConfig time ->
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
                        |> decrementMechs delta
                        |> advanceMechs viewConfig.past.amount

                Input input ->
                    case decodeMechs input of
                        Ok mechs ->
                            { model | mechs = mechs }

                        _ ->
                            model

                Reset ->
                    { initModel
                        | isTicking = True
                        , route = model.route
                    }

                Continue ->
                    { model | isTicking = True }

                Pause ->
                    { model | isTicking = False }

                NoOp ->
                    model
        newCommand =
            case msg of
                Continue ->
                    Task.perform Init Time.now
                _ ->
                    Cmd.none
    in
    ( newModel, newCommand )


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

parseRoute : Url -> Route
parseRoute url =
    Maybe.withDefault NotFound
        <| Url.parse routeParser url

routeParser : Url.Parser (Route -> a) a
routeParser =
    let
        rootParser =
            -- todo: index.html only for development
            Url.oneOf [ Url.s "index.html", Url.top ]

        checkOverlayEnabled query =
            case query of
                Just str ->
                    if String.toLower str == "true" || str == "1"
                        then Overlay
                        else Site
                Nothing -> Site
    in
        Url.map checkOverlayEnabled
            <| rootParser <?> Query.string "overlay"


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { isTicking, viewConfig } =
    if
        isTicking
    then
        Time.every ( toFloat tickTimeMillis ) ( Tick viewConfig )
    else
        Sub.none



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
        reset =
            Html.button
                [ Html.onClick Reset
                , Html.css <|
                    ( Css.marginTop <| Css.rem 8 ) :: buttonCss
                ] [Html.text "Reset"]

        pause =
            Html.button
                [ Html.onClick
                    <| Basic.choose isTicking Pause Continue
                , Html.css <|
                    ( Css.marginTop <| Css.rem 28 ) :: buttonCss
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
        [ reset
        , pause
        , clock
        , mechsView
        , overflow
        ]