module Timingway.Overlay exposing (view)

import Css
import Css.Colors as Colors
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Timingway.Mech as Mech exposing (Mech, computePercent)
-- import Timingway.Util.Basic as Basic

-- VIEW

view : List Mech -> Html msg
view mechs =
    let

        mechsOverflow =
            mechs
                |> List.drop 1
                |> List.take 4
        mechPast =
            case List.head mechs of
               Just m -> [m]
               Nothing -> []
    in
        Html.div [] [
            Html.div
            [ Html.css [
                Css.marginBottom <| Css.rem 2.5
            ]
            ]
            (if List.isEmpty mechsOverflow
                then [ viewPlaceholder ]
                else List.map viewMech mechPast) ,
            Html.div []
             ( List.map viewMech mechsOverflow )
        ]

viewMech : Mech -> Html msg
viewMech mech =
    Html.div
            [ Html.css
                [ Css.position Css.relative
                , Css.backgroundColor <| Css.rgba 50 50 50 0.5
                , Css.borderRadius <| Css.rem 0.1
                , Css.width <| Css.rem 20
                , Css.height <| Css.rem 2.5
                , Css.marginBottom <| Css.rem 1
                , Css.displayFlex
                ]
            ]
            [ Html.div
                [ Html.css
                    [ Css.position Css.absolute
                    , Css.borderRadius <| Css.rem 0.1
                    , Css.backgroundImage <| Css.linearGradient2 Css.toBottomLeft (Css.stop2 ( Css.rgba 0 155 255 0.7 ) <| Css.pct 20) (Css.stop <| Css.rgba 250 0 255 0.7) []
                    , let
                        percentLeft =
                            100 - computePercent 15000 mech.millisLeft
                    in
                    Css.width <| Css.rem <| 20/100 * percentLeft
                    , Css.height <| Css.rem 2.5
                    , Css.marginBottom <| Css.rem 1
                    ]
                ]
                []
            , Html.div
                [ Html.css
                    [ Css.position Css.absolute
                    , Css.width <| Css.pct 100
                    , Css.color Colors.white
                    , Css.marginLeft <| Css.rem 0.3
                    , Css.textAlign Css.left
                    , Css.marginBottom <| Css.rem 2
                    , Css.marginTop <| Css.rem 0.75
                    ]
                ]
                [ viewMechInfo mech ]
            , Html.div
                [ Html.css
                    [ Css.position Css.absolute
                    , Css.width <| Css.pct 98
                    , Css.color Colors.white
                    , Css.textAlign Css.right
                    , Css.marginTop <| Css.rem 0.75
                    , Css.marginBottom <| Css.rem 2
                    ]
                ] [ viewMechTime mech
                ]
            ]
-- 
{-|
    formatNotes n =
        "(" ++ n ++ ")"

    notesContent =
        Basic.maybe "" formatNotes mech.optionalNotes

    notes =
        Html.span
            [ Html.css [ Css.fontSize <| Css.rem 1 ] ]
            [ Html.text notesContent ]
-}
viewMechInfo : Mech -> Html msg
viewMechInfo mech =
    let

        attackName =
            Html.span
                [ Html.css [ Css.fontWeight Css.bold ] ]
                [ Html.text mech.attackName ]

        resolveType =
            Html.text mech.resolveType

        divider =
            Html.text " : "


    in
        Html.div
            [ Html.css
                [ Css.color Colors.white
                , Css.fontSize <| Css.rem 1
                ]
            ]
            [ Html.div
                []
                [ Html.div [] [ attackName, divider, resolveType ]
                -- , Html.div [] [ notes ]
                ]
            ]

viewMechTime : Mech -> Html msg
viewMechTime { millisLeft } =
    Html.div
        [ Html.css
            [ Css.color Colors.white
            , Css.fontSize <| Css.rem 1
            ]
        ]
        [ Html.text <| Mech.displaySeconds ( if ( millisLeft < 15000 ) then 1 else 0 ) millisLeft
        ]

viewPlaceholder : Html msg
viewPlaceholder =
    Html.div
        [ Html.css
            [ Css.color Colors.white
            , Css.backgroundColor <| Css.rgba 150 150 150 0.5
            , Css.padding <| Css.rem 0.5
            , Css.borderRadius <| Css.rem 0.1
            , Css.textAlign Css.center
            , Css.fontSize <| Css.rem 0.75
            , Css.fontStyle Css.italic
            ]
        ]
        [ Html.text "No future mechanics known..."
        ]
