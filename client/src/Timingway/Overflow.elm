module Timingway.Overflow exposing (view)

import Css
import Css.Colors as Colors
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Timingway.Config exposing (ViewConfig)
import Timingway.Mech exposing (Mech)
import Timingway.Util.Basic as Basic

-- VIEW

view : ViewConfig -> List Mech -> Html msg
view { past, present, future } mechs =
    let
        numBeforeOverflow =
            past.amount + present.amount + future.amount

        mechsOverflow =
            List.drop numBeforeOverflow mechs
    in
        Html.div
            [ Html.css
                [ Css.marginLeft <| Css.rem 5
                , Css.marginTop <| Css.rem 15
                , Css.outlineColor Colors.white
                , Css.outlineStyle Css.solid
                , Css.outlineWidth <| Css.rem 0.25
                , Css.borderRadius <| Css.rem 0.5
                , Css.padding <| Css.rem 0.5
                , Css.width <| Css.rem 30
                , Css.height <| Css.rem 35
                , Css.backgroundColor <| Css.rgba 0 0 0 0.4
                ]
            ]
            (if List.isEmpty mechsOverflow
                then [ viewPlaceholder ]
                else List.map viewMech mechsOverflow)

viewMech : Mech -> Html msg
viewMech { attackName, optionalNotes, resolveType } =
    let
        formatNotes n =
            " (" ++ n ++ ")"

        notes =
            Basic.maybe "" formatNotes optionalNotes

        text =
            attackName ++ " : " ++ resolveType ++ notes
    in
        Html.div
            [ Html.css
                [ Css.color Colors.white
                , Css.backgroundColor <| Css.rgba 150 150 150 0.5
                , Css.padding <| Css.rem 0.5
                , Css.marginBottom <| Css.rem 0.5
                , Css.borderRadius <| Css.rem 0.5
                , Css.textAlign Css.left
                , Css.fontSize <| Css.rem 1.5
                ]
            ]
            [ Html.text text
            ]

viewPlaceholder : Html msg
viewPlaceholder =
    Html.div
        [ Html.css
            [ Css.color Colors.white
            , Css.backgroundColor <| Css.rgba 150 150 150 0.5
            , Css.padding <| Css.rem 0.5
            , Css.borderRadius <| Css.rem 0.5
            , Css.textAlign Css.center
            , Css.fontSize <| Css.rem 2
            , Css.fontStyle Css.italic
            ]
        ]
        [ Html.text "No future moves known..."
        ]
