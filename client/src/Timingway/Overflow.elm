module Timingway.Overflow exposing (view)

import Css
import Css.Colors as Colors
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html
import Timingway.Config exposing (ViewConfig)
import Timingway.Mech as Mech exposing (Mech)
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
                , Css.width <| Css.rem 40
                , Css.height <| Css.rem 35
                , Css.backgroundColor <| Css.rgba 0 0 0 0.4
                ]
            ]
            (if List.isEmpty mechsOverflow
                then [ viewPlaceholder ]
                else List.map viewMech mechsOverflow)

viewMech : Mech -> Html msg
viewMech mech =
    Html.div
        [ Html.css
            [ Css.displayFlex
            , Css.justifyContent Css.spaceBetween
            , Css.color Colors.white
            , Css.backgroundColor <| Css.rgba 150 150 150 0.5
            , Css.padding <| Css.rem 0.5
            , Css.marginBottom <| Css.rem 0.5
            , Css.borderRadius <| Css.rem 0.5
            , Css.textAlign Css.left
            , Css.fontSize <| Css.rem 1.5
            ]
        ]
        [ viewMechInfo mech
        , viewMechTime mech
        ]

viewMechInfo : Mech -> Html msg
viewMechInfo mech =
    let
        formatNotes n =
            "(" ++ n ++ ")"

        notesContent =
            Basic.maybe "" formatNotes mech.optionalNotes

        notes =
            Html.span
                [ Html.css [ Css.fontSize <| Css.rem 1 ] ]
                [ Html.text notesContent ]

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
                , Css.fontSize <| Css.rem 1.5
                ]
            ]
            [ Html.div
                []
                [ Html.div [] [ attackName, divider, resolveType ]
                , Html.div [] [ notes ]
                ]
            ]

viewMechTime : Mech -> Html msg
viewMechTime { millisLeft } =
    Html.div
        [ Html.css
            [ Css.color Colors.white
            , Css.fontSize <| Css.rem 1.5
            ]
        ]
        [ Html.text <| Mech.displaySeconds millisLeft
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
