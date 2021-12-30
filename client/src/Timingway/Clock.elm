module Timingway.Clock exposing (view)

import Css
import Css.Colors as Colors
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Html



-- VIEW


{-| Render the given number of miliseconds as a clock.
-}
view : Int -> Html msg
view millis =
    Html.div
        [ Html.css
            [ Css.position <| Css.absolute
            , Css.marginTop <| Css.rem 1
            , Css.marginLeft <| Css.rem 65
            , Css.color Colors.white
            , Css.backgroundColor <| Css.rgba 150 150 150 0.5
            , Css.padding <| Css.rem 1
            , Css.width <| Css.rem 15
            , Css.borderRadius <| Css.rem 0.5
            , Css.textAlign Css.center
            , Css.fontSize <| Css.rem 4
            ]
        ]
        [ Html.text <| displayClock millis
        ]


{-| Display the given number of milliseconds as a MM:SS string.
-}
displayClock : Int -> String
displayClock millis =
    let
        padTime =
            String.padLeft 2 '0'

        totalSeconds =
            millis // 1000

        seconds =
            String.fromInt <| remainderBy 60 totalSeconds

        minutes =
            String.fromInt <| totalSeconds // 60
    in
    padTime minutes ++ " : " ++ padTime seconds
