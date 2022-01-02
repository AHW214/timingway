module Timingway.Time exposing
    ( Time
    , fromString
    , toMillis
    )

type alias Time =
    { minutes: Int
    , seconds: Int
    }

fromString : String -> Maybe Time
fromString str =
    let
        components =
            List.map String.toInt <| String.split ":" str
    in
        case components of
            [ Just mm, Just ss ] ->
                if mm >= 0 && ss >= 0 && ss <= 60
                    then Just <| Time mm ss
                    else Nothing

            _ -> Nothing

toMillis : Time -> Int
toMillis { minutes, seconds } =
    60000 * minutes + 1000 * seconds
