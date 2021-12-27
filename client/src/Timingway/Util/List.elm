module Timingway.Util.List exposing (padLeft)


padLeft : Int -> a -> List a -> List a
padLeft n elem xs =
    let
        m =
            n - List.length xs

        ys =
            List.take n xs
    in
    List.repeat m elem ++ ys
