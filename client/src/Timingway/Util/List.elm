module Timingway.Util.List exposing (padLeft, choose)


padLeft : Int -> a -> List a -> List a
padLeft n elem xs =
    let
        m =
            n - List.length xs

        ys =
            List.take n xs
    in
    List.repeat m elem ++ ys

choose : Bool -> a -> a -> a
choose clause valTrue valFalse =
    if clause then valTrue else valFalse