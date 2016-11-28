module Utils exposing (..)

import Array exposing (Array)


get2 : Int -> Int -> Array (Array b) -> Maybe b
get2 i0 i1 arr =
    case Array.get i0 arr of
        Just a1 ->
            Array.get i1 a1

        Nothing ->
            Nothing


otherwise : Maybe a -> Maybe a -> Maybe a
otherwise a b =
    case a of
        Just _ ->
            a

        Nothing ->
            b
