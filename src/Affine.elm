module Affine exposing (createProblem)

import Alpha exposing (Alpha)
import Array exposing (Array)
import Common exposing (..)
import Interface exposing (..)
import Extra
import Token exposing (Token)

createProblem : RandomInput -> List (List Token) -> Problem
createProblem randomInput words =
    let
        params =
            { a = Extra.randomIdx randomInput.a (Array.length valuesOfA)
                |> (\idx -> Array.get idx valuesOfA)
                |> Maybe.withDefault 1
            , b = Extra.randomInt randomInput.b 1 maxValueOfB
            }
    in
    { cipherType = Affine
    , instructions =
        "Affine (A = " ++ (String.fromInt params.a) ++ ", B = " ++ (String.fromInt params.b) ++ ")"
    , words = words |> List.map (\w -> { letters = w |> List.map (encryptLetter params) })
    }

type alias Params = { a : Int, b : Int }

encryptLetter : Params -> Token -> Letter
encryptLetter params t =
    let
        cipher = encrypt params t.char
    in
    { idx = t.idx
    , group = cipher
    , plain = t.char |> Alpha.toStr
    , cipher = cipher
    , guess = Nothing
    }

encrypt : Params -> Alpha -> String
encrypt params char =
    params.a * (Alpha.toVal char) + params.b
        |> Alpha.fromVal
        |> Alpha.toStr

valuesOfA : Array Int
valuesOfA = Array.fromList [1, 3, 5, 7, 9, 11, 15, 17, 19, 21, 23, 15]

maxValueOfB : Int
maxValueOfB = 25