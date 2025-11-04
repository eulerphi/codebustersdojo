module Caesar exposing (createProblem)

import Alpha exposing (Alpha)
import Common exposing (..)
import Interface exposing (..)
import Extra
import Token exposing (Token)

createProblem : RandomInput -> List (List Token) -> Problem
createProblem randomInput words =
    let
        offset = Extra.randomInt randomInput.a minOffset maxOffset
    in
    { cipherType = Caesar
    , instructions = "Caesar"
    , words = words |> List.map (\w -> { letters = w |> List.map (encryptLetter offset) })
    }

encryptLetter : Int -> Token -> Letter
encryptLetter offset t =
    let
        cipher = encrypt offset t.char
    in
    { idx = t.idx
    , group = cipher
    , plain = t.char |> Alpha.toStr
    , cipher = cipher
    , guess = Nothing
    }

encrypt : Int -> Alpha -> String
encrypt offset char =
    offset + (Alpha.toVal char)
        |> Alpha.fromVal
        |> Alpha.toStr

minOffset : Int
minOffset = 1

maxOffset : Int
maxOffset = 25