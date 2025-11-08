module Aristocrat exposing (createProblem)

import Alpha exposing (Alpha)
import Common exposing (..)
import Interface exposing (..)
import Token exposing (Token)
import Key exposing (Key)

createProblem : RandomInput -> List (List Token) -> Problem
createProblem randomInput words =
    let
        params = { key = Key.create randomInput }
    in
    { cipherType = Aristocrat
    , instructions = "Aristocrat"
    , words = words |> List.map (\w -> { letters = w |> List.map (encryptLetter params) })
    }

encryptLetter : Params -> Token -> Letter
encryptLetter params t =
    { idx = t.idx
    , group = t.char |> Alpha.toStr
    , plain = t.char |> Alpha.toStr
    , cipher = encrypt params t.char
    , guess = Nothing
    }

encrypt : Params -> Alpha -> String
encrypt params char =
    Key.encode char params.key
        |> Alpha.toStr

type alias Params = { key : Key }