module Nihilist exposing (createProblem)

import Alpha
import Common exposing (..)
import Data
import Interface exposing (..)
import Keyword exposing (Keyword)
import Polybius exposing (Polybius)
import Token exposing (Token)

createProblem : RandomInput -> List (List Token) -> Problem
createProblem randomInput words =
    let
        params =
            { k = randomInput.a |> Data.randomKeyword |> Keyword.createOrDefault
            , p = randomInput.b |> Data.randomKeyword |> Polybius.create
            }
    in
    { cipherType = Nihilist
    , instructions =
        "Nihilist (Polybius Key = "
            ++ (Polybius.toKeywordStr params.p)
            ++ ", Keyword = "
            ++ (Keyword.toStr params.k)
             ++ ")"
    , words = words |> List.map (\w -> { letters = w |> List.map (encryptLetter params) })
    }

encryptLetter : Params -> Token -> Letter
encryptLetter params t =
    { idx = t.idx
    , group = (Keyword.getAt params.k t.idx |> Alpha.toStr)
        ++ "--"
        ++ (Alpha.toStr t.char)
    , plain = t.char |> Alpha.toStr
    , cipher = encrypt params t
    , guess = Nothing
    }

encrypt : Params -> Token -> String
encrypt params t =
    Polybius.encode params.p params.k t
        |> String.fromInt
    
type alias Params = { k : Keyword , p : Polybius }