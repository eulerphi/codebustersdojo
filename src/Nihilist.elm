module Nihilist exposing (createProblem)

import Alpha
import Common exposing (..)
import Data
import Interface exposing (..)
import Keyword exposing (Keyword)
import Polybius exposing (Polybius)
import Token exposing (Token, TokenData)

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
        "Nihilist (polybius key = "
            ++ (Polybius.toKeywordStr params.p)
            ++ ", keyword = "
            ++ (Keyword.toStr params.k)
             ++ ")"
    , words = words |> List.map (\w -> { letters = w |> List.map (encryptLetter params) })
    , table = Nothing
    }

encryptLetter : Params -> Token -> Letter
encryptLetter params t =
    case t of
        Token.Interactive d ->
            Interface.Interactive
                { idx = d.idx
                , group = (Keyword.getAt params.k d.idx |> Alpha.toStr)
                    ++ "--"
                    ++ (Alpha.toStr d.char)
                , plain = d.char |> Alpha.toStr
                , cipher = encrypt params d
                , guess = Nothing
                }
        Token.Punctuation d ->
            Interface.Punctuation d

encrypt : Params -> TokenData -> String
encrypt params t =
    Polybius.encode params.p params.k t
        |> String.fromInt
    
type alias Params = { k : Keyword , p : Polybius }