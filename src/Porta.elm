module Porta exposing (createProblem)

import Alpha
import Common exposing (..)
import Data
import Interface exposing (..)
import Keyword exposing (Keyword)
import Token exposing (Token, TokenData)

createProblem : RandomInput -> List (List Token) -> Problem
createProblem randomInput words =
    let
        params =
            { k = randomInput.a |> Data.randomKeyword |> Keyword.createOrDefault }
    in
    { cipherType = Porta
    , instructions = "Porta (keyword = " ++ (Keyword.toStr params.k) ++ ")"
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
    let
        cVal = t.char |> Alpha.toVal

        offset = 
            Keyword.getAt params.k t.idx
                |> Alpha.toVal
                |> (\k -> k // 2 + 13)

        val = if cVal <= (Alpha.toVal Alpha.m)
            then
                cVal + offset
            else
                cVal - offset

        val_ = if val >= 0 && val <= 25
            then
                val
            else if val < 0
                then
                    val + 13
                else -- val > 25
                    val - 13
    in
    val_ |> Alpha.fromVal |> Alpha.toStr

type alias Params = { k : Keyword }