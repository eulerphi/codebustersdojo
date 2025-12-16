module Baconian exposing (createProblem)

import Alpha exposing (Alpha)
import Common exposing (..)
import Interface exposing (..)
import Token exposing (Token)

createProblem : RandomInput -> List (List Token) -> Problem
createProblem _ words =
    { cipherType = Baconian
    , instructions = "Baconian"
    , words = words |> List.map (\w -> { letters = w |> List.map encryptLetter })
    , table = Nothing
    }

encryptLetter : Token -> Letter
encryptLetter t =
    case t of
        Token.Interactive d ->
            Interface.Interactive
                { idx = d.idx
                , group = d.char |> Alpha.toStr
                , plain = d.char |> Alpha.toStr
                , cipher = encrypt d.char
                , guess = Nothing
                }
        Token.Punctuation d ->
            Interface.Punctuation d

encrypt : Alpha -> String
encrypt char =
    char |> Alpha.toVal |> toBaconianBinary

toBaconianBinary : Int -> String
toBaconianBinary n =
    let
        n_ = if n >= ('v' |> toLetterIndexUnsafe)
            then
                n - 2
            else if n >= ('j' |> toLetterIndexUnsafe)
                then
                    n - 1
                else
                    n 
    in
    [16, 8, 4, 2, 1]
        |> List.foldl
            (\pv (cur, bs) -> if cur >= pv then (cur - pv, 'B' :: bs) else (cur, 'A' :: bs))
            (n_, [])
        |> Tuple.second
        |> String.fromList
        |> String.reverse