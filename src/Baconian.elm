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
    }

encryptLetter : Token -> Letter
encryptLetter t =
    { idx = t.idx
    , group = t.char |> Alpha.toStr
    , plain = t.char |> Alpha.toStr
    , cipher = encrypt t.char
    , guess = Nothing
    }

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