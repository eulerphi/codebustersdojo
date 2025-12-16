module Atbash exposing (createProblem)

import Alpha exposing (Alpha)
import Common exposing (..)
import Interface exposing (..)
import Token exposing (Token)

createProblem : RandomInput -> List (List Token) -> Problem
createProblem _ words =
    { cipherType = Atbash
    , instructions = "Atbash"
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
    25 - (Alpha.toVal char)
        |> Alpha.fromVal
        |> Alpha.toStr