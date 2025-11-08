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
    25 - (Alpha.toVal char)
        |> Alpha.fromVal
        |> Alpha.toStr