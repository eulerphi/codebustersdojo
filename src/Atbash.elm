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
    let
        cipher = encrypt t.char
    in
    { idx = t.idx
    , group = cipher
    , plain = t.char |> Alpha.toStr
    , cipher = cipher
    , guess = Nothing
    }

encrypt : Alpha -> String
encrypt char =
    25 - (Alpha.toVal char)
        |> Alpha.fromVal
        |> Alpha.toStr

-- createProblem : RandomInput -> String -> Maybe Problem
-- createProblem _ plainText =
--     plainText
--         |> String.split " "
--         |> List.indexedMap encryptToWord
--         |> Extra.combine
--         |> Maybe.map (\ws ->
--             { cipherType = Atbash
--             , instructions = "Atbash"
--             , words = ws
--             })

-- encryptToWord : Int -> String -> Maybe Word
-- encryptToWord wordIdx letters =
--     letters
--         |> String.toList
--         |> List.indexedMap (\letterIdx -> encryptToLetter (wordIdx, letterIdx))
--         |> Extra.combine
--         |> Maybe.map (\ls -> { idx = wordIdx , letters = ls })

-- encryptToLetter : (Int, Int) -> Char -> Maybe Letter
-- encryptToLetter idx letter =
--     encrypt letter
--         |> Maybe.map (\ct ->
--             { idx = idx
--             , plain = String.fromChar letter
--             , cipher = ct
--             , guess = Nothing
--             })

-- encrypt : Char -> Maybe String
-- encrypt letter =
--     Common.toLetterIndex letter
--         |> Maybe.map (\idx -> 25 - idx)
--         |> Maybe.andThen Common.fromLetterIndex
--         |> Maybe.map String.fromChar