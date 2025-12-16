module Aristocrat exposing (createProblem, createK1Problem)

import Alpha exposing (Alpha)
import Common exposing (..)
import Interface exposing (..)
import Token exposing (Token)
import Key exposing (Key)
import WordEx

createProblem : RandomInput -> List (List Token) -> Problem
createProblem randomInput words =
    let
        params = { key = Key.create randomInput }
        words_ = words
            |> List.map (\w ->
                { letters = w |> List.map (encryptLetter params) }) 
    in
    { cipherType = Aristocrat
    , instructions = "Aristocrat"
    , words = words_
    , table = Just
        { mappings = Key.list params.key
        , frequencies = WordEx.frequencies words_
        }
    }

createK1Problem : RandomInput -> List (List Token) -> Problem
createK1Problem randomInput words =
    let
        params = { key = Key.createK1 randomInput }
        words_ = words
            |> List.map (\w ->
                { letters = w |> List.map (encryptLetter params) }) 
    in
    { cipherType = AristocratK1
    , instructions = "Aristocrat (K1)"
    , words = words_
    , table = Just
        { mappings = Key.list params.key
        , frequencies = WordEx.frequencies words_
        }
    }

encryptLetter : Params -> Token -> Letter
encryptLetter params t =
    case t of
        Token.Interactive d ->
            Interface.Interactive
                { idx = d.idx
                , group = d.char |> Alpha.toStr
                , plain = d.char |> Alpha.toStr
                , cipher = encrypt params d.char
                , guess = Nothing
                }
        Token.Punctuation d ->
            Interface.Punctuation d

encrypt : Params -> Alpha -> String
encrypt params char =
    Key.encode char params.key
        |> Alpha.toStr

type alias Params = { key : Key }