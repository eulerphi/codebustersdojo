module Key exposing (Key, create, createK1, encode, list)

import Alpha exposing (Alpha)
import Data
import Dict exposing (Dict)
import DictEx
import Extra
import Interface exposing (Letter, RandomInput)
import ListEx
import Token

type Key = Key (Dict Int Int)

create : RandomInput -> Key
create randomInput =
    List.range 0 25
        |> ListEx.shuffle randomInput.hundred
        |> Maybe.withDefault (List.range 0 25)
        |> createHelper randomInput

createK1 : RandomInput -> Key
createK1 randomInput =
    randomInput.a
        |> Data.randomKeyword
        |> Token.tokenizeWord
        |> List.filterMap Token.tryGetTokenData
        |> List.map (\t -> t.char |> Alpha.toVal)
        |> (\xs -> xs ++ (List.range 0 25))
        |> ListEx.dedupe
        |> createHelper randomInput

createHelper : RandomInput -> List Int -> Key
createHelper randomInput shuffled =
    shuffled
        |> allMappings
        |> (\ms ->
            ListEx.itemAt
                (Extra.randomIdx randomInput.a (List.length ms))
                ms)
        |> Maybe.withDefault (Dict.empty)
        |> Key

encode : Alpha -> Key -> Alpha
encode plainText (Key d) =
    Alpha.toVal plainText
        |> (\val -> Dict.get val d)
        |> Maybe.withDefault 0
        |> Alpha.fromVal

list : Key -> List Letter
list (Key d) =
    let
        reverseMap = Dict.toList d
            |> List.map (\(k, v) -> (v, k))
            |> Dict.fromList
    in
    List.range 0 25
        |> List.map (\idx ->
            let
                plainVal = DictEx.getOrZero idx reverseMap
                cipherVal = idx
            in
            Interface.Interactive
                { idx = idx
                , group = Alpha.fromValToStr plainVal
                , plain = Alpha.fromValToStr plainVal
                , cipher = Alpha.fromValToStr cipherVal
                , guess = Nothing
                })

allMappings : List Int -> List (Dict Int Int)
allMappings cs =
    List.range 0 25
    |> List.filterMap (\idx -> tryCreateAt idx cs)

tryCreateAt : Int -> List Int -> Maybe (Dict Int Int)
tryCreateAt idx ps =
    let
        cs = (List.range idx 25) ++ (List.range 0 (idx - 1))
        mappings = List.map2 Tuple.pair ps cs
    in
    if List.all isValidSubstitution mappings then
        Just (Dict.fromList mappings)
    else
        Nothing

isValidSubstitution : (Int, Int) -> Bool
isValidSubstitution (p, c) =
    p /= c
