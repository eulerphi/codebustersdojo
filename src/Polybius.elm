module Polybius exposing (
    Polybius, create, encode, toKeywordStr)

import Alpha exposing (Alpha)
import Extra
import Keyword exposing (Keyword)
import ListEx
import Token exposing (Token)

type Polybius = Polybius (String, List Alpha)

create : String -> Polybius
create keyword = 
    let
        keyword_ = keyword
            |> String.filter Char.isAlpha
            |> String.toLower
            |> String.replace "j" "i"
            |> Extra.dedupe

        alphabet = Extra.filterOut
            { source = "abcdefghiklmnopqrstuvwxyz"
            , exclude = keyword_
            }
    in
    (keyword_ ++ alphabet)
        |> String.toList
        |> List.map Alpha.parse
        |> Extra.combine
        |> Maybe.map (\cs -> Polybius (keyword, cs))
        |> Maybe.withDefault defaultPolybius

defaultPolybius : Polybius
defaultPolybius =
    List.range 0 25
        |> List.map Alpha.fromVal
        |> (\cs -> Polybius("ABC", cs))

encode : Polybius -> Keyword -> Token -> Int
encode p k t =
    let
        kVal = Keyword.getAt k t.idx
            |> toVal p
        tVal = t.char |> toVal p
    in
    kVal + tVal

iChar : Alpha
iChar = Alpha.fromVal 8

jChar : Alpha
jChar = Alpha.fromVal 9

toKeywordStr : Polybius -> String
toKeywordStr (Polybius (kw, _)) =
    kw |> String.toUpper

toVal : Polybius -> Alpha -> Int
toVal (Polybius (_, cs)) char =
    let
        char_ = if Alpha.eq char jChar then iChar else char
    in
    case ListEx.indexOf (Alpha.eq char_) cs of
        Nothing -> 1000
        Just idx ->
            let
                tens = 10 * (idx // 5 + 1)
                ones = modBy 5 idx |> (+) 1

                -- _ = Debug.log "p:" (cs |> List.map Alpha.toStr |> String.join "")
                -- _ = Debug.log "(c, idx)" ("(" ++ (Alpha.toStr char) ++ ", " ++ (String.fromInt idx) ++ ")")
            in
            tens + ones
