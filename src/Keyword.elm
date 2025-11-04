module Keyword exposing (
    Keyword, create, createOrDefault, getAt, toStr)

import Alpha exposing (Alpha)
import Array exposing (Array)
import Extra

type Keyword = Keyword (String, Array Alpha)

create : String -> Maybe Keyword
create str =
    if String.isEmpty str then
        Nothing
    else
        str
            |> String.toList
            |> List.map Alpha.parse
            |> Extra.combine
            |> Maybe.map Array.fromList
            |> Maybe.map (\cs -> Keyword(str, cs))

createOrDefault : String -> Keyword
createOrDefault str =
    str
        |> create
        |> Maybe.withDefault defaultKeyword

defaultKeyword : Keyword
defaultKeyword =
    [Alpha.fromVal 7, Alpha.fromVal 8]
        |> Array.fromList
        |> (\cs -> Keyword("HI", cs))

dummyAlpha : Alpha
dummyAlpha = Alpha.fromVal 0

getAt : Keyword -> Int -> Alpha
getAt (Keyword (_, cs)) idx =
    modBy (Array.length cs) idx
        |> (\i -> Array.get i cs)
        |> Maybe.withDefault dummyAlpha

toStr : Keyword -> String
toStr (Keyword (str, _)) = str |> String.toUpper