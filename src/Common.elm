module Common exposing (toLetterIndex, toLetterIndexUnsafe, fromLetterIndex)

toLetterIndex : Char -> Maybe Int
toLetterIndex letter =
    let
        code = letter |> Char.toLower |> Char.toCode
    in
    if code >= letterACode && code <= letterZCode
    then Just (code - letterACode)
    else Nothing

toLetterIndexUnsafe : Char -> Int
toLetterIndexUnsafe letter =
    toLetterIndex letter |> Maybe.withDefault -1

fromLetterIndex : Int -> Char
fromLetterIndex idx =
    (modBy 26 idx) + letterACode |> Char.fromCode

letterACode : Int
letterACode = 'a' |> Char.toCode

letterZCode : Int
letterZCode = 'z' |> Char.toCode
