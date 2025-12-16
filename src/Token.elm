module Token exposing (Token(..), TokenData, tokenize, tokenizeWord, tryGetTokenData)

import Alpha exposing (Alpha)

type Token
    = Interactive TokenData
    | Punctuation { char : String }

type alias TokenData = { idx : Int, char : Alpha }

tokenize : String -> List (List Token)
tokenize str = str
    |> String.toList
    |> List.filter isAllowChar
    |> List.map Char.toUpper
    |> (\input -> tokenizeHelper input 0 [] [])

tokenizeWord : String -> List Token
tokenizeWord str =
    tokenize str |> List.head |> Maybe.withDefault [] 

tryGetTokenData : Token -> Maybe TokenData
tryGetTokenData t =
    case t of
        Interactive d -> Just d
        Punctuation _ -> Nothing

isAllowChar : Char -> Bool
isAllowChar c =
    Char.isAlpha c
    || isPunctuation c
    || c == ' '

isPunctuation : Char -> Bool
isPunctuation c =
    c == '.'
    || c == '!'
    || c == '?'
    || c == ','
    || c == ':'
    || c == ';'
    || c == '-'
    || c == '\''

tokenizeHelper : List Char -> Int -> List Token -> List (List Token) -> List (List Token)
tokenizeHelper input currentIdx currentWord output =
    case input of
        [] ->
            if List.isEmpty currentWord then
                output
            else
                output ++ [currentWord]
        ' '::xs ->
            if not (List.isEmpty currentWord) then
                tokenizeHelper xs currentIdx [] (output ++ [currentWord])
            else
                tokenizeHelper xs currentIdx [] output
        x::xs ->
            if isPunctuation x then
                tokenizeHelper
                    xs
                    currentIdx
                    (currentWord ++ [Punctuation { char = String.fromChar x }])
                    output
            else case Alpha.parse x of
                Nothing ->
                    tokenizeHelper xs currentIdx currentWord output
                Just c ->
                    tokenizeHelper
                        xs
                        (currentIdx + 1)
                        (currentWord ++ [Interactive { idx = currentIdx, char = c }])
                        output