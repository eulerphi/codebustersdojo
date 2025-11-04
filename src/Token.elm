module Token exposing (Token, tokenize)

import Alpha exposing (Alpha)

type alias Token = { idx : Int, char : Alpha }

tokenize : String -> List (List Token)
tokenize str = str
    |> String.toList
    |> List.filter (\c -> Char.isAlpha c || c == ' ')
    |> List.map Char.toUpper
    |> (\input -> tokenizeHelper input 0 [] [])

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
            case Alpha.parse x of
                Nothing ->
                    tokenizeHelper xs currentIdx currentWord output
                Just c ->
                    tokenizeHelper
                        xs
                        (currentIdx + 1)
                        (currentWord ++ [{ idx = currentIdx, char = c }])
                        output