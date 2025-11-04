module Extra exposing (
    combine, dedupe, equalsIgnoreCase, filterOut, getAt, randomIdx, randomInt)

import Set exposing (Set)

combine : List (Maybe a) -> Maybe (List a)
combine =
    List.foldr (Maybe.map2 (::)) (Just [])

dedupe : String -> String
dedupe source =
    dedupeHelper source Set.empty ""

dedupeHelper : String -> Set Char -> String -> String
dedupeHelper source seen output =
    case String.uncons source of
        Nothing ->
            output
        Just ( x, xs ) ->
            if Set.member x seen then
                dedupeHelper xs seen output
            else
                dedupeHelper xs (Set.insert x seen) (output ++ String.fromChar x)

equalsIgnoreCase : String -> String -> Bool
equalsIgnoreCase s1 s2 =
    (String.toUpper s1) == (String.toUpper s2)

filterOut : { source: String, exclude: String } -> String
filterOut input =
    let
        exclude_ = input.exclude |> String.toList |> Set.fromList
    in
    String.filter (\c -> not (Set.member c exclude_)) input.source

getAt : Int -> String -> Maybe Char
getAt idx str =
    if idx < 0 || idx >= String.length str then
        Nothing
    else
        String.slice idx (idx + 1) str
            |> String.uncons
            |> Maybe.map Tuple.first

randomIdx : Float -> Int -> Int
randomIdx rand length = randomInt rand 0 (length - 1)

randomInt : Float -> Int -> Int -> Int
randomInt rand lo hi =
    (rand * (hi - lo |> toFloat))
    |> round
    |> (+) lo