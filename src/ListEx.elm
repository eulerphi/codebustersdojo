module ListEx exposing (dedupe, indexOf, itemAt, partitionAt, shuffle)

import Extra
import Set exposing (Set)

dedupe : List comparable -> List comparable
dedupe items =
    dedupeHelper items Set.empty []

dedupeHelper : List comparable -> Set comparable -> List comparable -> List comparable
dedupeHelper input seen output =
    case input of
        [] -> output
        x::xs ->
            if Set.member x seen then
                dedupeHelper xs seen output
            else
                dedupeHelper xs (Set.insert x seen) (output ++ [x])

indexOf : (a -> Bool) -> List a -> Maybe Int
indexOf fn items =
    indexOfHelper fn items 0

indexOfHelper : (a -> Bool) -> List a -> Int -> Maybe Int
indexOfHelper fn items currentIdx =
    case items of
        [] -> Nothing
        x::xs ->
            if fn(x) then
                Just currentIdx
            else
                indexOfHelper fn xs (currentIdx + 1)

itemAt : Int -> List a -> Maybe a
itemAt idx items =
    case items of
        [] -> Nothing
        x::xs ->
            if idx == 0 then
                Just x
            else
                itemAt (idx - 1) xs

partitionAt : Int -> List a -> Maybe (List a, a, List a)
partitionAt idx items =
    if idx >= 0 then
        partitionAtHelper idx items []
    else
        Nothing

partitionAtHelper : Int -> List a -> List a -> Maybe (List a, a, List a)
partitionAtHelper idx items before =
    case items of
        [] -> Nothing
        x::xs -> if idx == 0
            then Just (List.reverse before, x, xs)
            else partitionAtHelper (idx - 1) xs (x :: before)

shuffle : List Float -> List a -> Maybe (List a)
shuffle randoms items =
    shuffleHelper randoms items []

shuffleHelper : List Float -> List a -> List a -> Maybe (List a)
shuffleHelper randoms items output =
    if List.isEmpty items then
        Just output
    else
        case randoms of
            [] -> Nothing
            r::rs -> Extra.randomIdx r (List.length items)
                |> (\idx -> partitionAt idx items)
                |> Maybe.andThen (\(before, x, after) ->
                    shuffleHelper rs (before ++ after) (output ++ [x]))

        

