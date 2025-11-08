module ListEx exposing (indexOf, partitionAt)

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
