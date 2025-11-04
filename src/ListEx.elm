module ListEx exposing (indexOf)

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
