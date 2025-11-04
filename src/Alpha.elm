module Alpha exposing
    (Alpha, eq, fromVal, parse, toChar, toStr, toVal)

type Alpha = Alpha Char

eq : Alpha -> Alpha -> Bool
eq (Alpha x) (Alpha y) = x == y

fromVal : Int -> Alpha
fromVal idx =
    (modBy 26 idx) + lowerACode
        |> Char.fromCode
        |> Alpha

parse : Char -> Maybe Alpha
parse c =
    if Char.isAlpha c then
        c
            |> Char.toLower
            |> Alpha
            |> Just
    else
        Nothing

toChar : Alpha -> Char
toChar (Alpha c) = c

toVal : Alpha -> Int
toVal (Alpha c) =
    toValHelper c |> Maybe.withDefault -1

toValHelper : Char -> Maybe Int
toValHelper c =
    let
        code = c |> Char.toLower |> Char.toCode
    in
    if code >= lowerACode && code <= lowerZCode then
        Just (code - lowerACode)
    else
        Nothing

toStr : Alpha -> String
toStr (Alpha c) = c |> String.fromChar

lowerACode : Int
lowerACode = 'a' |> Char.toCode

lowerZCode : Int
lowerZCode = 'z' |> Char.toCode