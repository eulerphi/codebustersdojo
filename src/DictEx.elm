module DictEx exposing (getOrZero, increment)

import Dict exposing (Dict)

getOrZero : comparable -> Dict comparable Int -> Int
getOrZero targetKey dict =
    Dict.get targetKey dict |> Maybe.withDefault 0

increment : comparable -> Dict comparable Int -> Dict comparable Int
increment targetKey dict =
    getOrZero targetKey dict
        |> (\val -> Dict.insert targetKey (val + 1) dict)
