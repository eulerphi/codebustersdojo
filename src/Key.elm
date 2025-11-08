module Key exposing (Key, create, encode)

import Alpha exposing (Alpha)
import Dict exposing (Dict)
import Interface exposing (RandomInput)
import Extra
import ListEx

type Key = Key (Dict Int Int)

create : RandomInput -> Key
create randomInput =
    createHelper randomInput.hundred (List.range 0 25) Dict.empty
        |> Maybe.withDefault (Dict.empty)
        |> Key

encode : Alpha -> Key -> Alpha
encode plainText (Key d) =
    Alpha.toVal plainText
        |> (\val -> Dict.get val d)
        |> Maybe.withDefault 0
        |> Alpha.fromVal

createHelper : List Float -> List Int -> Dict Int Int -> Maybe (Dict Int Int)
createHelper randoms remaining output =
    if List.isEmpty remaining then
        Just output
    else
        case randoms of
            [] -> Nothing
            r::rs -> Extra.randomIdx r (List.length remaining)
                |> (\idx -> ListEx.partitionAt idx remaining)
                |> Maybe.andThen (\(before, x, after) ->
                    createHelper
                        rs
                        (before ++ after)
                        (Dict.insert x (Dict.size output) output))