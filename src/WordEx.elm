module WordEx exposing (..)

import Dict
import DictEx
import Interface exposing (Frequencies, Word)
import Interface exposing (tryGetLetterData)

frequencies : List Word -> Frequencies
frequencies ws =
    List.concatMap (\w -> w.letters) ws
        |> List.filterMap tryGetLetterData
        |> List.foldl
        (\l freq ->
            { plain = DictEx.increment l.plain freq.plain
            , cipher = DictEx.increment l.cipher freq.cipher
            })
        { plain = Dict.empty, cipher = Dict.empty }