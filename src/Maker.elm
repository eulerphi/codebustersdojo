module Maker exposing (createProblem)

import Interface exposing (..)
import Affine
import Extra
import Array 
import Atbash
import Caesar
import Baconian
import Token exposing (Token)
import Nihilist
import Data
import Porta
import Aristocrat

createProblem : Cipher -> RandomInput -> Problem
createProblem cipher randomInput =
    (getCreateProblemFn cipher randomInput)
        randomInput 
        (randomInput.a |> Data.randomQuote |> Token.tokenize)

getCreateProblemFn : Cipher -> RandomInput -> (RandomInput -> List (List Token) -> Problem)
getCreateProblemFn cipher randomInput =
    case cipher of
        RandomCipher -> randomCreateProblemFn randomInput
        Affine -> Affine.createProblem
        Aristocrat -> Aristocrat.createProblem
        Atbash -> Atbash.createProblem
        Baconian -> Baconian.createProblem
        Caesar -> Caesar.createProblem
        Nihilist -> Nihilist.createProblem
        Porta -> Porta.createProblem

randomCreateProblemFn : RandomInput -> (RandomInput -> List (List Token) -> Problem)
randomCreateProblemFn randomInput =
    Extra.randomIdx randomInput.a (Array.length allCiphers)
        |> (\idx -> Array.get idx allCiphers)
        |> Maybe.map (\cipher -> getCreateProblemFn cipher randomInput)
        |> Maybe.withDefault (getCreateProblemFn Affine randomInput)