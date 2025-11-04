module Interface exposing (..)

import Array exposing (Array)

type Cipher = RandomCipher | Affine | Atbash | Baconian | Caesar | Nihilist | Porta

type alias Letter =
    { idx : Int
    , group : String
    , plain : String
    , cipher : String
    , guess : Maybe String
    }

invalidLetter : Letter
invalidLetter =
    { idx = -1
    , group = ""
    , plain = ""
    , cipher = ""
    , guess = Nothing
    }

type alias Selection =
    { idx : Int
    , group : String
    }

clearGuess : Letter -> Letter
clearGuess = setGuess Nothing

setGuess : Maybe String -> Letter -> Letter
setGuess guess_ l = { l | guess = guess_ }

getByIdx : Int -> List Word -> Maybe Letter
getByIdx idx words =
    case words of
        [] -> Nothing
        x::xs -> case x.letters |> List.filter (\l -> l.idx == idx) |> List.head of
            Nothing -> getByIdx idx xs
            Just l -> Just l

getSelection : Letter -> Selection
getSelection l =
    { idx = l.idx, group = l.group }

mutate : (Letter -> Letter) -> List Word -> List Word
mutate fn words =
    words |> List.map (\w -> { w | letters = w.letters |> List.map fn })

type alias Problem =
    { cipherType : Cipher
    , instructions : String
    , words : List Word
    }

type alias RandomInput =
    { a : Float
    , b : Float
    , c : Float
    , d : Float
    , e : Float
    }

type alias Word = { letters : List Letter }

allCiphers : Array Cipher
allCiphers = [Affine, Atbash, Baconian, Caesar, Nihilist] |> Array.fromList

cipherToString : Cipher -> String
cipherToString cipher = case cipher of
    RandomCipher -> "Random"
    Affine -> "Affine"
    Atbash -> "Atbash"
    Baconian -> "Baconian"
    Caesar -> "Caesar"
    Nihilist -> "Nihilist"
    Porta -> "Porta"

