module Interface exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Extra
import Token exposing (Token(..))

type Cipher
    = RandomCipher
    | Affine
    | Aristocrat
    | AristocratK1
    | Atbash
    | Baconian
    | Caesar
    | Nihilist
    | Porta

type Letter
    = Interactive LetterData
    | Punctuation { char : String }

type alias LetterData =
    { idx : Int
    , group : String
    , plain : String
    , cipher : String
    , guess : Maybe String
    }

invalidLetter : LetterData
invalidLetter =
    { idx = -1
    , group = ""
    , plain = ""
    , cipher = ""
    , guess = Nothing
    }

clearGuess : LetterData -> LetterData
clearGuess = setGuess Nothing

isGuessCorrect : Letter -> Bool
isGuessCorrect l =
    case l of
        Interactive d ->
            case d.guess of
                Just g -> Extra.equalsIgnoreCase d.plain g
                Nothing -> False
        Punctuation _ -> True

setGuess : Maybe String -> LetterData -> LetterData
setGuess guess_ d = { d | guess = guess_}

tryGetLetterData : Letter -> Maybe LetterData
tryGetLetterData l =
    case l of
        Interactive d -> Just d
        Punctuation _ -> Nothing

getByIdx : Int -> List Word -> Maybe LetterData
getByIdx idx words =
    case words of
        [] -> Nothing
        w::ws -> case (getByIdxInWord idx w.letters) of
            Nothing -> getByIdx idx ws
            Just d -> Just d

getByIdxInWord : Int -> List Letter -> Maybe LetterData
getByIdxInWord idx letters =
    case letters of
        [] -> Nothing
        l::ls -> case l of
            Interactive d ->
                if d.idx == idx then
                    Just d
                else
                    getByIdxInWord idx ls
            Punctuation _ -> 
                getByIdxInWord idx ls

mutate : (LetterData -> LetterData) -> List Word -> List Word
mutate fn words =
    words |> List.map (mutateWord fn)

mutateWord : (LetterData -> LetterData) -> Word -> Word
mutateWord fn w =
    { w | letters = w.letters |> List.map (mutateLetter fn) }

mutateLetter : (LetterData -> LetterData) -> Letter -> Letter
mutateLetter fn l =
    case l of
        Interactive d -> Interactive (fn d)
        Punctuation d -> Punctuation d

type alias Problem =
    { cipherType : Cipher
    , instructions : String
    , words : List Word
    , table : Maybe FrequencyTable
    }

type alias ProblemInput =
    { cipherType : Cipher
    , hardMode : Bool
    }

type alias Frequencies =
    { plain : Dict String Int
    , cipher : Dict String Int
    }

type alias FrequencyTable =
    { mappings : List Letter
    , frequencies : Frequencies
    }

type alias RandomInput =
    { a : Float
    , b : Float
    , hundred : List Float
    }

type alias Word = { letters : List Letter }

allCiphers : Array Cipher
allCiphers = [Affine, Aristocrat, AristocratK1, Atbash, Baconian, Caesar, Nihilist, Porta] |> Array.fromList

cipherToString : Cipher -> String
cipherToString cipher = case cipher of
    RandomCipher -> "Random"
    Affine -> "Affine"
    Aristocrat -> "Aristocrat"
    AristocratK1 -> "Aristocrat K1"
    Atbash -> "Atbash"
    Baconian -> "Baconian"
    Caesar -> "Caesar"
    Nihilist -> "Nihilist"
    Porta -> "Porta"

