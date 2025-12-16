module Main exposing (..)

import Array
import Browser
import Browser.Events
import DictEx
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events
import Interface exposing (..)
import Json.Decode as Decode
import Maker
import Platform.Cmd as Cmd
import Platform.Cmd as Cmd
import Random
import Task
import Time

-- ##########
-- MODEL
-- ##########
type Model
    = Loading
    | Ready ReadyState

type Selected
    = ProblemLetter LetterData
    | TableLetter LetterData

type alias ReadyState =
    { cipherType : Cipher
    , hardMode : Bool
    , instructions : String
    , words : List Word
    , selected : Selected
    , table : Maybe FrequencyTable
    , attempts : Int
    , startTime : Maybe Time.Posix
    , endTime : Maybe Time.Posix
    , solved : Bool
    }

generateRandomInput : Random.Generator RandomInput
generateRandomInput =
    Random.list 102 (Random.float 0 1)
        |> Random.map Array.fromList
        |> Random.map (\xs ->
            { a = xs |> Array.get 0 |> Maybe.withDefault 0
            , b = xs |> Array.get 1 |> Maybe.withDefault 0
            , hundred = xs |> Array.slice 2 102 |> Array.toList
            })

init : Float -> (Model, Cmd Msg)
init _ = ( Loading, newProblemCmd { cipherType = RandomCipher, hardMode = False } )

newProblemCmd : ProblemInput -> Cmd Msg
newProblemCmd input = Random.generate (NewProblem input) generateRandomInput

main : Program Float Model Msg
main = Browser.element { init = init , subscriptions = subscriptions , update = update , view = view }

-- ###############
-- Msg / Sub / Update
-- ###############
type Msg
    = Reset
    | Submit
    | InitNewProblem ProblemInput
    | KeyDown Int
    | NewProblem ProblemInput RandomInput
    | Select Selected
    | SetStartTime Time.Posix
    | SetEndTime Time.Posix
    | ToggleHardMode Bool

subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown (Decode.map KeyDown Html.Events.keyCode)

update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
  case msg of
    Reset -> case m of
        Loading -> (m, Cmd.none)
        Ready s ->
            (Ready { s | words = mutate clearGuess s.words }, Cmd.none)

    KeyDown code -> case m of
        Loading -> (m, Cmd.none)
        Ready s -> (Ready (code |> toKeyOp |> onKeyDown s), Cmd.none)

    InitNewProblem input ->
        (Loading, Random.generate (NewProblem input) generateRandomInput)

    NewProblem problemInput randomInput ->
        let
            p = Maker.createProblem problemInput randomInput
        in
        (Ready
            { cipherType = p.cipherType
            , hardMode = problemInput.hardMode
            , instructions = p.instructions
            , words = p.words
            , selected = p.words
                |> List.concatMap (\w -> w.letters)
                |> List.filterMap tryGetLetterData
                |> List.head
                |> Maybe.withDefault invalidLetter
                |> ProblemLetter
            , table = p.table
            , attempts = 0
            , startTime = Nothing
            , endTime = Nothing
            , solved = False
            }
        , Task.perform SetStartTime Time.now)

    Submit -> case m of
        Loading -> (m, Cmd.none)
        Ready s ->
            let
                solved =
                    List.concatMap (\w -> w.letters) s.words
                    |> List.all isGuessCorrect
            in
            (Ready { s | solved = solved, attempts = s.attempts + 1 }
            , if solved then Task.perform SetEndTime Time.now else Cmd.none)
        

    Select selected -> case m of
        Loading -> (m, Cmd.none)
        Ready s -> ( Ready { s | selected = selected }, Cmd.none)

    SetStartTime p -> case m of
        Loading -> (m, Cmd.none)
        Ready s -> ( Ready { s | startTime = Just p }, Cmd.none)

    SetEndTime p -> case m of
        Loading -> (m, Cmd.none)
        Ready s -> ( Ready { s | endTime = Just p }, Cmd.none)

    ToggleHardMode checked -> case m of
        Loading -> ( Loading
                   , Random.generate
                        (NewProblem { cipherType = RandomCipher, hardMode = checked })
                        generateRandomInput)
        Ready s -> ( Loading
                   , Random.generate
                        (NewProblem { cipherType = s.cipherType, hardMode = checked })
                        generateRandomInput)


-- ############
-- KEYOP
-- ############
type KeyOp = Noop | Clear | Set (String) | Left | Right

onKeyDown : ReadyState -> KeyOp -> ReadyState
onKeyDown s op =
    case op of
        Noop -> s
        Clear -> onKeyDown (onKeyDownInner s Nothing) Left
        Set letter -> onKeyDown (onKeyDownInner s (Just letter)) Right
        Left ->
            case s.selected of
                ProblemLetter selected ->
                    getByIdx (selected.idx - 1) s.words
                        |> Maybe.map (\l_ -> { s | selected = ProblemLetter l_ })
                        |> Maybe.withDefault s
                TableLetter selected ->
                    s.table
                        |> Maybe.map (\t -> [{ letters = t.mappings }])
                        |> Maybe.andThen (getByIdx (selected.idx - 1))
                        |> Maybe.map (\l_ -> { s | selected = TableLetter l_ })
                        |> Maybe.withDefault s
        Right ->
            case s.selected of
                ProblemLetter selected ->
                    getByIdx (selected.idx + 1) s.words
                        |> Maybe.map (\l_ -> { s | selected = ProblemLetter l_ })
                        |> Maybe.withDefault s
                TableLetter selected ->
                    s.table
                        |> Maybe.map (\t -> [{ letters = t.mappings }])
                        |> Maybe.andThen (getByIdx (selected.idx + 1))
                        |> Maybe.map (\l_ -> { s | selected = TableLetter l_ })
                        |> Maybe.withDefault s

onKeyDownInner : ReadyState -> Maybe String -> ReadyState
onKeyDownInner s guess_ =
    let
        setProblemLetterIfMatch : LetterData -> LetterData
        setProblemLetterIfMatch d =
            case s.selected of
                ProblemLetter selected ->
                    if selected.idx == d.idx then
                        d |> setGuess guess_
                    else if not s.hardMode && selected.group == d.group then
                        d |> setGuess guess_
                    else
                        d
                TableLetter selected ->
                    if not s.hardMode && selected.group == d.group then
                        d |> setGuess guess_
                    else
                        d

        setTableLetterIfMatch : LetterData -> LetterData
        setTableLetterIfMatch d =
            case s.selected of
                ProblemLetter selected ->
                    if not s.hardMode && selected.group == d.group then
                        d |> setGuess guess_
                    else
                        d
                TableLetter selected ->
                    if selected.idx == d.idx then
                        d |> setGuess guess_
                    else if not s.hardMode && selected.group == d.group then
                        d |> setGuess guess_
                    else
                        d
        
        words_ = mutate setProblemLetterIfMatch s.words
        table_ = s.table
            |> Maybe.andThen (\t ->
                mutate setTableLetterIfMatch [{ letters = t.mappings }]
                |> List.head
                |> Maybe.map (\w ->
                    { mappings = w.letters, frequencies = t.frequencies }))
    in
    {s | words = words_, table = table_ }

toKeyOp : Int -> KeyOp
toKeyOp val =
    let
        keyChar = Char.fromCode val
    in
    if val == 8 || val == 46 then Clear
    else if val == 37 then Left
    else if val == 38 then Right
    else if val == 39 then Right
    else if val == 40 then Left
    else if Char.isAlpha keyChar then Set (keyChar |> Char.toUpper |> String.fromChar) else Noop

-- ################
-- VIEW
-- ################
view : Model -> Html Msg
view m =
    case m of
        Loading -> Html.main_ [] []
        Ready s -> viewMain s

viewMain : ReadyState -> Html Msg
viewMain s =
    Html.main_ []
        [ Html.div [ Attrs.class "navBar"]
            ((viewHardModeToggle s) :: (viewNavBarButtons s))
        , Html.div [ Attrs.class "topContainer"]
            [ Html.div [ Attrs.class "instructionsContainer" ] [ Html.text s.instructions ]
            , Html.div [ Attrs.class "problemContainer" ] (s.words |> List.map (viewWord s))
            , Html.div [ Attrs.class "frequencyContainer" ] [ viewFrequencyTable s ]
            , Html.div [ Attrs.class "btnContainer" ] (viewButtons s)
            , Html.div [ Attrs.class "infoContainer" ] (viewInfo s)
            ]
        ]

viewHardModeToggle : ReadyState -> Html Msg
viewHardModeToggle s =
    Html.div
        [Attrs.class "checkbox-wrapper-14"]
        [ Html.input
            [ Attrs.type_ "checkbox"
            , Attrs.id "hardModeToggle"
            , Attrs.class "switch"
            , Attrs.checked s.hardMode
            , Html.Events.onCheck ToggleHardMode
            ]
            []
        , Html.label [Attrs.for "hardModeToggle"] [ Html.text "Hard Mode"]
        ]

viewNavBarButtons : ReadyState -> List (Html Msg)
viewNavBarButtons s =
    allCiphers
        |> Array.map (\c -> Html.button
            [InitNewProblem { cipherType = c, hardMode = s.hardMode } |> Html.Events.onClick]
            [ Html.text ("New " ++ cipherToString c)])
        |> Array.toList

viewWord : ReadyState -> Word -> Html Msg
viewWord s w =
    Html.table [Attrs.class "word"]
        [ Html.tbody [] [ Html.tr [] (w.letters |> List.map (viewLetter s)) ] ]

viewLetter : ReadyState -> Letter -> Html Msg
viewLetter s l =
    case l of
        Interactive d -> viewInteractiveLetter s d 
        Punctuation d -> viewPunctuation s d.char

viewInteractiveLetter : ReadyState -> LetterData -> Html Msg
viewInteractiveLetter s l =
    let
        selectedClass =
            case s.selected of
                ProblemLetter selected ->
                    if selected.idx == l.idx then
                        "selected"
                    else if not s.hardMode && selected.group == l.group then
                        "ingroup"
                    else
                        ""
                TableLetter selected ->
                    if not s.hardMode && selected.group == l.group then
                        "ingroup"
                    else
                        ""
    in
    Html.td []
        [ Html.div [ Attrs.class "letterContainer"]
            [ Html.div
                [ Attrs.class "input"
                , Attrs.class selectedClass
                , Select (ProblemLetter l) |> Html.Events.onClick
                ]
                [ l.guess |> Maybe.withDefault "" |> Html.text ]
            , Html.div [ Attrs.class "cipherText" ] [ Html.text l.cipher ]
            ]
        ]

viewPunctuation : ReadyState -> String -> Html Msg
viewPunctuation _ char =
    Html.td []
        [ Html.div [ Attrs.class "letterContainer"]
            [ Html.div [ Attrs.class "punctuation"] [ Html.text char ]
            , Html.div [] []
            ]
        ]

viewFrequencyTable : ReadyState -> Html Msg
viewFrequencyTable s =
    case s.table of
        Nothing -> Html.div [] []
        Just t ->
            let
                ms = t.mappings |> List.filterMap tryGetLetterData

                topRow = ms |> List.map (\l ->
                    Html.td
                        [ Attrs.class "pt"]
                        [ Html.text l.cipher])

                midRow = ms |> List.map (\l ->
                    Html.td
                        []
                        [DictEx.getOrZero l.cipher t.frequencies.cipher
                            |> String.fromInt
                            |> Html.text])

                botRow = ms |> List.map (\l ->
                    let
                        selectedClass =
                            case s.selected of
                                ProblemLetter selected ->
                                    if not s.hardMode && selected.group == l.group then
                                        "ingroup"
                                    else
                                        ""
                                TableLetter selected ->
                                    if selected.idx == l.idx then
                                        "selected"
                                    else if not s.hardMode && selected.group == l.group then
                                        "ingroup"
                                    else
                                        ""
                    in
                    Html.td
                        [ Select (TableLetter l) |> Html.Events.onClick ]
                        [ Html.div
                            [ Attrs.class "input", Attrs.class selectedClass ]
                            [ Maybe.withDefault "" l.guess |> Html.text ]
                        ])
            in
            Html.div
                []
                [ Html.table
                    []
                    [ Html.tbody
                        []
                        [ Html.tr
                            []
                            ((Html.td [Attrs.class "topLeftCell"] []) :: topRow)
                        , Html.tr
                            []
                            ((Html.td [] [Html.text "Frequency"]) :: midRow)
                        , Html.tr
                            []
                            ((Html.td [] [ Html.text "Replacement"]) :: botRow)
                        ]
                    ]
                ]

viewButtons : ReadyState -> List (Html Msg)
viewButtons s =
    if s.solved then
        []
    else
        [ Html.button [ Attrs.class "checkBtn", Html.Events.onClick Submit ] [ Html.text "Check Solution" ]
        , Html.button [ Attrs.class "resetBtn", Html.Events.onClick Reset ] [ Html.text "Reset" ]
        ]

viewInfo : ReadyState -> List (Html Msg)
viewInfo s =
    let
        durationInSecs =
            Maybe.map2
                (\start end -> ((Time.posixToMillis end) - (Time.posixToMillis start)) // 1000)
                s.startTime
                s.endTime
            |> Maybe.withDefault 0
            |> String.fromInt
    in

    if s.solved then
        [ "🎉🎉 Solved! (Attempts: " ++ (s.attempts |> String.fromInt) ++ ", Time: " ++ (durationInSecs) ++ " seconds)" |> Html.text ]
    else if s.attempts > 0
        then
            [ "Not quite. (Attempts: " ++ (s.attempts |> String.fromInt) ++ ")" |> Html.text ]
        else
            []