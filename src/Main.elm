module Main exposing (..)

import Array
import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events
import Maker
import Platform.Cmd as Cmd
import Json.Decode as Decode
import Interface exposing (..)
import Random
import Platform.Cmd as Cmd
import Extra

-- ##########
-- MODEL
-- ##########
type Model
    = Loading
    | Ready ReadyState

type alias ReadyState =
    { cipherType : Cipher
    , hardMode : Bool
    , instructions : String
    , words : List Word
    , selected : Letter
    , attempts : Int
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
    | Select Letter
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
                |> List.head
                |> Maybe.andThen (\w -> w.letters |> List.head)
                |> Maybe.withDefault invalidLetter
            , attempts = 0
            , solved = False
            }
        , Cmd.none)

    Submit -> case m of
        Loading -> (m, Cmd.none)
        Ready s ->
            let
                solved = s.words |> List.all (\w ->
                    w.letters |> List.all (\l ->
                        l.guess
                            |> Maybe.map (Extra.equalsIgnoreCase l.plain)
                            |> Maybe.withDefault False))
            in
            (Ready { s | solved = solved, attempts = s.attempts + 1 }, Cmd.none)
        

    Select idx -> case m of
        Loading -> (m, Cmd.none)
        Ready s -> ( Ready { s | selected = idx }, Cmd.none)

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
            case getByIdx (s.selected.idx - 1) s.words of
                Nothing -> s
                Just l -> { s | selected = l}
        Right ->
            case getByIdx (s.selected.idx + 1) s.words of
                Nothing -> s
                Just l -> { s | selected = l}

onKeyDownInner : ReadyState -> Maybe String -> ReadyState
onKeyDownInner s guess_ =
    let
        setIfMatch : Letter -> Letter
        setIfMatch l =
            if s.selected.idx == l.idx then
                l |> setGuess guess_
            else if not s.hardMode && s.selected.group == l.group then
                l |> setGuess guess_
            else
                l
    in
    {s | words = mutate setIfMatch s.words }

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
    let
        selectedClass =
            if s.selected.idx == l.idx then
                "selected"
            else if not s.hardMode && s.selected.group == l.group then
                "ingroup"
            else
                ""
    in
    Html.td []
        [ Html.div [ Attrs.class "letterContainer"]
            [ Html.div
                [ Attrs.class "input"
                , Attrs.class selectedClass
                , Select l |> Html.Events.onClick
                ]
                [ l.guess |> Maybe.withDefault "" |> Html.text ]
            , Html.div [ Attrs.class "cipherText" ] [ Html.text l.cipher ]
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
    if s.solved then
        [ "🎉🎉 Solved! (Attempts: " ++ (s.attempts |> String.fromInt) ++ ")" |> Html.text ]
    else if s.attempts > 0
        then
            [ "Not quite. (Attempts: " ++ (s.attempts |> String.fromInt) ++ ")" |> Html.text ]
        else
            []