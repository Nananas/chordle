module Training exposing (..)

import Backend
import Browser.Dom
import Browser.Events
import Common
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Lazy
import Help
import Html.Attributes
import Icons
import Json.Decode
import Json.Encode
import List.Extra as List
import MobileUI
import Process
import Random
import Set
import Storage
import String.Extra as String
import Task
import Time
import UI
import Utils exposing (..)
import WordChain exposing (WordChain)
import Words exposing (..)


type Msg
    = OnStorageLoaded Storage.Storage
    | NewWordChain WordChain
    | InputHanzi String
    | UserPressedEnter
    | UserPressedCtrlEnter
    | ToNextWord
    | OnGiveUpClicked
    | OnToggleHelp
    | OnToggleScreenKeyboard
    | OnToggleDictionaryModal
    | OnToggleDictionaryShowAllWords
    | OnRestartClick
      --
    | OnMouseEnterCharacter ( Int, Int )
    | OnMouseLeaveCharacter
    | OnTick Time.Posix
      --
    | OnActivity Time.Posix
      --
    | NoOp
    | NoOpString String
    | KeyboardInput String
    | KeyboardBackspace
    | KeyboardClear
      --
    | OnClickedHome
      --
    | BackendMsg Backend.Msg


type Model
    = LoadingGameStatsFromStorage
    | LoadingWordsFoundFromStorage GameStats
    | Initializing (List String) GameStats
    | Ready GameModel
    | GameFinished GameModel


type alias GameModel =
    { wordsFound : List String
    , gameStats : GameStats

    --
    , wordChain : WordChain
    , answers : List PinyinPart
    , currentInput : String
    , errorMsg : Maybe String
    , gameState : GameState

    --
    , showPopupForCharacter : Maybe ( Int, Int )
    , showTodoUpdate : Maybe Int
    , showTodoUpdateTimer : Int
    , showHelp : Bool
    , showDictionaryModal : Bool
    , unhideDictionary : Bool

    --
    , useScreenKeyboardOnMobile : Bool
    , keyboardKeyFeedback : Maybe String

    --
    , lastActivity : Time.Posix
    }


type GameState
    = NotDone
    | TooManyWrongAnswers
    | FilledInCorrectly


type alias GameStats =
    { correct : Int
    , attempts : Int
    , retries : Int
    }


inactivityTime =
    30 * 60 * 1000


idNextButton =
    "next-btn"


{-| Used by wordchain generator
-}
amount =
    ( 2, 4 )


defaultGameStats =
    { correct = 0, attempts = 0, retries = 0 }


encodeGameStats gameStats =
    Json.Encode.object
        [ ( "correct", Json.Encode.int gameStats.correct )
        , ( "attempts", Json.Encode.int gameStats.attempts )
        , ( "retries", Json.Encode.int gameStats.retries )
        ]


gameStatsDecoder =
    Json.Decode.map3
        GameStats
        (Json.Decode.field "correct" Json.Decode.int)
        (Json.Decode.field "attempts" Json.Decode.int)
        (Json.Decode.field "retries" Json.Decode.int)


withDictionarySize size stats =
    { stats | dictionarySize = size }


withAddAttempts attemptsDelta stats =
    { stats | attempts = attemptsDelta + stats.attempts }


withAddOneCorrect stats =
    { stats | correct = stats.correct + 1 }


withAddRetries retriesDelta stats =
    { stats | retries = retriesDelta + stats.retries }



--


init : ( Model, Cmd Msg )
init =
    ( LoadingGameStatsFromStorage, Storage.loadStorage "game-stats" )


gameOver uuid activeDicts game =
    let
        newGameStats =
            game.gameStats
                |> withAddRetries (List.length game.wordChain)
                |> withAddAttempts (List.length game.wordChain)
    in
    ( Ready
        { game
            | gameState = TooManyWrongAnswers
            , showTodoUpdate = Just (List.length game.wordChain)
            , showTodoUpdateTimer = 2
            , gameStats = newGameStats
        }
    , Cmd.batch
        [ Storage.setStorage <| { name = "game-stats", json = encodeGameStats newGameStats }
        , roundFinishedPost uuid False activeDicts game
        ]
    )


toNextWord uuid activeDicts dictionaries game =
    let
        a =
            game.wordsFound
                |> Set.fromList

        b =
            dictionaries
                |> allWords activeDicts
                |> List.map (\w -> List.map .hanzi w.characters |> String.join "")
                |> Set.fromList
    in
    --if List.length game.wordsFound >= List.length (allWords game.dictsActive) then
    if (Set.intersect a b |> Set.size) == Set.size b then
        gameFinished uuid activeDicts game

    else
        let
            dictionary =
                dictionaries
                    |> allWords activeDicts
                    |> List.filter (\word -> not <| List.member (wordHanzi word) game.wordsFound)
        in
        ( Ready game
        , Cmd.batch
            [ Random.generate NewWordChain (WordChain.singleChainGenerator amount dictionary)
            , Browser.Dom.focus Common.idInput
                |> Task.attempt (\_ -> NoOp)
            ]
        )



--


update : Backend.Uuid -> Words.Dictionaries -> List String -> Msg -> Model -> ( Model, Cmd Msg )
update uuid dictionaries activeDicts msg model =
    case ( model, msg ) of
        ( LoadingGameStatsFromStorage, OnStorageLoaded storage ) ->
            let
                gameStats =
                    storage.json
                        |> Json.Decode.decodeValue gameStatsDecoder
            in
            case gameStats of
                Err _ ->
                    ( LoadingWordsFoundFromStorage defaultGameStats, Storage.loadStorage "words-found" )

                Ok stats ->
                    ( LoadingWordsFoundFromStorage stats, Storage.loadStorage "words-found" )

        ( LoadingWordsFoundFromStorage gameStats, OnStorageLoaded storage ) ->
            let
                wordsFound : List String
                wordsFound =
                    storage.json
                        |> Json.Decode.decodeValue (Json.Decode.list Json.Decode.string)
                        |> Result.toMaybe
                        |> Maybe.withDefault []

                --wordsFoundDebug =
                --allWords Dict.empty |> List.drop 1
                --|> Debug.log "Debug stuff"
            in
            if List.length wordsFound >= List.length (allWords activeDicts dictionaries) then
                gameFinished
                    uuid
                    activeDicts
                    { wordsFound = wordsFound
                    , wordChain = []
                    , answers = []
                    , currentInput = ""
                    , errorMsg = Nothing
                    , gameState = NotDone
                    , showPopupForCharacter = Nothing
                    , showTodoUpdate = Nothing
                    , showTodoUpdateTimer = 0
                    , showHelp = False
                    , showDictionaryModal = False
                    , unhideDictionary = False
                    , lastActivity = Time.millisToPosix 0
                    , useScreenKeyboardOnMobile = True
                    , keyboardKeyFeedback = Nothing
                    , gameStats = gameStats
                    }

            else
                let
                    dictionary =
                        allWords activeDicts dictionaries
                            |> List.filter
                                (\word -> not <| List.member (wordHanzi word) wordsFound)
                in
                ( Initializing wordsFound gameStats
                , Random.generate NewWordChain (WordChain.singleChainGenerator amount dictionary)
                )

        ( Initializing wordsFound gameStats, NewWordChain wordChain ) ->
            ( Ready
                { wordsFound = wordsFound
                , wordChain = wordChain
                , answers = []
                , currentInput = ""
                , errorMsg = Nothing
                , gameState = NotDone
                , showPopupForCharacter = Nothing
                , showTodoUpdate = Nothing
                , showTodoUpdateTimer = 0
                , showHelp = False
                , showDictionaryModal = False
                , unhideDictionary = False
                , lastActivity = Time.millisToPosix 0
                , useScreenKeyboardOnMobile = True
                , keyboardKeyFeedback = Nothing
                , gameStats = gameStats
                }
            , Cmd.batch
                [ Storage.setStorage
                    { name = "words-found"
                    , json =
                        Json.Encode.list
                            Json.Encode.string
                            wordsFound
                    }
                , Process.sleep 100
                    |> Task.andThen (\_ -> Browser.Dom.focus Common.idInput)
                    |> Task.attempt (\_ -> NoOp)
                ]
            )

        ( Ready game, OnActivity now ) ->
            ( Ready { game | lastActivity = now }, Cmd.none )

        ( Ready game, NewWordChain wordChain ) ->
            ( Ready
                { game
                    | wordChain = wordChain
                    , answers = []
                    , currentInput = ""
                    , errorMsg = Nothing
                    , gameState = NotDone
                    , showPopupForCharacter = Nothing
                    , showTodoUpdate = Nothing
                    , showTodoUpdateTimer = 0
                    , showHelp = False
                }
            , Cmd.none
            )

        ( Ready game, InputHanzi txt ) ->
            ( Ready
                { game | currentInput = txt, errorMsg = Nothing }
            , Cmd.none
            )

        ( Ready game, KeyboardInput char ) ->
            ( Ready { game | currentInput = game.currentInput ++ char, errorMsg = Nothing }, Cmd.none )

        ( Ready game, KeyboardBackspace ) ->
            ( Ready { game | currentInput = String.dropRight 1 game.currentInput, errorMsg = Nothing }, Cmd.none )

        ( Ready game, KeyboardClear ) ->
            ( Ready { game | currentInput = "", errorMsg = Nothing }, Cmd.none )

        ( Ready game, OnToggleScreenKeyboard ) ->
            ( Ready { game | useScreenKeyboardOnMobile = not game.useScreenKeyboardOnMobile }, Cmd.none )

        ( Ready game, UserPressedEnter ) ->
            if game.currentInput == "" then
                ( Ready game, Cmd.none )

            else
                let
                    txt =
                        String.toCodePoints game.currentInput
                            |> List.filter (\code -> code <= 126)
                            |> String.fromCodePoints

                    ( newModel, cmd ) =
                        processInput txt game
                            |> processRoundFinished uuid activeDicts
                in
                ( Ready newModel, cmd )

        ( Ready game, UserPressedCtrlEnter ) ->
            case game.gameState of
                NotDone ->
                    gameOver uuid activeDicts game

                _ ->
                    toNextWord uuid activeDicts dictionaries game

        ( Ready game, ToNextWord ) ->
            toNextWord uuid activeDicts dictionaries game

        ( Ready game, OnGiveUpClicked ) ->
            gameOver uuid activeDicts game

        ( Ready game, OnRestartClick ) ->
            restartGame dictionaries activeDicts game

        ( GameFinished game, OnRestartClick ) ->
            restartGame dictionaries activeDicts game

        ( Ready game, OnToggleHelp ) ->
            ( Ready
                { game
                    | showHelp = not game.showHelp
                    , showDictionaryModal = False
                }
            , Cmd.none
            )

        ( Ready game, OnToggleDictionaryModal ) ->
            ( Ready
                { game
                    | showDictionaryModal = not game.showDictionaryModal
                    , showHelp = False
                }
            , Cmd.none
            )

        ( Ready game, OnToggleDictionaryShowAllWords ) ->
            ( Ready { game | unhideDictionary = not game.unhideDictionary }, Cmd.none )

        --
        ( Ready game, OnMouseEnterCharacter id ) ->
            ( Ready { game | showPopupForCharacter = Just id }, Cmd.none )

        ( Ready game, OnMouseLeaveCharacter ) ->
            ( Ready { game | showPopupForCharacter = Nothing }, Cmd.none )

        ( Ready game, OnTick _ ) ->
            let
                withUpdateShowTodo m =
                    if m.showTodoUpdateTimer > 0 then
                        { m | showTodoUpdateTimer = m.showTodoUpdateTimer - 1 }

                    else
                        { m | showTodoUpdate = Nothing }
            in
            ( Ready (withUpdateShowTodo game), Cmd.none )

        ( GameFinished _, _ ) ->
            ( model, Cmd.none )

        --
        ( LoadingGameStatsFromStorage, _ ) ->
            ( model, Cmd.none )

        ( LoadingWordsFoundFromStorage _, _ ) ->
            ( model, Cmd.none )

        ( Initializing _ _, _ ) ->
            ( model, Cmd.none )

        ( Ready _, _ ) ->
            ( model, Cmd.none )


onActivity =
    Task.perform OnActivity Time.now


liftModel fn ( model, cmd ) =
    ( fn model, cmd )


restartGame dictionaries activeDicts model =
    let
        newGameStats =
            { correct = 0
            , attempts = 0
            , retries = 0
            }
    in
    ( Ready
        { model
            | gameState = NotDone
            , wordsFound = []
            , gameStats = newGameStats
        }
    , Cmd.batch
        [ Random.generate NewWordChain (WordChain.singleChainGenerator amount <| allWords activeDicts dictionaries)
        , Browser.Dom.focus Common.idInput
            |> Task.attempt (\_ -> NoOp)
        , Storage.setStorage
            { name = "words-found"
            , json =
                Json.Encode.list
                    Json.Encode.string
                    []
            }
        , Storage.setStorage { name = "game-stats", json = encodeGameStats newGameStats }
        ]
    )


gameFinished : Backend.Uuid -> List String -> GameModel -> ( Model, Cmd Msg )
gameFinished uuid activeDicts model =
    ( GameFinished model, Backend.postTrainingFinished uuid activeDicts model |> Cmd.map BackendMsg )


roundFinishedPost : Backend.Uuid -> Bool -> List String -> GameModel -> Cmd Msg
roundFinishedPost uuid wasSuccess activeDicts game =
    Backend.postTrainingRoundEnd uuid
        { dictsActive = activeDicts
        , nrWordsFound = List.length game.wordsFound
        , wasSuccess = wasSuccess
        , mistakes = List.length (WordChain.wrongAnswersOf game.wordChain game.answers)
        }
        |> Cmd.map BackendMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model of
            Ready { showTodoUpdate } ->
                case showTodoUpdate of
                    Nothing ->
                        Sub.none

                    _ ->
                        Time.every 1000 OnTick

            _ ->
                Sub.none
        , Browser.Events.onKeyDown (Common.onCtrlEnter UserPressedCtrlEnter NoOp)
        ]



-- VIEW


view : Device -> Words.Dictionaries -> List String -> Model -> Element Msg
view device dictionaries activeDicts model =
    case model of
        Ready game ->
            viewGame device dictionaries activeDicts game

        GameFinished game ->
            viewGameFinished game.gameStats

        _ ->
            UI.spinner


viewGame : Device -> Words.Dictionaries -> List String -> GameModel -> Element Msg
viewGame device dictionaries activeDicts game =
    let
        onMobile =
            isOnMobile device

        modals =
            column
                [ width fill
                , height fill
                , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
                ]
                [ viewDictionaryModal onMobile dictionaries activeDicts game
                , Help.view game.showHelp onMobile OnToggleHelp NoOpString
                ]
    in
    Common.viewContainer onMobile
        True
        { popup = modals
        , topbar = viewTopBar onMobile dictionaries activeDicts game
        , wordlist =
            game.wordChain
                |> List.indexedMap
                    (\wordId { word } ->
                        row [ width fill, spacing 20, padding 5 ]
                            [ el [ width <| fillPortion 1 ] <| viewWordAnswers onMobile game wordId word
                            , el [ width <| fillPortion 2 ] <| Common.viewWordEnglish onMobile word
                            ]
                    )
        , bottom =
            [ case game.gameState of
                NotDone ->
                    Common.viewInput onMobile game { msgUserPressedEnter = UserPressedEnter, msgInputHanzi = InputHanzi }

                FilledInCorrectly ->
                    el [ height (px 50), centerX ] <| UI.niceText "Good job!"

                TooManyWrongAnswers ->
                    el [ height (px 50), centerX ] <| UI.niceText "Better next time..."
            , Common.viewWrongAnwers onMobile (WordChain.wrongAnswersOf game.wordChain game.answers)
            , case game.gameState of
                NotDone ->
                    el [ centerX ] <| UI.niceButton "I give up, show me the answers" OnGiveUpClicked Nothing

                _ ->
                    el [ centerX ] <| UI.niceButtonWith [ htmlAttribute <| Html.Attributes.id idNextButton ] "Next" ToNextWord Nothing
            , el [ centerX, Font.color <| UI.gray, Font.size 14 ] <| none
            ]
        , msgKeyboardInput = KeyboardInput
        , msgKeyboardBackspace = KeyboardBackspace
        , msgKeyboardClear = KeyboardClear
        }


viewDictionaryModal : Bool -> Dictionaries -> List String -> GameModel -> Element Msg
viewDictionaryModal onMobile dictionaries activeDicts game =
    if game.showDictionaryModal then
        let
            modalFn =
                if onMobile then
                    MobileUI.modal

                else
                    UI.modal

            minSize =
                if onMobile then
                    300

                else
                    600

            remaining =
                wordsToGo dictionaries activeDicts game.wordsFound
        in
        modalFn OnToggleDictionaryModal
            [ row [ width <| minimum minSize fill ]
                [ el [ alignLeft ] <| UI.heading "Words:"
                , el [ alignRight ] <|
                    UI.niceButton
                        (if game.unhideDictionary then
                            "Hide " ++ String.fromInt remaining ++ " words"

                         else
                            "Show " ++ String.fromInt remaining ++ " hidden words"
                        )
                        OnToggleDictionaryShowAllWords
                        (Just <|
                            if game.unhideDictionary then
                                Icons.eyeOff 20

                            else
                                Icons.eye 20
                        )
                ]
            , dictionaries
                |> Words.allWords activeDicts
                |> List.map
                    (\word ->
                        Words.wordToStringParts word
                            |> (\( hanzi, pinyin, english ) ->
                                    let
                                        wordAlreadyDone =
                                            List.member (wordHanzi word) game.wordsFound

                                        maybeHide str =
                                            if game.unhideDictionary || wordAlreadyDone then
                                                str

                                            else
                                                "."
                                    in
                                    row
                                        [ width fill
                                        , spacing 20
                                        , Font.color <|
                                            if wordAlreadyDone then
                                                UI.correctColor

                                            else
                                                UI.black
                                        ]
                                        [ paragraph [ width <| fillPortion 1, Font.alignRight ] [ text hanzi ]
                                        , paragraph [ width <| fillPortion 3, Font.center ] [ text <| maybeHide pinyin ]
                                        , paragraph [ width <| fillPortion 7 ] [ text <| maybeHide english ]
                                        ]
                               )
                    )
                |> column
                    [ scrollbarY
                    , height fill
                    , width fill
                    , padding 10
                    , spacing 10
                    ]
            ]

    else
        none


viewTopBar : Bool -> Words.Dictionaries -> List String -> GameModel -> Element Msg
viewTopBar onMobile dictionaries activeDicts game =
    let
        buttonFn =
            if onMobile then
                \_ onClick icon -> MobileUI.simpleIconButtonInverted icon onClick

            else
                \str onClick icon -> UI.niceButton str onClick (Just icon)

        iconSize =
            if onMobile then
                16

            else
                20
    in
    row
        ([ height <| px 50, width fill, Background.color UI.accentColor ]
            ++ (if onMobile then
                    [ spacing 4, behindContent <| UI.viewLogo "Training" ]

                else
                    [ paddingXY 20 0, spacing 20, behindContent <| UI.viewLogo "Training" ]
               )
        )
        [ el [ alignLeft ] <| UI.niceIconButton (Icons.arrowBack 20) OnClickedHome "Home"
        , el
            [ alignLeft
            , onRight <|
                el [] <|
                    case game.showTodoUpdate of
                        Nothing ->
                            text <| ""

                        Just u ->
                            el
                                [ Font.color <|
                                    if u < 0 then
                                        UI.correctColor

                                    else
                                        UI.errorColor
                                , Font.bold
                                ]
                            <|
                                text <|
                                    "  "
                                        ++ (if u > 0 then
                                                "+"

                                            else
                                                ""
                                           )
                                        ++ String.fromInt u
            ]
          <|
            Element.Lazy.lazy3 viewWordsToGo dictionaries activeDicts game.wordsFound
        , el [ alignLeft ] <| buttonFn "Restart Game" OnRestartClick (Icons.refresh iconSize)
        , el [ alignRight ] <| buttonFn "Words" OnToggleDictionaryModal (Icons.translate iconSize)
        , buttonFn "Help" OnToggleHelp (Icons.questionmark iconSize)
        ]


viewWordsToGo : Words.Dictionaries -> List String -> List String -> Element Msg
viewWordsToGo dictionaries activeDicts wordsFound =
    UI.niceTextWith [ Font.color UI.white ] <|
        let
            total =
                wordsInActiveDict activeDicts dictionaries
        in
        (String.fromInt <| total - wordsToGo dictionaries activeDicts wordsFound)
            ++ "/"
            ++ String.fromInt total


viewGameFinished game =
    el [ width fill, height fill ] <|
        column [ centerX, centerY, spacing 40 ]
            [ UI.heading "Game Finished!"
            , text <| "Nr of attempts: " ++ String.fromInt game.attempts
            , text <| "Of which nr of successes: " ++ String.fromInt game.correct
            , text <| "Of which nr of retries: " ++ String.fromInt game.retries
            , el [ centerX ] <| UI.niceButton "Restart" OnRestartClick (Just <| Icons.refresh 20)
            ]


viewWordAnswers : Bool -> GameModel -> Int -> Word -> Element Msg
viewWordAnswers onMobile game wordId word =
    word.characters
        |> List.indexedMap (viewSingleHanzi onMobile game wordId)
        |> row [ spacing 10 ]


viewSingleHanzi : Bool -> GameModel -> Int -> Int -> Character -> Element Msg
viewSingleHanzi onMobile game wordId id character =
    let
        known =
            isCharacterKnown character game.answers

        state =
            case game.gameState of
                NotDone ->
                    if known then
                        WordChain.Known

                    else
                        let
                            similar =
                                isCharacterSimilar character game.answers
                        in
                        if similar then
                            WordChain.Similar

                        else
                            WordChain.Unknown

                _ ->
                    WordChain.Show known
    in
    WordChain.viewSingleHanzi
        onMobile
        { state = state
        , showPopup = game.showPopupForCharacter
        , onMouseEnterCharacterMsg = OnMouseEnterCharacter
        , onMouseLeaveCharacterMsg = OnMouseLeaveCharacter
        , wordId = wordId
        , id = id
        , character = character
        }



--


dictActive game dictName =
    Dict.get dictName game.dictsActive
        |> Maybe.withDefault True


wordsToGo : Words.Dictionaries -> List String -> List String -> Int
wordsToGo dictionaries activeDicts wordsFound =
    Words.allWords activeDicts dictionaries
        |> List.filter (\w -> not <| List.member (wordHanzi w) wordsFound)
        |> List.length


wordsInActiveDict : List String -> Words.Dictionaries -> Int
wordsInActiveDict activeDicts dictionaries =
    Words.allWords activeDicts dictionaries
        |> List.length


processInput : String -> GameModel -> GameModel
processInput txt game =
    case txt |> splitStringIntoPinyin of
        Ok pinyinParts ->
            { game
                | answers = pinyinParts ++ game.answers |> List.unique
                , currentInput = ""
                , errorMsg = Nothing
            }

        Err err ->
            -- TODO: show error message
            { game | currentInput = "", errorMsg = Just err }


processRoundFinished : Backend.Uuid -> List String -> GameModel -> ( GameModel, Cmd Msg )
processRoundFinished uuid activeDicts game =
    let
        withFocusNextBtn ( m, cmd1 ) =
            ( m
            , Cmd.batch
                [ cmd1
                , Browser.Dom.focus idNextButton
                    |> Task.attempt (\_ -> NoOp)
                ]
            )
    in
    withFocusNextBtn <|
        if List.length (WordChain.wrongAnswersOf game.wordChain game.answers) > 5 then
            -- Too many wrong answers
            let
                newGameStats =
                    game.gameStats
                        |> withAddRetries (List.length game.wordChain)
                        |> withAddAttempts (List.length game.wordChain)
            in
            ( { game
                | gameState = TooManyWrongAnswers
                , showTodoUpdate = Just (List.length game.wordChain)
                , showTodoUpdateTimer = 2
                , gameStats = newGameStats
                , errorMsg = Nothing
              }
            , Cmd.batch
                [ Storage.setStorage { name = "game-stats", json = encodeGameStats newGameStats }
                , roundFinishedPost uuid False activeDicts game
                ]
            )

        else
            let
                finished =
                    game.wordChain
                        |> List.foldl
                            (\{ word } acc -> isWordFullyKnown word game.answers && acc)
                            True
            in
            if finished then
                -- word round finished, we found all correctly
                let
                    newGameStats =
                        game.gameStats
                            |> withAddAttempts 1
                            |> withAddOneCorrect

                    wordsFound =
                        List.foldl
                            (\{ word } acc ->
                                if List.member (wordHanzi word) acc then
                                    acc

                                else
                                    wordHanzi word :: acc
                            )
                            game.wordsFound
                            game.wordChain
                in
                ( { game
                    | gameState = FilledInCorrectly
                    , showTodoUpdate = Just -(List.length game.wordChain)
                    , showTodoUpdateTimer = 2
                    , wordsFound = wordsFound
                    , gameStats = newGameStats
                    , errorMsg = Nothing
                  }
                , Cmd.batch
                    [ Storage.setStorage
                        { name = "words-found"
                        , json =
                            Json.Encode.list
                                Json.Encode.string
                                wordsFound
                        }
                    , Storage.setStorage <| { name = "game-stats", json = encodeGameStats newGameStats }
                    , roundFinishedPost uuid True activeDicts game
                    ]
                )

            else
                ( { game | gameState = NotDone }, Cmd.none )



--
--
