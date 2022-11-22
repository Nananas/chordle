module Training exposing (..)

import Backend
import Browser
import Browser.Dom
import Browser.Events
import Common
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import Help
import Html
import Html.Attributes
import Html.Events
import Icons
import Json.Decode
import Json.Encode
import List.Extra as List
import MobileUI
import Notifications
import Process
import Random
import Random.List
import Set
import Storage
import String.Extra as String
import Task
import Time
import Tones exposing (..)
import UI
import Utils exposing (..)
import WordChain exposing (WordChain)
import Words exposing (..)


type Msg
    = OnStorageLoaded Storage.Storage
    | NewWordChain WordChain
    | InputHanzi String
    | UserPressedEnter
    | ToNextWord
    | OnGiveUpClicked
    | OnToggleHelp
    | OnToggleScreenKeyboard
    | OnToggleDictionaryModal
    | OnToggleDictionaryShowAllWords
    | OnToggleDictionaryActive String
    | OnRestartClick
      --
    | OnMouseEnterCharacter ( Int, Int )
    | OnMouseLeaveCharacter
    | OnTick Time.Posix
    | OnNotificationPermissionChanged String
    | OnToggleNotifications
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
    | Initializing (List Word) GameStats
    | Ready GameModel
    | GameFinished GameModel


type alias GameModel =
    { wordsFound : List Word
    , gameStats : GameStats

    --
    , wordChain : WordChain
    , answers : List PinyinPart
    , currentInput : String
    , gameState : GameState

    --
    , showPopupForCharacter : Maybe ( Int, Int )
    , showTodoUpdate : Maybe Int
    , showTodoUpdateTimer : Int
    , showHelp : Bool
    , showDictionaryModal : Bool
    , unhideDictionary : Bool
    , dictsActive : Dict String Bool

    --
    , useScreenKeyboardOnMobile : Bool
    , keyboardKeyFeedback : Maybe String

    --
    , notificationsEnabled : Maybe Bool
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
        Json.Decode.int
        Json.Decode.int
        Json.Decode.int


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



--


update : Backend.Uuid -> Msg -> Model -> ( Model, Cmd Msg )
update uuid msg model =
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
                words : List String
                words =
                    storage.json
                        |> Json.Decode.decodeValue (Json.Decode.list Json.Decode.string)
                        |> Result.toMaybe
                        |> Maybe.withDefault []

                ( wordsFound, dictionary ) =
                    List.partition
                        (\word ->
                            List.member
                                (word.characters
                                    |> List.map .hanzi
                                    |> String.join ""
                                )
                                words
                        )
                        (allWords Dict.empty)

                --wordsFoundDebug =
                --allWords Dict.empty |> List.drop 1
                --|> Debug.log "Debug stuff"
            in
            if List.length wordsFound >= List.length (allWords Dict.empty) then
                gameFinished
                    uuid
                    { wordsFound = wordsFound
                    , wordChain = []
                    , answers = []
                    , currentInput = ""
                    , gameState = NotDone
                    , showPopupForCharacter = Nothing
                    , showTodoUpdate = Nothing
                    , showTodoUpdateTimer = 0
                    , showHelp = False
                    , showDictionaryModal = False
                    , unhideDictionary = False
                    , dictsActive = Dict.empty
                    , notificationsEnabled = Nothing
                    , lastActivity = Time.millisToPosix 0
                    , useScreenKeyboardOnMobile = True
                    , keyboardKeyFeedback = Nothing
                    , gameStats = gameStats
                    }

            else
                ( Initializing wordsFound gameStats
                , Random.generate NewWordChain (WordChain.singleChainGenerator amount dictionary)
                )

        ( Initializing wordsFound gameStats, NewWordChain wordChain ) ->
            ( Ready
                { wordsFound = wordsFound
                , wordChain = wordChain
                , answers = []
                , currentInput = ""
                , gameState = NotDone
                , showPopupForCharacter = Nothing
                , showTodoUpdate = Nothing
                , showTodoUpdateTimer = 0
                , showHelp = False
                , showDictionaryModal = False
                , unhideDictionary = False
                , dictsActive = Dict.empty
                , notificationsEnabled = Nothing
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
                            (\word ->
                                word.characters
                                    |> List.map .hanzi
                                    |> String.join ""
                                    |> Json.Encode.string
                            )
                            wordsFound
                    }
                , Process.sleep 100
                    |> Task.andThen (\_ -> Browser.Dom.focus Common.idInput)
                    |> Task.attempt (\_ -> NoOp)
                ]
            )

        ( Ready game, OnToggleNotifications ) ->
            case game.notificationsEnabled of
                Nothing ->
                    ( Ready { game | notificationsEnabled = Just False }, Notifications.requestPermissions () )

                Just enabled ->
                    ( Ready { game | notificationsEnabled = Just <| not enabled }, Cmd.none )

        ( Ready game, OnNotificationPermissionChanged "granted" ) ->
            case game.notificationsEnabled of
                Nothing ->
                    ( Ready { game | notificationsEnabled = Just False }, Cmd.none )

                Just _ ->
                    ( Ready { game | notificationsEnabled = Just True }, onActivity )

        ( Ready game, OnActivity now ) ->
            ( Ready { game | lastActivity = now }, Cmd.none )

        ( Ready game, NewWordChain wordChain ) ->
            ( Ready
                { game
                    | wordChain = wordChain
                    , answers = []
                    , currentInput = ""
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
                { game | currentInput = txt }
            , Cmd.none
            )

        ( Ready game, KeyboardInput char ) ->
            ( Ready { game | currentInput = game.currentInput ++ char }, Cmd.none )

        ( Ready game, KeyboardBackspace ) ->
            ( Ready { game | currentInput = String.dropRight 1 game.currentInput }, Cmd.none )

        ( Ready game, KeyboardClear ) ->
            ( Ready { game | currentInput = "" }, Cmd.none )

        ( Ready game, OnToggleScreenKeyboard ) ->
            ( Ready { game | useScreenKeyboardOnMobile = not game.useScreenKeyboardOnMobile }, Cmd.none )

        ( Ready game, UserPressedEnter ) ->
            let
                txt =
                    String.toCodePoints game.currentInput
                        |> List.filter (\code -> code <= 126)
                        |> String.fromCodePoints

                ( newModel, cmd ) =
                    processInput txt game
                        |> processRoundFinished
            in
            ( Ready newModel, cmd )

        ( Ready game, ToNextWord ) ->
            if List.length game.wordsFound >= List.length (allWords game.dictsActive) then
                gameFinished uuid game

            else
                let
                    dictionary =
                        allWords game.dictsActive
                            |> List.filter (\word -> not <| List.member word game.wordsFound)
                in
                ( Ready game
                , Cmd.batch
                    [ Random.generate NewWordChain (WordChain.singleChainGenerator amount dictionary)
                    , Browser.Dom.focus Common.idInput
                        |> Task.attempt (\_ -> NoOp)
                    ]
                )

        ( Ready game, OnGiveUpClicked ) ->
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
            , Storage.setStorage <| { name = "game-stats", json = encodeGameStats newGameStats }
            )

        ( Ready game, OnRestartClick ) ->
            restartGame game

        ( GameFinished game, OnRestartClick ) ->
            restartGame game

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

        ( Ready game, OnToggleDictionaryActive dictName ) ->
            ( Ready
                { game
                    | dictsActive =
                        Dict.update dictName
                            (\v ->
                                case v of
                                    Nothing ->
                                        -- Default state is True
                                        Just False

                                    Just active ->
                                        Just <| not active
                            )
                            game.dictsActive
                }
            , Cmd.none
            )

        --
        ( Ready game, OnMouseEnterCharacter id ) ->
            ( Ready { game | showPopupForCharacter = Just id }, Cmd.none )

        ( Ready game, OnMouseLeaveCharacter ) ->
            ( Ready { game | showPopupForCharacter = Nothing }, Cmd.none )

        ( Ready game, OnTick now ) ->
            let
                withUpdateShowTodo m =
                    if m.showTodoUpdateTimer > 0 then
                        { m | showTodoUpdateTimer = m.showTodoUpdateTimer - 1 }

                    else
                        { m | showTodoUpdate = Nothing }

                withUpdateNotifications m =
                    case m.notificationsEnabled of
                        Just True ->
                            if (Time.posixToMillis now - Time.posixToMillis m.lastActivity) > inactivityTime then
                                ( { m | lastActivity = now }, Notifications.showNotification "Time for a CHORDLE!" )

                            else
                                ( m, Cmd.none )

                        _ ->
                            ( m, Cmd.none )
            in
            game
                |> withUpdateShowTodo
                |> withUpdateNotifications
                |> liftModel Ready

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


restartGame model =
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
        [ Random.generate NewWordChain (WordChain.singleChainGenerator amount <| allWords model.dictsActive)
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


gameFinished : Backend.Uuid -> GameModel -> ( Model, Cmd Msg )
gameFinished uuid model =
    ( GameFinished model, Backend.postTrainingFinished uuid model |> Cmd.map BackendMsg )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model of
            Ready _ ->
                Time.every 1000 OnTick

            _ ->
                Sub.none
        ]



-- VIEW


view : Device -> Model -> Element Msg
view device model =
    case model of
        Ready game ->
            viewGame device game

        GameFinished game ->
            viewGameFinished game.gameStats

        _ ->
            UI.spinner


viewGame : Device -> GameModel -> Element Msg
viewGame device game =
    let
        onMobile =
            isOnMobile device

        modals =
            column
                [ width fill
                , height fill
                , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
                ]
                [ viewDictionaryModal onMobile game
                , Help.view game.showHelp onMobile OnToggleHelp NoOpString
                ]

        contentSpacing =
            if onMobile then
                10

            else
                20
    in
    Common.viewContainer onMobile
        True
        { popup = modals
        , topbar = viewTopBar onMobile game
        , wordlist =
            game.wordChain
                |> List.indexedMap
                    (\wordId word ->
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
            , Common.viewWrongAnwers onMobile (wrongAnswersOf game)
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


viewDictionaryModal : Bool -> GameModel -> Element Msg
viewDictionaryModal onMobile game =
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

        wordCount =
            wordsInActiveDict game.dictsActive
    in
    if game.showDictionaryModal then
        modalFn OnToggleDictionaryModal
            [ row [ width <| minimum minSize fill ]
                [ el [ alignLeft ] <| UI.heading "Dictionary"
                , el [ alignRight ] <|
                    UI.niceButton
                        (if game.unhideDictionary then
                            "Hide all " ++ String.fromInt wordCount ++ " words"

                         else
                            "Show all " ++ String.fromInt wordCount ++ " words"
                        )
                        OnToggleDictionaryShowAllWords
                        (Just <|
                            if game.unhideDictionary then
                                Icons.eyeOff 20

                            else
                                Icons.eye 20
                        )
                ]
            , Words.allDicts
                |> List.map (\( dictName, _ ) -> UI.niceToggleButton dictName (OnToggleDictionaryActive dictName) Nothing <| dictActive game dictName)
                |> row [ centerX, height shrink, paddingXY 10 0, spacing 20 ]
            , Words.allWords game.dictsActive
                |> List.map
                    (\word ->
                        Words.wordToStringParts word
                            |> (\( hanzi, pinyin, english ) ->
                                    let
                                        wordAlreadyDone =
                                            List.member word game.wordsFound

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


viewTopBar : Bool -> GameModel -> Element Msg
viewTopBar onMobile game =
    let
        buttonFn =
            if onMobile then
                \str onClick icon -> MobileUI.simpleIconButtonInverted icon onClick

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
            Element.Lazy.lazy3 viewWordsToGo onMobile game.dictsActive game.wordsFound
        , el [ alignLeft ] <| buttonFn "Restart Game" OnRestartClick (Icons.refresh iconSize)
        , el [ alignRight ] <| buttonFn "Dictionaries" OnToggleDictionaryModal (Icons.translate iconSize)
        , buttonFn "Help" OnToggleHelp (Icons.questionmark iconSize)
        ]


viewWordsToGo onMobile dictsActive wordsFound =
    UI.niceTextWith [ Font.color UI.white ] <|
        let
            total =
                wordsInActiveDict dictsActive
        in
        (String.fromInt <| total - wordsToGo dictsActive wordsFound)
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
            isCharacterKnown game character

        similar =
            isCharacterSimilar game character

        state =
            case game.gameState of
                NotDone ->
                    if known then
                        WordChain.Known

                    else if similar then
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



--let
--    popup =
--        case game.showPopupForCharacter of
--            Nothing ->
--                none
--            Just ( wid, i ) ->
--                if wid == wordId && id == i then
--                    column [ centerX ]
--                        [ el [ height <| px 10 ] none
--                        , el
--                            [ Background.color UI.white
--                            , padding 10
--                            , centerX
--                            , Font.color UI.gray
--                            , UI.floatingHigh
--                            , Border.rounded 10
--                            , Font.size 16
--                            , Font.regular
--                            ]
--                          <|
--                            text <|
--                                formatPinyin character.pinyinPart
--                        ]
--                else
--                    none
--    known =
--        isCharacterKnown game character
--    similar =
--        isCharacterSimilar game character
--in
--el
--    ([ UI.floating
--     , UI.rounded 5
--     , paddingXY 20 0
--     --, width <| maximum 50 shrink
--     , width <|
--        if known then
--            px 50
--        else if similar then
--            shrink
--        else
--            px 50
--     , height <| px 50
--     , Font.color <|
--        case game.gameState of
--            NotDone ->
--                UI.black
--            _ ->
--                toneToColor character.pinyinPart.tone
--     , Font.medium
--     , Background.color <|
--        case game.gameState of
--            NotDone ->
--                if known then
--                    rgb255 152 226 172
--                else if similar then
--                    rgb255 240 230 110
--                else
--                    rgb 1 1 1
--            _ ->
--                -- TODO: tones
--                rgb 1 1 1
--     , Border.color <|
--        case game.gameState of
--            NotDone ->
--                rgba 0 0 0 0
--            _ ->
--                if known then
--                    rgb255 152 226 172
--                else
--                    rgb255 230 125 125
--     , Border.width 2
--     , below popup
--     ]
--        ++ (case game.gameState of
--                NotDone ->
--                    []
--                _ ->
--                    [ Element.Events.onMouseEnter (OnMouseEnterCharacter ( wordId, id ))
--                    , Element.Events.onMouseLeave OnMouseLeaveCharacter
--                    ]
--           )
--    )
--<|
--    el [ centerX, centerY, Font.size 20 ] <|
--        text <|
--            case game.gameState of
--                NotDone ->
--                    if known then
--                        character.hanzi
--                    else if similar then
--                        character.pinyinPart.pinyin
--                    else
--                        ""
--                _ ->
--                    character.hanzi


viewWrongAnwers : Bool -> List PinyinPart -> Element Msg
viewWrongAnwers onMobile wrongAnswers =
    let
        size =
            if onMobile then
                40

            else
                50

        fontsize =
            if onMobile then
                14

            else
                20
    in
    (List.map (\w -> Just w) wrongAnswers ++ List.repeat (6 - List.length wrongAnswers) Nothing)
        |> List.map
            (\mPart ->
                el
                    [ UI.floating
                    , UI.rounded 5
                    , height (px size)
                    , width <|
                        minimum size shrink
                    , paddingXY 10 0
                    , Background.color <|
                        case mPart of
                            Just _ ->
                                UI.errorColorLight

                            _ ->
                                rgb255 226 226 226
                    , Border.color <|
                        case mPart of
                            Just _ ->
                                UI.errorColor

                            _ ->
                                rgb255 226 226 226
                    , Border.width 2
                    ]
                <|
                    el [ centerX, centerY, Font.size fontsize ] <|
                        case mPart of
                            Just part ->
                                text (formatPinyin part)

                            Nothing ->
                                text "     "
            )
        |> row [ centerX, height (px size), centerY, spacing 5 ]
        |> el [ width fill ]



--


dictActive game dictName =
    Dict.get dictName game.dictsActive
        |> Maybe.withDefault True


wordsToGo dictsActive wordsFound =
    Words.allWords dictsActive
        |> List.filter (\w -> not <| List.member w wordsFound)
        |> List.length


wordsInActiveDict dictsActive =
    Words.allWords dictsActive
        |> List.length


processInput : String -> GameModel -> GameModel
processInput txt game =
    case txt |> splitStringIntoPinyin of
        Ok pinyinParts ->
            { game | answers = pinyinParts ++ game.answers |> List.unique, currentInput = "" }

        Err err ->
            -- TODO: show error message
            { game | currentInput = "" }


isWordFullyKnown : GameModel -> Word -> Bool
isWordFullyKnown game word =
    word.characters
        |> List.filter (\character -> not <| List.member character.pinyinPart game.answers)
        |> List.length
        |> (==) 0


isCharacterKnown : GameModel -> Character -> Bool
isCharacterKnown game character =
    List.member character.pinyinPart game.answers


isCharacterSimilar : GameModel -> Character -> Bool
isCharacterSimilar game character =
    List.any (\part -> part.pinyin == character.pinyinPart.pinyin) game.answers


wrongAnswersOf : GameModel -> List PinyinPart
wrongAnswersOf game =
    game.answers
        |> List.filter (\part -> not <| WordChain.isPinyinValid part game.wordChain)


processRoundFinished : GameModel -> ( GameModel, Cmd Msg )
processRoundFinished game =
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
        if List.length (wrongAnswersOf game) > 5 then
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
              }
            , Storage.setStorage { name = "game-stats", json = encodeGameStats newGameStats }
            )

        else
            let
                finished =
                    game.wordChain
                        |> List.foldl
                            (\word acc -> isWordFullyKnown game word && acc)
                            True

                wordsFound =
                    List.foldl
                        (\w acc ->
                            if List.member w acc then
                                acc

                            else
                                w :: acc
                        )
                        game.wordsFound
                        game.wordChain
            in
            if finished then
                -- word round finished, we found all correctly
                let
                    newGameStats =
                        game.gameStats
                            |> withAddAttempts 1
                            |> withAddOneCorrect
                in
                ( { game
                    | gameState = FilledInCorrectly
                    , showTodoUpdate = Just -(List.length game.wordChain)
                    , showTodoUpdateTimer = 2
                    , wordsFound = wordsFound
                    , gameStats = newGameStats
                  }
                , Cmd.batch
                    [ Storage.setStorage
                        { name = "words-found"
                        , json =
                            Json.Encode.list
                                (\word ->
                                    word.characters
                                        |> List.map .hanzi
                                        |> String.join ""
                                        |> Json.Encode.string
                                )
                                wordsFound
                        }
                    , Storage.setStorage <| { name = "game-stats", json = encodeGameStats newGameStats }
                    ]
                )

            else
                ( { game | gameState = NotDone }, Cmd.none )



--
--
