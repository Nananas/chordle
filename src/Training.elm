module Training exposing (..)

import Browser
import Browser.Dom
import Browser.Events
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
    = StorageLoaded Storage.Storage
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


idInput =
    "text-input"


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( LoadingGameStatsFromStorage, StorageLoaded storage ) ->
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

        ( LoadingWordsFoundFromStorage gameStats, StorageLoaded storage ) ->
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
            if List.length wordsFound == List.length (allWords Dict.empty) then
                ( GameFinished
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
                , Cmd.none
                )

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
                    |> Task.andThen (\_ -> Browser.Dom.focus idInput)
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
            if List.length game.wordsFound == List.length (allWords game.dictsActive) then
                ( GameFinished game, Cmd.none )

            else
                let
                    dictionary =
                        allWords game.dictsActive
                            |> List.filter (\word -> not <| List.member word game.wordsFound)
                in
                ( Ready game
                , Cmd.batch
                    [ Random.generate NewWordChain (WordChain.singleChainGenerator amount dictionary)
                    , Browser.Dom.focus idInput
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
                                        Just True

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
        , Browser.Dom.focus idInput
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Storage.storageLoaded StorageLoaded
        , case model of
            Ready _ ->
                Time.every 1000 OnTick

            _ ->
                Sub.none
        , Notifications.permissionChanged OnNotificationPermissionChanged
        ]



-- VIEW


view : Device -> Model -> Element Msg
view device model =
    case model of
        Ready game ->
            if isOnMobile device then
                viewMobile game

            else
                viewDesktop game

        GameFinished game ->
            viewGameFinished game.gameStats

        _ ->
            UI.spinner


viewMobile : GameModel -> Element Msg
viewMobile game =
    let
        modals =
            column
                [ width fill
                , height fill
                , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
                ]
                [ viewDictionaryModal mobile game
                , Help.viewMobile game.showHelp OnToggleHelp NoOpString
                ]
    in
    column [ width fill, height fill ]
        [ mobileViewTopBar game
        , row [ width fill, height fill, inFront modals ]
            [ el [ height fill, width fill ] <|
                column [ width fill, height fill, spacing 20, paddingXY 0 10 ]
                    [ column [ width fill, height shrink, spacing 20, UI.floating, padding 10 ]
                        (List.indexedMap
                            (\wordId word ->
                                row [ width fill, spacing 20 ]
                                    [ el [ width <| fillPortion 1 ] <| viewWordAnswers game wordId word
                                    , el [ width <| fillPortion 1 ] <| viewWordEnglishMobile word
                                    ]
                            )
                            game.wordChain
                        )
                    , viewWrongAnwers <| allWrongAnswers game
                    , case game.gameState of
                        NotDone ->
                            el [ centerX ] <| UI.niceButton "I give up, show me the answers" OnGiveUpClicked Nothing

                        _ ->
                            el [ centerX, height <| px 40 ] none
                    , case game.gameState of
                        NotDone ->
                            viewInput mobile game

                        _ ->
                            UI.niceButtonWith [ centerX, htmlAttribute <| Html.Attributes.id idNextButton ] "Next" ToNextWord Nothing
                    , if game.useScreenKeyboardOnMobile then
                        el [ alignBottom, width fill, height <| maximum 300 fill ] <| MobileUI.viewKeyboard KeyboardInput KeyboardBackspace KeyboardClear

                      else
                        none
                    ]
            ]
        ]


viewDesktop : GameModel -> Element Msg
viewDesktop game =
    let
        modals =
            row [ width fill, height fill, htmlAttribute <| Html.Attributes.style "pointer-events" "none" ]
                [ viewDictionaryModal desktop game
                , Help.viewDesktop game.showHelp OnToggleHelp NoOpString
                ]
    in
    column [ width fill, height fill, inFront UI.viewFooter ]
        [ viewTopBar desktop game
        , row [ width fill, height fill, inFront modals ]
            [ el [ height fill, width <| fillPortion 1 ] none
            , el [ height fill, width <| fillPortion 5 ] <|
                column [ centerX, centerY, spacing 50 ]
                    [ column [ spacing 20, UI.floating, padding 40 ]
                        (List.indexedMap
                            (\wordId word ->
                                row [ width <| px 600, spacing 20 ]
                                    [ el [ width <| fillPortion 1 ] <| viewWordAnswers game wordId word
                                    , el [ width <| fillPortion 1 ] <| viewWordEnglish word
                                    ]
                            )
                            game.wordChain
                        )
                    , viewWrongAnwers <| allWrongAnswers game
                    , case game.gameState of
                        NotDone ->
                            el [ centerX ] <| UI.niceButton "I give up, show me the answers" OnGiveUpClicked Nothing

                        _ ->
                            el [ centerX, height <| px 40 ] none
                    , case game.gameState of
                        NotDone ->
                            viewInput desktop game

                        _ ->
                            UI.niceButtonWith [ centerX, htmlAttribute <| Html.Attributes.id idNextButton ] "Next" ToNextWord Nothing
                    ]
            , el [ height fill, width <| fillPortion 1 ] none
            ]
        ]


viewDictionaryModal : Device -> GameModel -> Element Msg
viewDictionaryModal device game =
    let
        modalFn =
            if isOnDesktop device then
                UI.modal

            else
                MobileUI.modal
    in
    if game.showDictionaryModal then
        modalFn OnToggleDictionaryModal
            [ row [ width <| minimum 400 fill ]
                [ el [ alignLeft ] <| UI.heading "Dictionary"
                , el [ alignRight ] <|
                    UI.niceButton
                        (if game.unhideDictionary then
                            "Hide all words"

                         else
                            "Show all words"
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
                                        [ paragraph [ width <| minimum 100 <| fillPortion 1, Font.size 24, Font.alignRight ] [ text hanzi ]
                                        , paragraph [ width <| fillPortion 2, Font.size 16, Font.center ] [ text <| maybeHide pinyin ]
                                        , paragraph [ width <| fillPortion 3, Font.size 16 ] [ text <| maybeHide english ]
                                        ]
                               )
                    )
                |> column [ scrollbarY, height fill, width fill, padding 10, spacing 10 ]
            ]

    else
        none


viewTopBar : Device -> GameModel -> Element Msg
viewTopBar device game =
    let
        buttonFn =
            case device.class of
                Desktop ->
                    \str onClick icon -> UI.niceButton str onClick (Just icon)

                _ ->
                    \str onClick icon -> UI.niceIconButton icon onClick
    in
    row [ height <| px 50, width fill, Background.color UI.accentColor, paddingXY 20 0, spacing 20, behindContent UI.viewLogo ]
        [ el [ alignLeft ] <| UI.niceIconButton (Icons.arrowBack 20) OnClickedHome
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
            Element.Lazy.lazy2 viewWordsToGo game.dictsActive game.wordsFound
        , el [ alignLeft ] <| buttonFn "Restart Game" OnRestartClick (Icons.refresh 20)

        --, el [ alignRight ] <|
        --    buttonFn
        --        (case model.notificationsEnabled of
        --            Nothing ->
        --                "Notifications"
        --            Just True ->
        --                "Disable Notifications"
        --            Just False ->
        --                "Enable Notifications"
        --        )
        --        OnToggleNotifications
        --        (Icons.bell 20)
        , el [ alignRight ] <| buttonFn "Dictionaries" OnToggleDictionaryModal (Icons.translate 20)
        , buttonFn "Help" OnToggleHelp (Icons.questionmark 20)
        ]


viewWordsToGo dictsActive wordsFound =
    UI.niceTextWith [ Font.color UI.white ] <|
        "Words to go: "
            ++ (String.fromInt <| wordsToGo dictsActive wordsFound)


mobileViewTopBar : GameModel -> Element Msg
mobileViewTopBar game =
    row [ height <| px 50, width fill, Background.color UI.accentColor ]
        [ el [ width fill ] <|
            el
                [ centerX
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
                MobileUI.niceTextWith [ Font.color UI.white ] <|
                    "Words to go: "
                        ++ (String.fromInt <| (List.length (allWords game.dictsActive) - List.length game.wordsFound))
        , MobileUI.simpleIconButtonInverted (Icons.refresh 20) OnRestartClick
            |> el [ alignRight ]
        , MobileUI.simpleIconButtonInverted (Icons.translate 20) OnToggleDictionaryModal
            |> el [ alignRight ]
        , MobileUI.simpleIconButtonInverted (Icons.questionmark 20) OnToggleHelp
            |> el [ alignRight ]
        ]


viewGameFinished game =
    el [ width fill, height fill ] <|
        column [ centerX, centerY, spacing 40 ]
            [ UI.heading "Game Finished!"
            , text <| "Nr of attempts: " ++ String.fromInt game.attempts
            , text <| "Of which nr of successes: " ++ String.fromInt game.correct
            , text <| "Of which nr of retries: " ++ String.fromInt game.retries
            , el [ centerX ] <| UI.niceButton "Restart" OnRestartClick (Just <| Icons.refresh 20)
            ]


viewWordAnswers : GameModel -> Int -> Word -> Element Msg
viewWordAnswers game wordId word =
    word.characters
        |> List.indexedMap (viewSingleHanzi game wordId)
        |> row [ spacing 10 ]


viewWordEnglish word =
    word.english
        |> String.split "|"
        |> List.map (\txt -> paragraph [] [ text txt ])
        |> column [ spacing 5, alignLeft ]


viewWordEnglishMobile word =
    word.english
        |> String.split "|"
        |> List.map (\txt -> paragraph [ Font.size 16 ] [ text txt ])
        |> column [ spacing 5, alignLeft ]


viewSingleHanzi : GameModel -> Int -> Int -> Character -> Element Msg
viewSingleHanzi game wordId id character =
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


viewInput : Device -> GameModel -> Element Msg
viewInput device game =
    el [ width fill, padding 10 ] <|
        row
            [ width fill
            , Border.widthEach
                { top = 0
                , left = 0
                , right = 0
                , bottom = 2
                }
            , spacing 20
            ]
            [ case device.class of
                Phone ->
                    text game.currentInput

                _ ->
                    Input.text
                        [ onEnter UserPressedEnter
                        , htmlAttribute <| Html.Attributes.id idInput
                        , Border.width 0
                        ]
                        { onChange = InputHanzi
                        , text = game.currentInput
                        , placeholder =
                            Just <|
                                Input.placeholder [ Font.size 14 ] <|
                                    text <|
                                        "Write pinyin with tones here (e.g. hao3 for 好), then press 'OK'"
                                            ++ (if isOnDesktop device then
                                                    " or the Enter key"

                                                else
                                                    ""
                                               )
                        , label = Input.labelHidden ""
                        }
            , Input.button
                [ mouseOver [ Font.color UI.accentColorHighlight ]
                , Font.color UI.accentColor
                , Background.color UI.white
                , width shrink
                , height <| px 40
                , width <| px 40
                , Border.rounded 10
                , alignRight
                ]
                { onPress = Just UserPressedEnter
                , label =
                    el [ centerY, centerX, paddingXY 20 0, spacing 20 ] <|
                        text "Ok"
                }
            ]


viewWrongAnwers : List PinyinPart -> Element Msg
viewWrongAnwers wrongAnswers =
    wrongAnswers
        |> List.map
            (\part ->
                el
                    [ UI.floating
                    , UI.rounded 5
                    , height <| px 50
                    , paddingXY 10 0
                    , Background.color <|
                        rgb255 226 226 226
                    ]
                <|
                    el [ centerX, centerY, Font.size 20 ] <|
                        text (formatPinyin part)
            )
        |> row [ spacing 10, height <| px 50, centerX ]



--


dictActive game dictName =
    Dict.get dictName game.dictsActive
        |> Maybe.withDefault True


wordsToGo dictsActive wordsFound =
    Words.allWords dictsActive
        |> List.filter (\w -> not <| List.member w wordsFound)
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


allWrongAnswers : GameModel -> List PinyinPart
allWrongAnswers game =
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
        if List.length (allWrongAnswers game) > 5 then
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
