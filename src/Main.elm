module Main exposing (..)

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
import Words exposing (..)


minDesktopViewportWidth =
    800


maxSmallDesktopViewportWidth =
    1200


inactivityTime =
    30 * 60 * 1000


idNextButton =
    "next-btn"


idInput =
    "text-input"


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--


type Msg
    = OnGetViewport Browser.Dom.Viewport
    | OnResizeViewport Int Int
    | StorageLoaded Storage.Storage
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



--| MODEL


type Session
    = LoadingDeviceType
    | LoadingGameStatsFromStorage Device
    | LoadingWordsFoundFromStorage GameStats Device
    | Initializing (List Word) GameStats Device
    | Ready Model
    | GameFinished Model


type alias Model =
    { device : Device

    --
    , wordsFound : List Word
    , gameStats : GameStats

    --
    , wordChain : List Word
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


type Device
    = Mobile
    | DesktopSmall
    | Desktop


type alias WordChain =
    List Word


type GameState
    = NotDone
    | TooManyWrongAnswers
    | FilledInCorrectly


type alias GameStats =
    { correct : Int
    , attempts : Int
    , retries : Int
    }


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


init : () -> ( Session, Cmd Msg )
init _ =
    ( LoadingDeviceType
    , Browser.Dom.getViewport
        |> Task.perform OnGetViewport
    )



--


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    case ( session, msg ) of
        ( LoadingDeviceType, OnGetViewport { viewport } ) ->
            let
                device =
                    if viewport.width < minDesktopViewportWidth then
                        Mobile

                    else
                        Desktop
            in
            ( LoadingGameStatsFromStorage device, Storage.loadStorage "game-stats" )

        ( LoadingGameStatsFromStorage device, StorageLoaded storage ) ->
            let
                gameStats =
                    storage.json
                        |> Json.Decode.decodeValue gameStatsDecoder
            in
            case gameStats of
                Err _ ->
                    ( LoadingWordsFoundFromStorage defaultGameStats device, Storage.loadStorage "words-found" )

                Ok stats ->
                    ( LoadingWordsFoundFromStorage stats device, Storage.loadStorage "words-found" )

        ( LoadingWordsFoundFromStorage gameStats device, StorageLoaded storage ) ->
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
            in
            if List.length wordsFound == List.length (allWords Dict.empty) then
                ( GameFinished
                    { device = device
                    , wordsFound = wordsFound
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
                ( Initializing wordsFound gameStats device, Random.generate NewWordChain (wordChainGenerator dictionary) )

        ( Initializing wordsFound gameStats device, NewWordChain wordChain ) ->
            ( Ready
                { device = device
                , wordsFound = wordsFound
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

        ( Ready model, OnResizeViewport width height ) ->
            let
                device =
                    if width < minDesktopViewportWidth then
                        Mobile

                    else if width < maxSmallDesktopViewportWidth then
                        DesktopSmall

                    else
                        Desktop
            in
            ( Ready { model | device = device }, Cmd.none )

        ( Ready model, OnToggleNotifications ) ->
            case model.notificationsEnabled of
                Nothing ->
                    ( Ready { model | notificationsEnabled = Just False }, Notifications.requestPermissions () )

                Just enabled ->
                    ( Ready { model | notificationsEnabled = Just <| not enabled }, Cmd.none )

        ( Ready model, OnNotificationPermissionChanged "granted" ) ->
            case model.notificationsEnabled of
                Nothing ->
                    ( Ready { model | notificationsEnabled = Just False }, Cmd.none )

                Just _ ->
                    ( Ready { model | notificationsEnabled = Just True }, onActivity )

        ( Ready model, OnActivity now ) ->
            ( Ready { model | lastActivity = now }, Cmd.none )

        ( Ready model, NewWordChain wordChain ) ->
            ( Ready
                { model
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

        ( Ready model, InputHanzi txt ) ->
            ( Ready
                { model | currentInput = txt }
            , Cmd.none
            )

        ( Ready model, KeyboardInput char ) ->
            ( Ready { model | currentInput = model.currentInput ++ char }, Cmd.none )

        ( Ready model, KeyboardBackspace ) ->
            ( Ready { model | currentInput = String.dropRight 1 model.currentInput }, Cmd.none )

        ( Ready model, KeyboardClear ) ->
            ( Ready { model | currentInput = "" }, Cmd.none )

        ( Ready model, OnToggleScreenKeyboard ) ->
            ( Ready { model | useScreenKeyboardOnMobile = not model.useScreenKeyboardOnMobile }, Cmd.none )

        ( Ready model, UserPressedEnter ) ->
            let
                txt =
                    String.toCodePoints model.currentInput
                        |> List.filter (\code -> code <= 126)
                        |> String.fromCodePoints

                ( newModel, cmd ) =
                    processInput txt model
                        |> processRoundFinished
            in
            ( Ready newModel, cmd )

        ( Ready model, ToNextWord ) ->
            if List.length model.wordsFound == List.length (allWords model.dictsActive) then
                ( GameFinished model, Cmd.none )

            else
                let
                    dictionary =
                        allWords model.dictsActive
                            |> List.filter (\word -> not <| List.member word model.wordsFound)
                in
                ( Ready model
                , Cmd.batch
                    [ Random.generate NewWordChain (wordChainGenerator dictionary)
                    , Browser.Dom.focus idInput
                        |> Task.attempt (\_ -> NoOp)
                    ]
                )

        ( Ready model, OnGiveUpClicked ) ->
            let
                newGameStats =
                    model.gameStats
                        |> withAddRetries (List.length model.wordChain)
                        |> withAddAttempts (List.length model.wordChain)
            in
            ( Ready
                { model
                    | gameState = TooManyWrongAnswers
                    , showTodoUpdate = Just (List.length model.wordChain)
                    , showTodoUpdateTimer = 2
                    , gameStats = newGameStats
                }
            , Storage.setStorage <| { name = "game-stats", json = encodeGameStats newGameStats }
            )

        ( Ready model, OnRestartClick ) ->
            restartGame model

        ( GameFinished model, OnRestartClick ) ->
            restartGame model

        ( Ready model, OnToggleHelp ) ->
            ( Ready
                { model
                    | showHelp = not model.showHelp
                    , showDictionaryModal = False
                }
            , Cmd.none
            )

        ( Ready model, OnToggleDictionaryModal ) ->
            ( Ready
                { model
                    | showDictionaryModal = not model.showDictionaryModal
                    , showHelp = False
                }
            , Cmd.none
            )

        ( Ready model, OnToggleDictionaryShowAllWords ) ->
            ( Ready { model | unhideDictionary = not model.unhideDictionary }, Cmd.none )

        ( Ready model, OnToggleDictionaryActive dictName ) ->
            ( Ready
                { model
                    | dictsActive =
                        Dict.update dictName
                            (\v ->
                                case v of
                                    Nothing ->
                                        Just True

                                    Just active ->
                                        Just <| not active
                            )
                            model.dictsActive
                }
            , Cmd.none
            )

        --
        ( Ready model, OnMouseEnterCharacter id ) ->
            ( Ready { model | showPopupForCharacter = Just id }, Cmd.none )

        ( Ready model, OnMouseLeaveCharacter ) ->
            ( Ready { model | showPopupForCharacter = Nothing }, Cmd.none )

        ( Ready model, OnTick now ) ->
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
            model
                |> withUpdateShowTodo
                |> withUpdateNotifications
                |> liftModel Ready

        ( GameFinished _, _ ) ->
            ( session, Cmd.none )

        --
        ( LoadingDeviceType, _ ) ->
            ( session, Cmd.none )

        ( LoadingGameStatsFromStorage _, _ ) ->
            ( session, Cmd.none )

        ( LoadingWordsFoundFromStorage _ _, _ ) ->
            ( session, Cmd.none )

        ( Initializing _ _ _, _ ) ->
            ( session, Cmd.none )

        ( Ready _, _ ) ->
            ( session, Cmd.none )


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
        [ Random.generate NewWordChain (wordChainGenerator <| allWords model.dictsActive)
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


subscriptions : Session -> Sub Msg
subscriptions session =
    Sub.batch
        [ Storage.storageLoaded StorageLoaded
        , case session of
            Ready _ ->
                Time.every 1000 OnTick

            _ ->
                Sub.none
        , Notifications.permissionChanged OnNotificationPermissionChanged
        , Browser.Events.onResize OnResizeViewport
        ]



-- VIEW


view : Session -> Html.Html Msg
view session =
    layoutWith { options = [ focusStyle { backgroundColor = Nothing, borderColor = Nothing, shadow = Nothing } ] } [ width fill, height fill ] <|
        case session of
            Ready model ->
                case model.device of
                    Mobile ->
                        viewMobile model

                    _ ->
                        viewDesktop model

            GameFinished model ->
                viewGameFinished model.gameStats

            _ ->
                UI.spinner


viewMobile model =
    let
        modals =
            column
                [ width fill
                , height fill
                , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
                ]
                [ viewDictionaryModal model
                , Help.viewMobile model.showHelp OnToggleHelp NoOpString
                ]
    in
    column [ width fill, height fill ]
        [ mobileViewTopBar model
        , row [ width fill, height fill, inFront modals ]
            [ el [ height fill, width fill ] <|
                column [ width fill, height fill, spacing 20, paddingXY 0 10 ]
                    [ column [ width fill, height shrink, spacing 20, UI.floating, padding 10 ]
                        (List.indexedMap
                            (\wordId word ->
                                row [ width fill, spacing 20 ]
                                    [ el [ width <| fillPortion 1 ] <| viewWordAnswers model wordId word
                                    , el [ width <| fillPortion 1 ] <| viewWordEnglishMobile word
                                    ]
                            )
                            model.wordChain
                        )
                    , viewWrongAnwers <| allWrongAnswers model
                    , case model.gameState of
                        NotDone ->
                            el [ centerX ] <| UI.niceButton "I give up, show me the answers" OnGiveUpClicked Nothing

                        _ ->
                            el [ centerX, height <| px 40 ] none
                    , case model.gameState of
                        NotDone ->
                            viewInput model

                        _ ->
                            UI.niceButtonWith [ centerX, htmlAttribute <| Html.Attributes.id idNextButton ] "Next" ToNextWord Nothing
                    , if model.useScreenKeyboardOnMobile then
                        el [ alignBottom, width fill, height <| maximum 300 fill ] <| MobileUI.viewKeyboard KeyboardInput KeyboardBackspace KeyboardClear

                      else
                        none
                    ]
            ]
        ]


viewDesktop : Model -> Element Msg
viewDesktop model =
    let
        modals =
            row [ width fill, height fill, htmlAttribute <| Html.Attributes.style "pointer-events" "none" ]
                [ viewDictionaryModal model
                , Help.viewDesktop model.showHelp OnToggleHelp NoOpString
                ]
    in
    column [ width fill, height fill, inFront viewFooter ]
        [ viewTopBar model
        , row [ width fill, height fill, inFront modals ]
            [ el [ height fill, width <| fillPortion 1 ] none
            , el [ height fill, width <| fillPortion 5 ] <|
                column [ centerX, centerY, spacing 50 ]
                    [ column [ spacing 20, UI.floating, padding 40 ]
                        (List.indexedMap
                            (\wordId word ->
                                row [ width <| px 600, spacing 20 ]
                                    [ el [ width <| fillPortion 1 ] <| viewWordAnswers model wordId word
                                    , el [ width <| fillPortion 1 ] <| viewWordEnglish word
                                    ]
                            )
                            model.wordChain
                        )
                    , viewWrongAnwers <| allWrongAnswers model
                    , case model.gameState of
                        NotDone ->
                            el [ centerX ] <| UI.niceButton "I give up, show me the answers" OnGiveUpClicked Nothing

                        _ ->
                            el [ centerX, height <| px 40 ] none
                    , case model.gameState of
                        NotDone ->
                            viewInput model

                        _ ->
                            UI.niceButtonWith [ centerX, htmlAttribute <| Html.Attributes.id idNextButton ] "Next" ToNextWord Nothing
                    ]
            , el [ height fill, width <| fillPortion 1 ] none
            ]
        ]


viewDictionaryModal model =
    let
        modalFn =
            if isOnDesktop model then
                UI.modal

            else
                MobileUI.modal
    in
    if model.showDictionaryModal then
        modalFn OnToggleDictionaryModal
            [ row [ width <| minimum 400 fill ]
                [ el [ alignLeft ] <| UI.heading "Dictionary"
                , el [ alignRight ] <|
                    UI.niceButton
                        (if model.unhideDictionary then
                            "Hide all words"

                         else
                            "Show all words"
                        )
                        OnToggleDictionaryShowAllWords
                        (Just <|
                            if model.unhideDictionary then
                                Icons.eyeOff 20

                            else
                                Icons.eye 20
                        )
                ]
            , Words.allDicts
                |> List.map (\( dictName, _ ) -> UI.niceToggleButton dictName (OnToggleDictionaryActive dictName) Nothing <| dictActive model dictName)
                |> row [ centerX, height shrink, paddingXY 10 0, spacing 20 ]
            , Words.allWords model.dictsActive
                |> List.map
                    (\word ->
                        Words.wordToStringParts word
                            |> (\( hanzi, pinyin, english ) ->
                                    let
                                        wordAlreadyDone =
                                            List.member word model.wordsFound

                                        maybeHide str =
                                            if model.unhideDictionary || wordAlreadyDone then
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


viewLogo =
    el [ width fill, height fill ] <|
        el [ centerX, centerY, Font.color UI.white, Font.bold ] <|
            text "Chordle"


viewFooter =
    el [ alignRight, alignBottom, padding 10 ] <|
        newTabLink [] { url = "https://github.com/Nananas/chordle", label = UI.simpleIconButton (Icons.github 32) NoOp }


viewTopBar : Model -> Element Msg
viewTopBar model =
    let
        buttonFn =
            case model.device of
                Desktop ->
                    \str onClick icon -> UI.niceButton str onClick (Just icon)

                _ ->
                    \str onClick icon -> UI.niceIconButton icon onClick
    in
    row [ height <| px 50, width fill, Background.color UI.accentColor, paddingXY 20 0, spacing 20, behindContent viewLogo ]
        [ el
            [ alignLeft
            , onRight <|
                el [] <|
                    case model.showTodoUpdate of
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
            Element.Lazy.lazy2 viewWordsToGo model.dictsActive model.wordsFound
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


mobileViewTopBar : Model -> Element Msg
mobileViewTopBar model =
    row [ height <| px 50, width fill, Background.color UI.accentColor ]
        [ el [ width fill ] <|
            el
                [ centerX
                , onRight <|
                    el [] <|
                        case model.showTodoUpdate of
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
                        ++ (String.fromInt <| (List.length (allWords model.dictsActive) - List.length model.wordsFound))
        , MobileUI.simpleIconButtonInverted (Icons.refresh 20) OnRestartClick
            |> el [ alignRight ]
        , MobileUI.simpleIconButtonInverted (Icons.translate 20) OnToggleDictionaryModal
            |> el [ alignRight ]
        , MobileUI.simpleIconButtonInverted (Icons.questionmark 20) OnToggleHelp
            |> el [ alignRight ]
        ]


viewGameFinished model =
    el [ width fill, height fill ] <|
        column [ centerX, centerY, spacing 40 ]
            [ UI.heading "Game Finished!"
            , text <| "Nr of attempts: " ++ String.fromInt model.attempts
            , text <| "Of which nr of successes: " ++ String.fromInt model.correct
            , text <| "Of which nr of retries: " ++ String.fromInt model.retries
            , el [ centerX ] <| UI.niceButton "Restart" OnRestartClick (Just <| Icons.refresh 20)
            ]


viewWordAnswers : Model -> Int -> Word -> Element Msg
viewWordAnswers model wordId word =
    word.characters
        |> List.indexedMap (viewSingleHanzi model wordId)
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


viewSingleHanzi : Model -> Int -> Int -> Character -> Element Msg
viewSingleHanzi model wordId id character =
    let
        popup =
            case model.showPopupForCharacter of
                Nothing ->
                    none

                Just ( wid, i ) ->
                    if wid == wordId && id == i then
                        column [ centerX ]
                            [ el [ height <| px 10 ] none
                            , el
                                [ Background.color UI.white
                                , padding 10
                                , centerX
                                , Font.color UI.gray
                                , UI.floatingHigh
                                , Border.rounded 10
                                , Font.size 16
                                , Font.regular
                                ]
                              <|
                                text <|
                                    formatPinyin character.pinyinPart
                            ]

                    else
                        none

        known =
            isCharacterKnown model character

        similar =
            isCharacterSimilar model character
    in
    el
        ([ UI.floating
         , UI.rounded 5
         , paddingXY 20 0

         --, width <| maximum 50 shrink
         , width <|
            if known then
                px 50

            else if similar then
                shrink

            else
                px 50
         , height <| px 50
         , Font.color <|
            case model.gameState of
                NotDone ->
                    UI.black

                _ ->
                    toneToColor character.pinyinPart.tone
         , Font.medium
         , Background.color <|
            case model.gameState of
                NotDone ->
                    if known then
                        rgb255 152 226 172

                    else if similar then
                        rgb255 240 230 110

                    else
                        rgb 1 1 1

                _ ->
                    -- TODO: tones
                    rgb 1 1 1
         , Border.color <|
            case model.gameState of
                NotDone ->
                    rgba 0 0 0 0

                _ ->
                    if known then
                        rgb255 152 226 172

                    else
                        rgb255 230 125 125
         , Border.width 2
         , below popup
         ]
            ++ (case model.gameState of
                    NotDone ->
                        []

                    _ ->
                        [ Element.Events.onMouseEnter (OnMouseEnterCharacter ( wordId, id ))
                        , Element.Events.onMouseLeave OnMouseLeaveCharacter
                        ]
               )
        )
    <|
        el [ centerX, centerY, Font.size 20 ] <|
            text <|
                case model.gameState of
                    NotDone ->
                        if known then
                            character.hanzi

                        else if similar then
                            character.pinyinPart.pinyin

                        else
                            ""

                    _ ->
                        character.hanzi


viewInput : Model -> Element Msg
viewInput model =
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
            [ case model.device of
                Mobile ->
                    text model.currentInput

                _ ->
                    Input.text
                        [ onEnter UserPressedEnter
                        , htmlAttribute <| Html.Attributes.id idInput
                        , Border.width 0
                        ]
                        { onChange = InputHanzi
                        , text = model.currentInput
                        , placeholder =
                            Just <|
                                Input.placeholder [ Font.size 14 ] <|
                                    text <|
                                        "Write pinyin with tones here (e.g. hao3 for 好), then press 'OK'"
                                            ++ (if isOnDesktop model then
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


isOnDesktop model =
    case model.device of
        Mobile ->
            False

        _ ->
            True


isOnMobile model =
    not <| isOnDesktop model


dictActive model dictName =
    Dict.get dictName model.dictsActive
        |> Maybe.withDefault True


wordsToGo dictsActive wordsFound =
    Words.allWords dictsActive
        |> List.filter (\w -> not <| List.member w wordsFound)
        |> List.length


wordChainGenerator : List Word -> Random.Generator WordChain
wordChainGenerator dictionary =
    let
        startWordGenerator : Random.Generator Word
        startWordGenerator =
            Random.List.choose dictionary
                |> Random.map (Tuple.first >> Maybe.withDefault emptyWord)

        dictionaryWithoutWord : List Word -> Word -> List Word
        dictionaryWithoutWord dict word =
            List.remove word dict

        oneSimilarWordGenerator : List Word -> Word -> Random.Generator Word
        oneSimilarWordGenerator fromList similarTo =
            similarWords similarTo fromList
                |> Random.List.choose
                |> Random.map (Tuple.first >> Maybe.withDefault emptyWord)

        go wordList similarWord acc count =
            if count <= 0 then
                acc

            else
                similarWords similarWord wordList
    in
    Random.map2
        (\startWord wordCount ->
            ( dictionaryWithoutWord dictionary startWord
                |> similarWords startWord
            , startWord
            , wordCount
            )
        )
        startWordGenerator
        (Random.int 2 4)
        |> Random.andThen
            (\( wordList, startWord, wordCount ) ->
                Random.List.choices wordCount wordList
                    |> Random.map (\( selected, _ ) -> startWord :: selected)
            )


processInput : String -> Model -> Model
processInput txt model =
    case txt |> splitStringIntoPinyin of
        Ok pinyinParts ->
            { model | answers = pinyinParts ++ model.answers |> List.unique, currentInput = "" }

        Err err ->
            -- TODO: show error message
            { model | currentInput = "" }


isPinyinValid : PinyinPart -> WordChain -> Bool
isPinyinValid pinyinPart wordChain =
    wordChain
        |> List.find
            (\word ->
                List.find (\character -> pinyinPartsSimilarity character.pinyinPart pinyinPart == CompletelySimilar) word.characters
                    |> Maybe.map (\_ -> True)
                    |> Maybe.withDefault False
            )
        |> Maybe.map (\_ -> True)
        |> Maybe.withDefault False


isWordFullyKnown : Model -> Word -> Bool
isWordFullyKnown model word =
    word.characters
        |> List.filter (\character -> not <| List.member character.pinyinPart model.answers)
        |> List.length
        |> (==) 0


isCharacterKnown : Model -> Character -> Bool
isCharacterKnown model character =
    List.member character.pinyinPart model.answers


isCharacterSimilar : Model -> Character -> Bool
isCharacterSimilar model character =
    List.any (\part -> part.pinyin == character.pinyinPart.pinyin) model.answers


allWrongAnswers : Model -> List PinyinPart
allWrongAnswers model =
    model.answers
        |> List.filter (\part -> not <| isPinyinValid part model.wordChain)


processRoundFinished : Model -> ( Model, Cmd Msg )
processRoundFinished model =
    let
        focusNextBtn =
            Browser.Dom.focus idNextButton
                |> Task.attempt (\_ -> NoOp)
    in
    if List.length (allWrongAnswers model) > 5 then
        -- Too many wrong answers
        let
            newGameStats =
                model.gameStats
                    |> withAddRetries (List.length model.wordChain)
                    |> withAddAttempts (List.length model.wordChain)
        in
        ( { model
            | gameState = TooManyWrongAnswers
            , showTodoUpdate = Just (List.length model.wordChain)
            , showTodoUpdateTimer = 2
            , gameStats = newGameStats
          }
        , Storage.setStorage { name = "game-stats", json = encodeGameStats newGameStats }
        )

    else
        let
            finished =
                model.wordChain
                    |> List.foldl
                        (\word acc -> isWordFullyKnown model word && acc)
                        True

            wordsFound =
                model.wordsFound
                    ++ model.wordChain
                    |> List.unique
        in
        if finished then
            -- word round finished, we found all correctly
            let
                newGameStats =
                    model.gameStats
                        |> withAddAttempts 1
                        |> withAddOneCorrect
            in
            ( { model
                | gameState = FilledInCorrectly
                , showTodoUpdate = Just -(List.length model.wordChain)
                , showTodoUpdateTimer = 2
                , wordsFound = wordsFound
                , gameStats = newGameStats
              }
            , Cmd.batch
                [ case ( model.device, model.useScreenKeyboardOnMobile ) of
                    ( Mobile, True ) ->
                        Cmd.none

                    _ ->
                        focusNextBtn
                , Storage.setStorage
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
            ( { model | gameState = NotDone }, focusNextBtn )



--


onEnter : msg -> Element.Attribute msg
onEnter msg =
    htmlAttribute
        (Html.Events.on "keyup"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.Decode.succeed msg

                        else
                            Json.Decode.fail "Not the enter key"
                    )
            )
        )



--
