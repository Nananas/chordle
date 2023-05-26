module Daily exposing (..)

import Backend
import Browser.Dom
import Clipboard
import Common
import DailyProgress exposing (..)
import Date
import Dict
import Dict.Extra as Dict
import Element exposing (..)
import Element.Background
import Element.Border
import Element.Font
import Help
import Html.Attributes
import Icons
import Json.Decode
import Json.Encode
import List.Extra as List
import MobileUI
import Process
import Random
import Random.List
import Storage exposing (Storage)
import String.Extra as String
import Task
import UI
import Utils
import WordChain exposing (WordChain)
import Words exposing (..)



-- CONSTANTS


idInput =
    "text-input"



-- TYPES


type Msg
    = OnSeedDateLoaded ( Int, Date.Date )
    | OnStorageLoaded Storage
    | NoOp
    | NoOpIntInt ( Int, Int )
    | UserPressedEnter
    | InputHanzi String
    | OnMouseEnterCharacter ( Int, Int )
    | OnMouseLeaveCharacter
    | OnGiveUpClicked
    | OnClickedHome
    | OnShareClicked String
    | OnClickedToggleShowHistory
    | OnClickedHideHistory
    | KeyboardInput String
    | KeyboardBackspace
    | KeyboardClear
    | BackendMsg Backend.Msg
    | OnClickedHistoryMonthChange Int
    | OnClickedHistoryGotoToday
    | OnToggleHelp
    | NoOpString String


type Model
    = LoadingSeed
    | LoadingStorage Random.Seed Date.Date
    | Playing GameModel
    | GameOver GameOverState EndScore GameModel


type alias GameModel =
    { wordChain : WordChain
    , currentInput : String
    , errorMsg : Maybe String
    , answers : List PinyinPart
    , showPopupForCharacter : Maybe ( Int, Int )
    , attempts : List Attempt
    , today : Date.Date
    , progress : Progress
    , showProgressPopup : Bool
    , historyShownDate : Date.Date
    , showHelp : Bool
    }


type Attempt
    = Correct
    | Wrong
    | Almost


type alias EndScore =
    { mistakes : Int
    , attempts : List Attempt
    }



-- INIT


init =
    ( LoadingSeed
    , Cmd.batch
        [ Task.map dateTimeToUniqueSeed Date.today
            |> Task.perform OnSeedDateLoaded
        ]
    )


initGame wordChain today progress =
    ( Playing
        { wordChain = wordChain
        , currentInput = ""
        , errorMsg = Nothing
        , answers = []
        , showPopupForCharacter = Nothing
        , attempts = []
        , today = today
        , progress = progress
        , showProgressPopup = False
        , historyShownDate = today
        , showHelp = False
        }
    , Process.sleep 100
        |> Task.andThen (\_ -> Browser.Dom.focus idInput)
        |> Task.attempt (\_ -> NoOp)
    )


gameOver : Backend.Uuid -> List String -> DayProgress -> GameModel -> ( Model, Cmd Msg )
gameOver uuid activeDicts dayProgress game =
    let
        state =
            if Dict.member (Date.toRataDie game.today) game.progress then
                Other dayProgress

            else
                First dayProgress

        newProgress =
            Dict.update (Date.toRataDie game.today)
                (\mDP ->
                    case mDP of
                        Nothing ->
                            Just dayProgress

                        _ ->
                            mDP
                )
                game.progress

        score =
            gameToEndScore game
    in
    ( GameOver state score { game | progress = newProgress }
    , Cmd.batch
        [ updateProgressInStorage game.today dayProgress game.progress
        , Backend.postDaily
            uuid
            activeDicts
            { state = state
            , attempts = List.length score.attempts
            , mistakes = score.mistakes
            , rata = Date.toRataDie game.today
            }
            |> Cmd.map BackendMsg
        ]
    )


dateTimeToUniqueSeed : Date.Date -> ( Int, Date.Date )
dateTimeToUniqueSeed today =
    --Debug.log "DEV SEED" 2
    ( Date.toRataDie today
    , today
    )


dayProgressToString p =
    case p of
        Succeeded ->
            "v"

        Failed ->
            "x"

        _ ->
            "?"


stringToDayProgress str =
    case str of
        "x" ->
            Just Failed

        "v" ->
            Just Succeeded

        "?" ->
            Just GiveUp

        _ ->
            Nothing


dayProgressDecoder =
    Json.Decode.map stringToDayProgress Json.Decode.string


parseProgress : Json.Decode.Value -> Progress
parseProgress storage =
    let
        decoder =
            Json.Decode.field "progress" (Json.Decode.dict dayProgressDecoder)
    in
    case Json.Decode.decodeValue decoder storage of
        Err _ ->
            Dict.empty

        Ok progress ->
            progress
                |> Dict.filterMap (\_ v -> identity v)
                |> Dict.mapKeys (String.toInt >> Maybe.withDefault 0)


encodeProgress : Progress -> Json.Encode.Value
encodeProgress progress =
    Json.Encode.object
        [ ( "progress"
          , progress |> Json.Encode.dict String.fromInt (dayProgressToString >> Json.Encode.string)
          )
        ]


updateProgressInStorage : Date.Date -> DayProgress -> Progress -> Cmd Msg
updateProgressInStorage today dayProgress progress =
    Storage.setStorage
        { name = "progress"
        , json =
            progress
                |> Dict.update (Date.toRataDie today)
                    (\mP ->
                        case mP of
                            Nothing ->
                                Just dayProgress

                            _ ->
                                mP
                    )
                |> encodeProgress
        }


genWordChain : Int -> Random.Seed -> List Word -> List Word -> WordChain
genWordChain count seed dictionary wordsWithoutConnection =
    let
        chainMaxLength =
            count // 2 + 1

        ( wordChain, _ ) =
            WordChain.multiChainGenerator count chainMaxLength dictionary
                |> Random.andThen
                    (\wc ->
                        let
                            wcOnlyWords =
                                wc
                                    |> List.map .word
                        in
                        wordsWithoutConnection
                            |> List.filter (\w -> not <| List.member w wcOnlyWords)
                            |> Random.List.choose
                            |> Random.map
                                (\( mW, _ ) ->
                                    case mW of
                                        Nothing ->
                                            wc

                                        Just w ->
                                            wc ++ [ WordChain.WordChainWord w 0 True ]
                                )
                    )
                |> (\g -> Random.step g seed)

        --)
        --seed
    in
    wordChain


update : Backend.Uuid -> Words.Dictionaries -> List String -> Msg -> Model -> ( Model, Cmd Msg )
update uuid dictionaries activeDicts msg model =
    case ( model, msg ) of
        ( LoadingSeed, OnSeedDateLoaded ( seedInt, today ) ) ->
            let
                seed =
                    Random.initialSeed seedInt
            in
            ( LoadingStorage seed today, Storage.loadStorage "progress" )

        ( LoadingStorage seed today, OnStorageLoaded storage ) ->
            let
                dictionary =
                    allWords activeDicts dictionaries

                wordsWithoutConnection =
                    singleWordsList dictionary

                wordChain =
                    genWordChain 11 seed dictionary wordsWithoutConnection

                progress =
                    parseProgress storage.json
            in
            initGame
                wordChain
                today
                progress

        ( Playing game, OnToggleHelp ) ->
            ( Playing { game | showHelp = not game.showHelp }
            , Cmd.none
            )

        ( GameOver s e game, OnToggleHelp ) ->
            ( GameOver s e { game | showHelp = not game.showHelp }
            , Cmd.none
            )

        ( Playing game, InputHanzi txt ) ->
            ( Playing
                { game | currentInput = txt, errorMsg = Nothing }
            , Cmd.none
            )

        ( Playing game, UserPressedEnter ) ->
            if game.currentInput == "" then
                ( model, Cmd.none )

            else
                let
                    txt =
                        String.toCodePoints game.currentInput
                            |> List.filter (\code -> code <= 126)
                            |> String.fromCodePoints
                in
                processInput txt game
                    |> processRoundFinished uuid activeDicts

        ( Playing game, OnGiveUpClicked ) ->
            gameOver uuid activeDicts GiveUp game

        ( Playing game, OnMouseEnterCharacter id ) ->
            ( Playing { game | showPopupForCharacter = Just id }, Cmd.none )

        ( Playing game, OnMouseLeaveCharacter ) ->
            ( Playing { game | showPopupForCharacter = Nothing }, Cmd.none )

        -- KEYBOARD
        ( Playing game, KeyboardInput char ) ->
            ( Playing { game | currentInput = game.currentInput ++ char, errorMsg = Nothing }, Cmd.none )

        ( Playing game, KeyboardBackspace ) ->
            ( Playing { game | currentInput = String.dropRight 1 game.currentInput, errorMsg = Nothing }, Cmd.none )

        ( Playing game, KeyboardClear ) ->
            ( Playing { game | currentInput = "", errorMsg = Nothing }, Cmd.none )

        -- OTHER
        ( GameOver success endScore game, OnMouseEnterCharacter id ) ->
            ( GameOver success endScore { game | showPopupForCharacter = Just id }, Cmd.none )

        ( GameOver success endScore game, OnMouseLeaveCharacter ) ->
            ( GameOver success endScore { game | showPopupForCharacter = Nothing }, Cmd.none )

        ( GameOver (First _) _ _, OnShareClicked shareText ) ->
            ( model, Clipboard.writeToClipboard shareText )

        ( Playing game, OnClickedToggleShowHistory ) ->
            ( Playing { game | showProgressPopup = not game.showProgressPopup }, Cmd.none )

        ( GameOver success endScore game, OnClickedToggleShowHistory ) ->
            ( GameOver success endScore { game | showProgressPopup = not game.showProgressPopup }, Cmd.none )

        ( Playing game, OnClickedHideHistory ) ->
            ( Playing { game | showProgressPopup = False }, Cmd.none )

        ( GameOver success endScore game, OnClickedHideHistory ) ->
            ( GameOver success endScore { game | showProgressPopup = False }, Cmd.none )

        -- TODO: fix redundant copies
        ( Playing game, OnClickedHistoryMonthChange i ) ->
            ( Playing { game | historyShownDate = Date.add Date.Months i game.historyShownDate }, Cmd.none )

        ( GameOver success endScore game, OnClickedHistoryMonthChange i ) ->
            ( GameOver success endScore { game | historyShownDate = Date.add Date.Months i game.historyShownDate }, Cmd.none )

        ( Playing game, OnClickedHistoryGotoToday ) ->
            ( Playing { game | historyShownDate = game.today }, Cmd.none )

        ( GameOver success endScore game, OnClickedHistoryGotoToday ) ->
            ( GameOver success endScore { game | historyShownDate = game.today }, Cmd.none )

        -- OTHER
        _ ->
            ( model, Cmd.none )


processInput : String -> GameModel -> GameModel
processInput txt game =
    case txt |> splitStringIntoPinyin of
        Ok pinyinParts ->
            { game
                | answers = pinyinParts ++ game.answers |> List.unique
                , currentInput = ""
            }

        Err err ->
            -- TODO: show error message
            { game | currentInput = "", errorMsg = Just err }


processRoundFinished : Backend.Uuid -> List String -> GameModel -> ( Model, Cmd Msg )
processRoundFinished uuid activeDicts game =
    if List.length (WordChain.wrongAnswersOf game.wordChain game.answers) > 5 then
        -- Too many wrong answers
        gameOver uuid activeDicts Failed game

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
            gameOver uuid activeDicts Succeeded game

        else
            ( Playing game, Cmd.none )


gameToEndScore : GameModel -> EndScore
gameToEndScore game =
    { mistakes = List.length (WordChain.wrongAnswersOf game.wordChain game.answers)
    , attempts =
        game.answers
            |> List.reverse
            |> List.map
                (\a ->
                    if WordChain.isPinyinValid a game.wordChain then
                        Correct

                    else if WordChain.isPinyinSimilar a game.wordChain then
                        Almost

                    else
                        Wrong
                )
    }



--


viewHistoryModal : Bool -> GameModel -> Element Msg
viewHistoryModal onMobile game =
    let
        rata =
            Date.toRataDie game.historyShownDate

        month =
            Date.monthNumber game.historyShownDate

        monthDays =
            List.range (rata - 31) (rata + 31)
                |> List.map Date.fromRataDie
                |> List.filter
                    (\d -> Date.monthNumber d == month)

        offset =
            List.head monthDays
                |> Maybe.map (\d -> Date.weekdayNumber d - 1)
                |> Maybe.withDefault 0

        monthDaysOffsetted =
            List.repeat offset Nothing ++ List.map Just monthDays

        progressOfDateToColor d =
            game.progress
                |> Dict.get (Date.toRataDie d)
                |> Maybe.map (\p -> [ Element.Background.color <| dayProgressToColor p ])
                |> Maybe.withDefault [ Element.Border.width 1, Element.Border.color UI.accentColorHighlight ]

        monthName =
            Date.format "MMMM" game.historyShownDate

        yearName =
            Date.format "y" game.historyShownDate

        squareSize =
            50

        dateToSquare mD =
            let
                ( attrs, label ) =
                    case mD of
                        Nothing ->
                            ( [], "" )

                        Just d ->
                            ( progressOfDateToColor d
                                ++ (if Date.diff Date.Days d game.today == 0 then
                                        [ Element.Border.width 2, Element.Border.color UI.accentColor ]

                                    else
                                        []
                                   )
                            , String.fromInt <| Date.day d
                            )
            in
            el ([ width (px squareSize), height (px squareSize), UI.rounded 3 ] ++ attrs) <|
                el [ centerX, centerY ] <|
                    text label

        weekdayHeader =
            List.range 1 7
                |> List.map
                    (\i ->
                        Date.numberToWeekday i
                            |> Date.fromWeekDate 2020 1
                            |> Date.format "E"
                            |> String.slice 0 3
                            |> text
                            |> el [ centerX, centerY ]
                            |> el [ width (px squareSize), height (px squareSize) ]
                    )
                |> row [ spacing 2, Element.Font.color UI.accentColor ]

        recurse : List (Element Msg) -> List (Maybe Date.Date) -> Element Msg
        recurse acc days =
            case List.splitAt 7 days of
                ( [], _ ) ->
                    column [ spacing 2 ]
                        (weekdayHeader
                            :: List.reverse acc
                        )

                ( week, rest ) ->
                    let
                        weekRow =
                            week
                                |> List.map dateToSquare
                                |> row [ spacing 2, Element.Font.color UI.accentColor, Element.Font.size 20 ]
                    in
                    recurse (weekRow :: acc) rest

        stats =
            game.progress
                |> Dict.toList
                |> List.foldl
                    (\( _, dp ) acc ->
                        case dp of
                            Succeeded ->
                                { acc | successes = acc.successes + 1 }

                            Failed ->
                                { acc | failures = acc.failures + 1 }

                            GiveUp ->
                                { acc | givenups = acc.givenups + 1 }
                    )
                    { successes = 0
                    , failures = 0
                    , givenups = 0
                    }

        monthlyStats =
            game.progress
                |> Dict.toList
                |> List.foldl
                    (\( r, dp ) acc ->
                        if
                            month
                                == (Date.fromRataDie r |> Date.monthNumber)
                                && Date.year game.historyShownDate
                                == (Date.fromRataDie r |> Date.year)
                        then
                            case dp of
                                Succeeded ->
                                    { acc | successes = acc.successes + 1 }

                                Failed ->
                                    { acc | failures = acc.failures + 1 }

                                GiveUp ->
                                    { acc | givenups = acc.givenups + 1 }

                        else
                            acc
                    )
                    { successes = 0
                    , failures = 0
                    , givenups = 0
                    }

        total =
            Dict.size game.progress
                |> max 1
                |> toFloat

        monthTotal =
            monthlyStats.successes
                + monthlyStats.failures
                + monthlyStats.givenups
                |> max 1

        modalFn =
            if onMobile then
                MobileUI.modal

            else
                UI.modal
    in
    modalFn
        OnClickedToggleShowHistory
        [ el [ alignTop, centerX ] <| UI.heading "History"
        , column [ centerX, spacing 10 ]
            [ row [ width fill ]
                [ el [] <| UI.niceText (monthName ++ " " ++ yearName)
                , el [ alignRight ] <| UI.simpleIconButton (Icons.chevronLeft 20) (OnClickedHistoryMonthChange -1) "Go back one month"
                , UI.simpleIconButton (Icons.calendar 20) OnClickedHistoryGotoToday "Go to Today"
                , UI.simpleIconButton (Icons.chevronRight 20) (OnClickedHistoryMonthChange 1) "Go forward one month"
                ]
            , el [ centerX, centerY, height (px (7 * squareSize)) ] <| recurse [] monthDaysOffsetted
            ]
        , el [ width fill, Element.Font.color UI.accentColorHighlight, UI.bottomBorder 2, paddingXY 50 0 ] none
        , row [ centerX, spacing 20 ]
            [ column [ spacing 10, Element.Font.color UI.accentColor ]
                [ el [ alignLeft ] <| UI.niceText "Monthly Summary"
                , text <| "Correct: " ++ (String.fromInt <| round <| monthlyStats.successes / monthTotal * 100) ++ "%"
                , text <| "Wrong: " ++ (String.fromInt <| round <| monthlyStats.failures / monthTotal * 100) ++ "%"
                , text <| "Given up: " ++ (String.fromInt <| round <| monthlyStats.givenups / monthTotal * 100) ++ "%"
                ]
            , el [ width (px 1), height fill, Element.Border.widthEach { bottom = 0, left = 0, right = 2, top = 0 }, Element.Font.color UI.accentColorHighlight ] none
            , column [ spacing 10, Element.Font.color UI.accentColor ]
                [ el [ alignLeft ] <| UI.niceText "Total Summary"
                , text <| "Correct: " ++ (String.fromInt <| round <| stats.successes / total * 100) ++ "%"
                , text <| "Wrong: " ++ (String.fromInt <| round <| stats.failures / total * 100) ++ "%"
                , text <| "Given up: " ++ (String.fromInt <| round <| stats.givenups / total * 100) ++ "%"
                ]
            ]
        ]


view : Device -> Model -> Element Msg
view device model =
    let
        onMobile =
            Utils.isOnMobile device

        modals game =
            if game.showHelp then
                Help.view game.showHelp onMobile OnToggleHelp NoOpString

            else if game.showProgressPopup then
                viewHistoryModal onMobile game

            else
                none

        wordList game wordStateFn =
            let
                offsetElements offset isOrphan =
                    if isOrphan then
                        [ el
                            [ Element.Font.color UI.accentColor
                            , width <| px 50
                            , htmlAttribute <| Html.Attributes.title "This word's hanzi are not connected to any other word"
                            ]
                          <|
                            el [ centerX ] <|
                                Icons.starFilled 24
                        ]

                    else if onMobile then
                        []

                    else
                        List.repeat (min 4 offset) (el [ width <| px <| 50 ] none)
            in
            game.wordChain
                |> List.indexedMap
                    (\wordId { word, offset, isOrphan } ->
                        row [ width fill, spacing 20, padding 5 ]
                            [ offsetElements offset isOrphan
                                ++ (word.characters
                                        |> List.indexedMap
                                            (\id character ->
                                                viewSingleHanzi
                                                    onMobile
                                                    wordStateFn
                                                    game.showPopupForCharacter
                                                    wordId
                                                    id
                                                    character
                                            )
                                   )
                                |> row [ spacing 10, width <| fillPortion 1 ]
                            , el [ width <| fillPortion 2 ] <| Common.viewWordEnglish onMobile word
                            ]
                    )
    in
    case model of
        Playing game ->
            let
                wordStateFn character =
                    --WordChain.Show False
                    if isCharacterKnown character game.answers then
                        WordChain.Known

                    else if isCharacterSimilar character game.answers then
                        WordChain.Similar

                    else
                        WordChain.Unknown
            in
            Common.viewContainer onMobile
                True
                { popup = modals game
                , topbar = viewTopBar
                , wordlist = wordList game wordStateFn
                , bottom =
                    [ Common.viewInput onMobile game { msgUserPressedEnter = UserPressedEnter, msgInputHanzi = InputHanzi }
                    , Common.viewWrongAnwers onMobile (WordChain.wrongAnswersOf game.wordChain game.answers)
                    , el [ centerX ] <| UI.niceButton "I give up, show me the answers" OnGiveUpClicked Nothing
                    , viewIsGameAReplay game
                    ]
                , msgKeyboardInput = KeyboardInput
                , msgKeyboardBackspace = KeyboardBackspace
                , msgKeyboardClear = KeyboardClear
                }

        GameOver state endScore game ->
            let
                wordStateFn character =
                    WordChain.Show <| isCharacterKnown character game.answers

                success =
                    case state of
                        First s ->
                            s

                        Other s ->
                            s
            in
            Common.viewContainer onMobile
                False
                { popup = modals game
                , topbar = viewTopBar
                , wordlist = wordList game wordStateFn
                , bottom =
                    [ el [ width fill, height shrink, paddingXY 0 20 ] <|
                        (if onMobile then
                            column

                         else
                            row
                        )
                            [ spacing 50, centerX, centerY ]
                            [ column [ width (px 200), spacing 20 ]
                                [ UI.heading <|
                                    if success == Succeeded then
                                        "Well done!"

                                    else
                                        "Better next time!"
                                , row [ width fill ] [ el [ alignLeft ] <| UI.niceText <| "Attemps:", el [ alignRight ] <| text <| String.fromInt <| List.length endScore.attempts ]
                                , row [ width fill ] [ el [ alignLeft ] <| UI.niceText <| "Mistakes:", el [ alignRight ] <| text <| String.fromInt endScore.mistakes ]
                                ]
                            , viewGameOverShare endScore game.today state
                            ]
                    ]
                , msgKeyboardInput = KeyboardInput
                , msgKeyboardBackspace = KeyboardBackspace
                , msgKeyboardClear = KeyboardClear
                }

        _ ->
            UI.spinner


viewTopBar =
    row [ height <| px 50, width fill, Element.Background.color UI.accentColor, paddingXY 20 0, spacing 20, behindContent <| UI.viewLogo "Daily" ]
        [ UI.niceIconButton (Icons.arrowBack 20) OnClickedHome "Home"
        , el [ alignRight ] <| UI.niceIconButton (Icons.academicCap 20) OnClickedToggleShowHistory "Show history"
        , UI.niceIconButton (Icons.questionmark 20) OnToggleHelp "Help"
        ]


viewSingleHanzi onMobile wordStateFn showPopupForCharacter wordId id character =
    WordChain.viewSingleHanzi onMobile
        { state = wordStateFn character
        , showPopup = showPopupForCharacter
        , onMouseEnterCharacterMsg = OnMouseEnterCharacter
        , onMouseLeaveCharacterMsg = OnMouseLeaveCharacter
        , wordId = wordId
        , id = id
        , character = character
        }


viewIsGameAReplay game =
    el [ centerX, Element.Font.color <| UI.gray, Element.Font.size 14 ] <|
        if Dict.member (Date.toRataDie game.today) game.progress then
            column [ spacing 5, padding 5 ]
                [ paragraph [] [ text "You already played today." ]
                , paragraph [] [ text "This game is a replay and will not count towards your score." ]
                ]

        else
            none


dayProgressToEmoji progress =
    case progress of
        Succeeded ->
            "😌"

        Failed ->
            "😐"

        GiveUp ->
            "😥"


dayProgressToColor progress =
    case progress of
        Succeeded ->
            UI.correctColorLight

        Failed ->
            UI.errorColorLight

        GiveUp ->
            UI.lightGray


viewGameOverShare : EndScore -> Date.Date -> GameOverState -> Element Msg
viewGameOverShare endScore today state =
    let
        square attempt =
            case attempt of
                Correct ->
                    "🟩"

                Wrong ->
                    "🟥"

                Almost ->
                    "🟨"

        progress =
            case state of
                First s ->
                    s

                Other s ->
                    s

        recurse acc sqs =
            case List.splitAt 5 sqs of
                ( [], _ ) ->
                    List.reverse acc

                ( s, more ) ->
                    recurse (String.join "" s :: acc) more

        intoRows s =
            recurse [] s
    in
    case state of
        First _ ->
            let
                shareText =
                    "Chordle"
                        ++ "\n"
                        ++ Date.format "dd-MMM-yyyy" today
                        ++ "\n"
                        ++ dayProgressToEmoji progress
                        ++ " "
                        ++ (String.fromInt <| List.length endScore.attempts)
                        ++ "/"
                        ++ String.fromInt endScore.mistakes
                        ++ "\n"
                        ++ (endScore.attempts
                                |> List.map square
                                |> intoRows
                                |> String.join "\n"
                           )
            in
            column [ centerX, UI.floating, padding 10, spacing 10 ]
                [ text shareText
                , UI.niceButton "Copy" (OnShareClicked shareText) Nothing
                ]

        _ ->
            none


subscriptions =
    Sub.none
