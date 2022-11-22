module Daily exposing (..)

import Backend
import Browser.Dom
import Clipboard
import Common
import DailyProgress exposing (..)
import Date
import Dict exposing (Dict)
import Dict.Extra as Dict
import Element exposing (..)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Html.Attributes
import Icons
import Json.Decode
import Json.Encode
import List.Extra as List
import MobileUI
import Process
import Random
import Storage exposing (Storage)
import String.Extra as String
import Task
import Time
import Time.Extra as Time
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


type Model
    = LoadingSeed
    | LoadingStorage Random.Seed Date.Date
    | Playing GameModel
    | GameOver GameOverState EndScore GameModel


type alias GameModel =
    { wordChain : List ( Word, Int )
    , currentInput : String
    , answers : List PinyinPart
    , showPopupForCharacter : Maybe ( Int, Int )
    , attempts : List Attempt
    , today : Date.Date
    , progress : Progress
    , showProgressPopup : Bool
    , historyShownDate : Date.Date
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
        , answers = []
        , showPopupForCharacter = Nothing
        , attempts = []
        , today = today
        , progress = progress
        , showProgressPopup = False
        , historyShownDate = today
        }
    , Process.sleep 100
        |> Task.andThen (\_ -> Browser.Dom.focus idInput)
        |> Task.attempt (\_ -> NoOp)
    )


gameOver : Backend.Uuid -> DayProgress -> GameModel -> ( Model, Cmd Msg )
gameOver uuid dayProgress game =
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
        Err err ->
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


offsetWords : List Word -> List ( Word, Int, Hanzi )
offsetWords words =
    let
        hasOffsetWith : Word -> ( Word, Int, Hanzi ) -> Maybe ( Word, Int, Hanzi )
        hasOffsetWith word ( other, otherIndex, _ ) =
            word.characters
                |> List.indexedFoldl
                    (\charIndex { hanzi } mResult ->
                        case mResult of
                            Nothing ->
                                Utils.indexedFind (\oChar -> oChar.hanzi == hanzi) other.characters
                                    |> Maybe.map (\( i, e ) -> ( word, i + otherIndex - charIndex, e.hanzi ))

                            _ ->
                                mResult
                    )
                    Nothing

        shiftedWords =
            words
                |> List.foldr
                    (\word acc ->
                        case acc of
                            [] ->
                                [ ( word, 0, List.map .hanzi word.characters |> String.concat ) ]

                            _ ->
                                (List.findMap (hasOffsetWith word) acc
                                    |> Maybe.withDefault ( word, List.length acc * 10, List.map .hanzi word.characters |> String.concat )
                                )
                                    :: acc
                    )
                    []

        minimumIndex =
            shiftedWords
                |> List.map (\( _, i, _ ) -> i)
                |> List.minimum
                |> Maybe.withDefault 0
    in
    shiftedWords
        |> List.map (\( w, i, h ) -> ( w, i - minimumIndex, h ))


sortWords : List ( Word, Int, Hanzi ) -> List ( Word, Int )
sortWords words =
    let
        comp : ( Word, Int, Hanzi ) -> ( Word, Int, Hanzi ) -> Order
        comp ( w1, i1, h1 ) ( w2, i2, h2 ) =
            compare h1 h2
    in
    words
        |> List.sortWith comp
        |> List.map (\( w, i, _ ) -> ( w, i ))


update : Backend.Uuid -> Msg -> Model -> ( Model, Cmd Msg )
update uuid msg model =
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
                    allWords Dict.empty

                ( wordChain, _ ) =
                    Random.step
                        (WordChain.multiChainGenerator 11 dictionary)
                        seed

                progress =
                    parseProgress storage.json
            in
            initGame
                (wordChain
                    |> offsetWords
                    |> sortWords
                )
                today
                progress

        ( Playing game, InputHanzi txt ) ->
            ( Playing
                { game | currentInput = txt }
            , Cmd.none
            )

        ( Playing game, UserPressedEnter ) ->
            let
                txt =
                    String.toCodePoints game.currentInput
                        |> List.filter (\code -> code <= 126)
                        |> String.fromCodePoints
            in
            processInput txt game
                |> processRoundFinished uuid

        ( Playing game, OnGiveUpClicked ) ->
            gameOver uuid GiveUp game

        ( Playing game, OnMouseEnterCharacter id ) ->
            ( Playing { game | showPopupForCharacter = Just id }, Cmd.none )

        ( Playing game, OnMouseLeaveCharacter ) ->
            ( Playing { game | showPopupForCharacter = Nothing }, Cmd.none )

        -- KEYBOARD
        ( Playing game, KeyboardInput char ) ->
            ( Playing { game | currentInput = game.currentInput ++ char }, Cmd.none )

        ( Playing game, KeyboardBackspace ) ->
            ( Playing { game | currentInput = String.dropRight 1 game.currentInput }, Cmd.none )

        ( Playing game, KeyboardClear ) ->
            ( Playing { game | currentInput = "" }, Cmd.none )

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
            { game | currentInput = "" }


processRoundFinished : Backend.Uuid -> GameModel -> ( Model, Cmd Msg )
processRoundFinished uuid game =
    if List.length (allWrongAnswers game) > 5 then
        -- Too many wrong answers
        gameOver uuid Failed game

    else
        let
            finished =
                game.wordChain
                    |> List.foldl
                        (\( word, _ ) acc -> isWordFullyKnown game word && acc)
                        True
        in
        if finished then
            -- word round finished, we found all correctly
            gameOver uuid Succeeded game

        else
            ( Playing game, Cmd.none )


allWrongAnswers : GameModel -> List PinyinPart
allWrongAnswers game =
    game.answers
        |> List.filter (\part -> not <| isPinyinValid part game.wordChain)


isWordFullyKnown : GameModel -> Word -> Bool
isWordFullyKnown game word =
    word.characters
        |> List.filter (\character -> not <| List.member character.pinyinPart game.answers)
        |> List.length
        |> (==) 0


isPinyinValid : PinyinPart -> List ( Word, Int ) -> Bool
isPinyinValid pinyinPart wordChain =
    wordChain
        |> List.find
            (\( word, _ ) ->
                List.find (\character -> pinyinPartsSimilarity character.pinyinPart pinyinPart == CompletelySimilar) word.characters
                    |> Maybe.map (\_ -> True)
                    |> Maybe.withDefault False
            )
        |> Maybe.map (\_ -> True)
        |> Maybe.withDefault False


isPinyinSimilar : PinyinPart -> List ( Word, Int ) -> Bool
isPinyinSimilar pinyinPart wordChain =
    wordChain
        |> List.find
            (\( word, _ ) ->
                List.find (\character -> pinyinPartsSimilarity character.pinyinPart pinyinPart == PinyinSimilar) word.characters
                    |> Maybe.map (\_ -> True)
                    |> Maybe.withDefault False
            )
        |> Maybe.map (\_ -> True)
        |> Maybe.withDefault False


gameToEndScore game =
    { mistakes = List.length (allWrongAnswers game)
    , attempts =
        game.answers
            |> List.reverse
            |> List.map
                (\a ->
                    if isPinyinValid a game.wordChain then
                        Correct

                    else if isPinyinSimilar a game.wordChain then
                        Almost

                    else
                        Wrong
                )
    }



--


isCharacterKnown : GameModel -> Character -> Bool
isCharacterKnown game character =
    List.member character.pinyinPart game.answers


isCharacterSimilar : GameModel -> Character -> Bool
isCharacterSimilar game character =
    List.any (\part -> part.pinyin == character.pinyinPart.pinyin) game.answers


wrongAnswersOf : GameModel -> List PinyinPart
wrongAnswersOf game =
    game.answers
        |> List.filter (\part -> not <| isPinyinValid part game.wordChain)


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
                    (\( r, dp ) acc ->
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

        total =
            Dict.size game.progress
                |> toFloat

        modalFn =
            if onMobile then
                MobileUI.modal

            else
                UI.modal
    in
    modalFn
        OnClickedToggleShowHistory
        [ el [ alignTop ] <| UI.heading "History"
        , column [ width fill, spacing 10 ]
            [ row [ width fill ]
                [ el [] <| UI.niceText (monthName ++ " " ++ yearName)
                , el [ alignRight ] <| UI.simpleIconButton (Icons.chevronLeft 20) (OnClickedHistoryMonthChange -1) "Go back one month"
                , UI.simpleIconButton (Icons.calendar 20) OnClickedHistoryGotoToday "Go to Today"
                , UI.simpleIconButton (Icons.chevronRight 20) (OnClickedHistoryMonthChange 1) "Go forward one month"
                ]
            , el [ centerX, centerY ] <| recurse [] monthDaysOffsetted
            ]
        , column [ spacing 10, Element.Font.color UI.accentColor ]
            [ el [ alignLeft ] <| UI.niceText "Summary:"
            , text <| "Correct: " ++ (String.fromInt <| round <| stats.successes / total * 100) ++ "%"
            , text <| "Wrong: " ++ (String.fromInt <| round <| stats.failures / total * 100) ++ "%"
            , text <| "Given up: " ++ (String.fromInt <| round <| stats.givenups / total * 100) ++ "%"
            ]
        ]


view : Device -> Model -> Element Msg
view device model =
    let
        onMobile =
            Utils.isOnMobile device

        historyPopup : GameModel -> Element Msg
        historyPopup game =
            if game.showProgressPopup then
                viewHistoryModal onMobile game

            else
                none

        wordList game wordStateFn =
            let
                offsetElements offset =
                    if onMobile then
                        []

                    else
                        List.repeat offset (el [ width <| px <| 50 ] none)
            in
            game.wordChain
                |> List.indexedMap
                    (\wordId ( word, offset ) ->
                        row [ width fill, spacing 20, padding 5 ]
                            [ offsetElements offset
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
                    if isCharacterKnown game character then
                        WordChain.Known

                    else if isCharacterSimilar game character then
                        WordChain.Similar

                    else
                        WordChain.Unknown
            in
            Common.viewContainer onMobile
                True
                { popup = historyPopup game
                , topbar = viewTopBar
                , wordlist = wordList game wordStateFn
                , bottom =
                    [ Common.viewInput onMobile game { msgUserPressedEnter = UserPressedEnter, msgInputHanzi = InputHanzi }
                    , Common.viewWrongAnwers onMobile (wrongAnswersOf game)
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
                    WordChain.Show <| isCharacterKnown game character

                success =
                    case state of
                        First s ->
                            s

                        Other s ->
                            s
            in
            Common.viewContainer onMobile
                False
                { popup = historyPopup game
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
            column [ spacing 5 ]
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
    case state of
        First success ->
            column [ centerX, UI.floating, padding 10, spacing 10 ]
                [ text shareText
                , UI.niceButton "Copy" (OnShareClicked shareText) Nothing
                ]

        _ ->
            none


subscriptions =
    Sub.none
