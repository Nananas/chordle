module Daily exposing (..)

import Browser.Dom
import Clipboard
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
    }


type GameOverState
    = First DayProgress
    | Other DayProgress


type Attempt
    = Correct
    | Wrong
    | Almost


type alias EndScore =
    { mistakes : Int
    , attempts : List Attempt
    }


type DayProgress
    = Succeeded
    | Failed
    | GiveUp


type alias Progress =
    -- Key is Rata Die format
    Dict Int DayProgress



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
        }
    , Process.sleep 100
        |> Task.andThen (\_ -> Browser.Dom.focus idInput)
        |> Task.attempt (\_ -> NoOp)
    )


gameOver : DayProgress -> GameModel -> ( Model, Cmd Msg )
gameOver dayProgress game =
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
    in
    ( GameOver state (gameToEndScore game) { game | progress = newProgress }
    , updateProgressInStorage game.today dayProgress game.progress
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


update msg model =
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
                        (WordChain.multiChainGenerator 10 dictionary)
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
                |> processRoundFinished

        ( Playing game, OnGiveUpClicked ) ->
            gameOver GiveUp game

        ( Playing game, OnMouseEnterCharacter id ) ->
            ( Playing { game | showPopupForCharacter = Just id }, Cmd.none )

        ( Playing game, OnMouseLeaveCharacter ) ->
            ( Playing { game | showPopupForCharacter = Nothing }, Cmd.none )

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


processRoundFinished : GameModel -> ( Model, Cmd Msg )
processRoundFinished game =
    if List.length (allWrongAnswers game) > 5 then
        -- Too many wrong answers
        gameOver Failed game

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
            gameOver Succeeded game

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


line =
    el [ width fill, height <| px 1, Element.Background.color <| rgba 0.8 0.8 0.8 1 ] none


view : Device -> Model -> Element Msg
view device model =
    let
        popup : Bool -> Progress -> Element Msg
        popup show progress =
            if show then
                row [ width fill, height fill, Element.Background.color <| rgba 0 0 0 0.1, Element.Events.onClick OnClickedHideHistory ]
                    [ column [ width (px 300), height fill, alignRight, Element.Background.color UI.accentColorLight, padding 20, spacing 20 ]
                        [ el [ width fill ] <| UI.heading "Play history:"
                        , Dict.toList progress
                            |> List.map
                                (\( rata, dp ) ->
                                    [ el [ width fill, Element.Background.color <| dayProgressToColor dp, UI.rounded 20, height (px 40) ] <|
                                        row [ centerX, centerY, spacing 10 ]
                                            [ text <| Date.format "dd-MMM-yyyy" <| Date.fromRataDie rata
                                            , text <| dayProgressToEmoji dp
                                            ]
                                    ]
                                )
                            |> List.concat
                            |> column
                                [ width fill, height fill, spacing 10, padding 5, scrollbarY ]
                        ]
                    ]

            else
                none
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
            column [ width fill, height fill, inFront UI.viewFooter ]
                [ viewTopBar
                , row [ height fill, width fill, paddingXY 40 0, inFront <| popup game.showProgressPopup game.progress ]
                    [ game.wordChain
                        |> List.indexedMap
                            (\wordId ( word, offset ) ->
                                row [ width fill, spacing 20, padding 5 ]
                                    [ List.repeat offset (el [ width <| px <| 50 ] none)
                                        ++ (word.characters
                                                |> List.indexedMap
                                                    (\id character ->
                                                        viewSingleHanzi
                                                            wordStateFn
                                                            game.showPopupForCharacter
                                                            wordId
                                                            id
                                                            character
                                                    )
                                           )
                                        |> row [ spacing 10, width <| fillPortion 1 ]
                                    , viewWordEnglish word
                                    ]
                            )
                        |> List.intersperse line
                        |> column [ width fill, centerY, spacing 4 ]
                        |> el [ width <| fillPortion 1 ]
                    , column [ width <| fillPortion 1, spacing 20 ]
                        [ viewInput device game
                        , el [ centerX ] <| UI.niceButton "I give up, show me the answers" OnGiveUpClicked Nothing
                        , viewWrongAnwers <| wrongAnswersOf game
                        , viewIsGameAReplay game
                        ]
                    ]
                ]

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
            column [ width fill, height fill, inFront UI.viewFooter ]
                [ viewTopBar
                , row [ height fill, width fill, paddingXY 40 0, inFront <| popup game.showProgressPopup game.progress ]
                    [ game.wordChain
                        |> List.indexedMap
                            (\wordId ( word, offset ) ->
                                row [ width fill, spacing 20, padding 5 ]
                                    [ List.repeat offset (el [ width <| px <| 50 ] none)
                                        ++ (word.characters
                                                |> List.indexedMap
                                                    (\id character ->
                                                        viewSingleHanzi
                                                            wordStateFn
                                                            game.showPopupForCharacter
                                                            wordId
                                                            id
                                                            character
                                                    )
                                           )
                                        |> row [ spacing 10, width <| fillPortion 1 ]
                                    , viewWordEnglish word
                                    ]
                            )
                        |> List.intersperse line
                        |> column [ width fill, centerY, spacing 4 ]
                        |> el [ width <| fillPortion 1 ]
                    , column [ width <| fillPortion 1 ]
                        [ el [ centerX, centerY ] <|
                            column [ width (px 200), spacing 20 ]
                                [ UI.heading <|
                                    if success == Succeeded then
                                        "Well done!"

                                    else
                                        "Better next time!"
                                , row [ width fill ] [ el [ alignLeft ] <| UI.niceText <| "Attemps:", el [ alignRight ] <| text <| String.fromInt <| List.length endScore.attempts ]
                                , row [ width fill ] [ el [ alignLeft ] <| UI.niceText <| "Mistakes:", el [ alignRight ] <| text <| String.fromInt endScore.mistakes ]
                                , viewGameOverShare endScore game.today state
                                ]
                        ]
                    ]
                ]

        _ ->
            UI.spinner


viewTopBar =
    row [ height <| px 50, width fill, Element.Background.color UI.accentColor, paddingXY 20 0, spacing 20, behindContent UI.viewLogo ]
        [ UI.niceIconButton (Icons.arrowBack 20) OnClickedHome
        , el [ alignRight ] <| UI.niceIconButton (Icons.academicCap 20) OnClickedToggleShowHistory
        ]


viewWordEnglish word =
    word.english
        |> String.split "|"
        |> List.map (\txt -> paragraph [ Element.Font.size 16 ] [ text txt ])
        |> column [ spacing 5, width <| fillPortion 1, alignLeft ]


viewSingleHanzi wordStateFn showPopupForCharacter wordId id character =
    WordChain.viewSingleHanzi
        { state = wordStateFn character
        , showPopup = showPopupForCharacter
        , onMouseEnterCharacterMsg = OnMouseEnterCharacter
        , onMouseLeaveCharacterMsg = OnMouseLeaveCharacter
        , wordId = wordId
        , id = id
        , character = character
        }


viewInput : Device -> GameModel -> Element Msg
viewInput device game =
    el [ width fill, padding 10 ] <|
        row
            [ width fill
            , Element.Border.widthEach
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
                    Element.Input.text
                        [ Utils.onEnter UserPressedEnter
                        , htmlAttribute <| Html.Attributes.id idInput
                        , Element.Border.width 0
                        ]
                        { onChange = InputHanzi
                        , text = game.currentInput
                        , placeholder =
                            Just <|
                                Element.Input.placeholder [ Element.Font.size 14 ] <|
                                    text <|
                                        "Write pinyin with tones here (e.g. hao3 for 好), then press 'OK'"
                                            ++ (if Utils.isOnDesktop device then
                                                    " or the Enter key"

                                                else
                                                    ""
                                               )
                        , label = Element.Input.labelHidden ""
                        }
            , Element.Input.button
                [ mouseOver [ Element.Font.color UI.accentColorHighlight ]
                , Element.Font.color UI.accentColor
                , Element.Background.color UI.white
                , width shrink
                , height <| px 40
                , width <| px 40
                , Element.Border.rounded 10
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
    (List.map (\w -> Just w) wrongAnswers ++ List.repeat (6 - List.length wrongAnswers) Nothing)
        |> List.map
            (\mPart ->
                el
                    [ UI.floating
                    , UI.rounded 5
                    , height <| px 50
                    , width <| minimum 50 shrink
                    , paddingXY 10 0
                    , Element.Background.color <|
                        case mPart of
                            Just _ ->
                                UI.errorColorLight

                            _ ->
                                rgb255 226 226 226
                    , Element.Border.color <|
                        case mPart of
                            Just _ ->
                                UI.errorColor

                            _ ->
                                rgb255 226 226 226
                    , Element.Border.width 2
                    ]
                <|
                    el [ centerX, centerY, Element.Font.size 20 ] <|
                        case mPart of
                            Just part ->
                                text (formatPinyin part)

                            Nothing ->
                                text "     "
            )
        |> row [ spacing 10, height <| px 50, centerX ]


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
    Storage.storageLoaded OnStorageLoaded
