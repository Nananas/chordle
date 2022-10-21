module Daily exposing (..)

import Browser.Dom
import Dict
import Element exposing (..)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html.Attributes
import Icons
import List.Extra as List
import Process
import Random
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
    = LoadedDate Int
    | NewWordChain WordChain
    | NoOp
    | NoOpIntInt ( Int, Int )
    | UserPressedEnter
    | InputHanzi String
    | OnMouseEnterCharacter ( Int, Int )
    | OnMouseLeaveCharacter
    | OnGiveUpClicked
    | OnClickedHome


type Model
    = Loading
    | Playing GameModel
    | GameOver Bool EndScore GameModel


type alias GameModel =
    { wordChain : List ( Word, Int )
    , currentInput : String
    , answers : List PinyinPart
    , showPopupForCharacter : Maybe ( Int, Int )
    , attempts : List Attempt
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
    ( Loading
    , Task.map2 dateTimeToUniqueSeed Time.here Time.now
        |> Task.perform LoadedDate
    )


initGame wordChain =
    ( Playing
        { wordChain = wordChain
        , currentInput = ""
        , answers = []
        , showPopupForCharacter = Nothing
        , attempts = []
        }
    , Process.sleep 100
        |> Task.andThen (\_ -> Browser.Dom.focus idInput)
        |> Task.attempt (\_ -> NoOp)
    )


monthToInt month =
    case month of
        Time.Jan ->
            0

        Time.Feb ->
            1

        Time.Mar ->
            2

        Time.Apr ->
            3

        Time.May ->
            4

        Time.Jun ->
            5

        Time.Jul ->
            6

        Time.Aug ->
            7

        Time.Sep ->
            8

        Time.Oct ->
            9

        Time.Nov ->
            10

        Time.Dec ->
            11


dateTimeToUniqueSeed zone time =
    --Debug.log "DEV SEED" 2
    Time.posixToParts zone time
        |> (\parts -> parts.year * 10000 + monthToInt parts.month * 100 + parts.day)


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
        ( Loading, LoadedDate uniqueInt ) ->
            let
                seed =
                    Random.initialSeed uniqueInt

                dictionary =
                    allWords Dict.empty

                ( wordChain, _ ) =
                    Random.step
                        (WordChain.multiChainGenerator 10 dictionary)
                        seed
            in
            initGame
                (wordChain
                    |> offsetWords
                    |> sortWords
                )

        ( Loading, NewWordChain wordChain ) ->
            ( model, Cmd.none )

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
            ( GameOver False (gameToEndScore game) game, Cmd.none )

        ( Playing game, OnMouseEnterCharacter id ) ->
            ( Playing { game | showPopupForCharacter = Just id }, Cmd.none )

        ( Playing game, OnMouseLeaveCharacter ) ->
            ( Playing { game | showPopupForCharacter = Nothing }, Cmd.none )

        ( GameOver success endScore game, OnMouseEnterCharacter id ) ->
            ( GameOver success endScore { game | showPopupForCharacter = Just id }, Cmd.none )

        ( GameOver success endScore game, OnMouseLeaveCharacter ) ->
            ( GameOver success endScore { game | showPopupForCharacter = Nothing }, Cmd.none )

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
        ( GameOver False (gameToEndScore game) game
          -- TODO: score
        , Cmd.none
        )

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
            ( GameOver True (gameToEndScore game) game
              -- TODO: score
            , Cmd.none
            )

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
                , row [ height fill, width fill, paddingXY 40 0 ]
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
                        ]
                    ]
                ]

        GameOver success endScore game ->
            let
                wordStateFn character =
                    WordChain.Show <| isCharacterKnown game character
            in
            column [ width fill, height fill, inFront UI.viewFooter ]
                [ viewTopBar
                , row [ height fill, width fill, paddingXY 40 0 ]
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
                                    if success then
                                        "Well done!"

                                    else
                                        "Better next time!"
                                , row [ width fill ] [ el [ alignLeft ] <| UI.niceText <| "Attemps:", el [ alignRight ] <| text <| String.fromInt <| List.length endScore.attempts ]
                                , row [ width fill ] [ el [ alignLeft ] <| UI.niceText <| "Mistakes:", el [ alignRight ] <| text <| String.fromInt endScore.mistakes ]
                                , if success then
                                    viewGameOverAsSquares endScore

                                  else
                                    none
                                ]
                        ]
                    ]
                ]

        Loading ->
            UI.spinner


viewTopBar =
    row [ height <| px 50, width fill, Element.Background.color UI.accentColor, paddingXY 20 0, spacing 20, behindContent UI.viewLogo ]
        [ UI.niceIconButton (Icons.arrowBack 20) OnClickedHome ]


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
    wrongAnswers
        |> List.map
            (\part ->
                el
                    [ UI.floating
                    , UI.rounded 5
                    , height <| px 50
                    , paddingXY 10 0
                    , Element.Background.color <|
                        rgb255 226 226 226
                    ]
                <|
                    el [ centerX, centerY, Element.Font.size 20 ] <|
                        text (formatPinyin part)
            )
        |> row [ spacing 10, height <| px 50, centerX ]


viewGameOverAsSquares endScore =
    let
        square attempt =
            el [ padding 1 ] <|
                text <|
                    case attempt of
                        Correct ->
                            "🟩"

                        Wrong ->
                            "🟥"

                        Almost ->
                            "🟨"

        --squares =
        --    endScore.attempts
        --        |> List.map square
        --recurse acc sqs =
        --    case List.splitAt 8 sqs of
        --        ( [], _ ) ->
        --            List.reverse acc
        --        ( s, more ) ->
        --            recurse (row [ spacing 2 ] s :: acc) more
    in
    --endScore.attempts
    --    |> List.map square
    --    |> recurse []
    --    |> column [ spacing 2, centerX, centerY ]
    endScore.attempts
        |> List.map square
        |> paragraph []
