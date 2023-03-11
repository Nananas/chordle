module Numbers exposing (..)

import Backend
import Element exposing (..)
import Element.Background as Background
import Element.Font
import EverySet exposing (EverySet)
import Icons
import List.Extra as List
import MobileUI
import Random
import Random.List
import Tones exposing (Tone(..))
import UI
import Utils exposing (..)
import WordChain
import Words exposing (PinyinPart)



-- TODO:
--   * Fractional values: 2/3:
--     三分之二
--     sān fēn zhī èr
--   * interior zeros: 10500:
--     一万〇五百


type Model
    = Generating GameModes NumberTypes
    | Ready GameModel


type Msg
    = OnClickedHome
    | NewGeneration ( GameMode, Int )
    | NextRound
    | UserPressedEnter
    | OnMouseLeaveCharacter
    | OnMouseEnterCharacter ( Int, Int )
    | OnToggleHelp
    | BackendMsg Backend.Msg
    | OnClickedToggleGameMode GameMode


type alias GameModel =
    { number : Int
    , hanzi : List String
    , errorMsg : Maybe String
    , roundEnd : Bool
    , showPopupForCharacter : Maybe ( Int, Int )
    , showHelp : Bool
    , gameModesEnabled : GameModes
    , numberTypes : List NumberType
    , roundGameMode : GameMode
    }


type GameMode
    = NumCh
    | ChNum
    | EnCh
    | ChEn


type alias GameModes =
    EverySet GameMode


type NumberType
    = FullDecimal


type alias NumberTypes =
    List NumberType


allGameModes =
    EverySet.fromList [ NumCh, ChNum, EnCh, ChEn ]


init : ( Model, Cmd Msg )
init =
    ( Generating allGameModes [ FullDecimal ], cmdRandomNumberAndMode allGameModes )


initReady : Int -> GameModes -> List NumberType -> GameMode -> Model
initReady number gameModes numberModes roundMode =
    Ready
        { number = number
        , hanzi = numberToParts hanziLanguage number
        , errorMsg = Nothing
        , roundEnd = False
        , showPopupForCharacter = Nothing
        , showHelp = False
        , gameModesEnabled = gameModes
        , numberTypes = numberModes
        , roundGameMode = roundMode
        }


fullDecimalGenerator =
    let
        firstGen =
            Random.int 1 9

        secondGen =
            Random.int 0 9

        zerosGen =
            Random.int 1 9
    in
    Random.map3
        (\first second zeros ->
            (first * 10 + second) * 10 ^ zeros
        )
        firstGen
        secondGen
        zerosGen


gameModeGenerator : GameModes -> Random.Generator GameMode
gameModeGenerator modes =
    modes
        |> EverySet.toList
        |> Random.List.choose
        |> Random.map (\( me, _ ) -> me |> Maybe.withDefault NumCh)


cmdRandomNumberAndMode : GameModes -> Cmd Msg
cmdRandomNumberAndMode modes =
    Random.generate NewGeneration (Random.pair (gameModeGenerator modes) fullDecimalGenerator)


update : Backend.Uuid -> Msg -> Model -> ( Model, Cmd Msg )
update uuid msg model =
    case ( model, msg ) of
        ( Generating gmodes nmodes, NewGeneration ( roundMode, number ) ) ->
            ( initReady number gmodes nmodes roundMode, Cmd.none )

        ( Generating _ _, _ ) ->
            ( model, Cmd.none )

        ( Ready game, NextRound ) ->
            ( model, cmdRandomNumberAndMode game.gameModesEnabled )

        ( Ready _, OnClickedHome ) ->
            ( model, Cmd.none )

        ( Ready game, OnToggleHelp ) ->
            ( Ready { game | showHelp = not game.showHelp }, Cmd.none )

        ( Ready game, NewGeneration ( roundMode, number ) ) ->
            ( Ready { game | number = number, hanzi = numberToParts hanziLanguage number, roundGameMode = roundMode }, Cmd.none )

        ( Ready game, OnClickedToggleGameMode gameMode ) ->
            let
                newModes =
                    if EverySet.member gameMode game.gameModesEnabled then
                        if EverySet.size game.gameModesEnabled > 1 then
                            EverySet.remove gameMode game.gameModesEnabled

                        else
                            allGameModes

                    else
                        EverySet.insert gameMode game.gameModesEnabled
            in
            ( Generating newModes game.numberTypes
            , cmdRandomNumberAndMode newModes
            )

        ( Ready game, UserPressedEnter ) ->
            if game.roundEnd then
                ( Generating game.gameModesEnabled game.numberTypes
                , Cmd.batch
                    [ Backend.postNumbersRoundEnd uuid game.number (gameModeAsShortString game.roundGameMode)
                        |> Cmd.map BackendMsg
                    , cmdRandomNumberAndMode game.gameModesEnabled
                    ]
                )

            else
                ( Ready { game | roundEnd = True }, Cmd.none )

        ( Ready game, OnMouseLeaveCharacter ) ->
            ( Ready { game | showPopupForCharacter = Nothing }, Cmd.none )

        ( Ready game, OnMouseEnterCharacter ( wordId, id ) ) ->
            ( Ready { game | showPopupForCharacter = Just ( wordId, id ) }, Cmd.none )

        ( _, BackendMsg _ ) ->
            ( model, Cmd.none )


view : Device -> Model -> Element Msg
view device model =
    let
        onMobile =
            isOnMobile device
    in
    column [ width fill, height fill ]
        [ viewTopBar onMobile model
        , case model of
            Generating _ _ ->
                UI.spinner

            Ready game ->
                viewGame onMobile game
        ]


viewTopBar : Bool -> Model -> Element Msg
viewTopBar onMobile model =
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

        --, el [ alignLeft ] <| buttonFn "Restart Game" OnRestartClick (Icons.refresh iconSize)
        , el [ alignRight ] <| buttonFn "Help" OnToggleHelp (Icons.questionmark iconSize)

        --, buttonFn "Help" OnToggleHelp (Icons.questionmark iconSize)
        ]


shownQuestionNumber game =
    case game.roundGameMode of
        NumCh ->
            formattedNumberRaw game.number

        EnCh ->
            formattedNumberEnglish game.number

        ChNum ->
            String.join "" game.hanzi

        ChEn ->
            String.join "" game.hanzi


showAnswerNumber game =
    case game.roundGameMode of
        NumCh ->
            String.join "" game.hanzi

        EnCh ->
            String.join "" game.hanzi

        ChNum ->
            formattedNumberRaw game.number

        ChEn ->
            formattedNumberEnglish game.number


viewGame : Bool -> GameModel -> Element Msg
viewGame onMobile game =
    let
        modal =
            if game.showHelp then
                let
                    modalFn =
                        if onMobile then
                            MobileUI.modal

                        else
                            UI.modal
                in
                modalFn OnToggleHelp (viewHelp onMobile game.showPopupForCharacter)

            else
                none
    in
    column [ width fill, height fill, inFront modal, padding 20 ]
        [ viewGameModes game
        , column [ centerX, centerY, spacing 50 ]
            [ UI.niceTextWith [ centerX ] <|
                case game.roundGameMode of
                    EnCh ->
                        "How to write this number using 中文?"

                    NumCh ->
                        "How to write this number using 中文?"

                    ChEn ->
                        "How to write this number in English?"

                    ChNum ->
                        "How to write this number using Arabic numerals?"
            , UI.headingWith [ Element.Font.size 32 ] <|
                shownQuestionNumber game
            , if game.roundEnd then
                case game.roundGameMode of
                    EnCh ->
                        viewHanzi onMobile game.hanzi game.showPopupForCharacter 0

                    NumCh ->
                        viewHanzi onMobile game.hanzi game.showPopupForCharacter 0

                    ChNum ->
                        UI.headingWith [ Element.Font.size 32 ] <| formattedNumberRaw game.number

                    ChEn ->
                        UI.headingWith [ Element.Font.size 32 ] <| formattedNumberEnglish game.number

              else
                UI.niceTextWith
                    [ centerX
                    , Element.Font.color UI.lightGray
                    , Element.Font.size 24
                    , height
                        (px <|
                            if onMobile then
                                40

                            else
                                50
                        )
                    ]
                    "..."
            , UI.niceButtonWith [ centerX ]
                (if game.roundEnd then
                    "Next"

                 else
                    "Show"
                )
                UserPressedEnter
                Nothing
            ]
        ]


gameModeAsString mode =
    case mode of
        EnCh ->
            "En → 中文"

        NumCh ->
            "123 → 中文"

        ChNum ->
            "中文 → 123"

        ChEn ->
            "中文 → En"


gameModeAsShortString mode =
    case mode of
        EnCh ->
            "en-ch"

        NumCh ->
            "num-ch"

        ChNum ->
            "ch-num"

        ChEn ->
            "ch-en"


viewGameModes : GameModel -> Element Msg
viewGameModes game =
    row [ centerX, spacing 20 ]
        [ UI.heading "Game modes:"
        , [ EnCh, NumCh, ChNum, ChEn ]
            |> List.map (\gm -> UI.niceToggleButton (gameModeAsString gm) (OnClickedToggleGameMode gm) Nothing (EverySet.member gm game.gameModesEnabled))
            |> row [ spacing 20 ]
        ]


{-| wordId = 0 for game, wordId > 0 for help
-}
viewHanzi onMobile hanzi showPopupForCharacter wordId =
    hanzi
        |> List.map (\str -> { hanzi = str, pinyinPart = hanziToPinyinPart str })
        |> List.indexedMap
            (\id char ->
                WordChain.viewSingleHanzi onMobile
                    { state = WordChain.Show True
                    , showPopup = showPopupForCharacter
                    , onMouseEnterCharacterMsg = OnMouseEnterCharacter
                    , onMouseLeaveCharacterMsg = OnMouseLeaveCharacter
                    , wordId = wordId
                    , id = id
                    , character = char
                    }
            )
        |> row [ centerX, Element.Font.size 24, spacing 5 ]


viewHelp onMobile showPopupForCharacter =
    let
        wrappedText txt =
            paragraph [] [ text txt ]

        reviewRow h e i =
            row [ spacing 20 ]
                [ viewHanzi onMobile [ h ] showPopupForCharacter (i + 2)
                , text e
                ]
    in
    [ el [] <| UI.heading "The Number game"
    , wrappedText "In this game the goal is to guess the correct translation of the number shown. "
    , wrappedText "\nFor example:"
    , el [ centerX ] <| UI.heading "89 000 000"
    , wrappedText "... will translate to:"
    , viewHanzi onMobile [ "八", "千", "九", "百", "万" ] showPopupForCharacter 1
    , wrappedText "Keep in mind that there are always multiple ways of translating the same number."
    , el [] <| UI.heading "Review"
    , wrappedText "Here is a review of the numbers:"
    , row [ centerX, spacing 50 ]
        [ column [ spacing 4 ]
            [ reviewRow "〇" "0" 0
            , reviewRow "五" "5" 5
            , reviewRow "十" "10" 10
            ]
        , column [ spacing 4 ]
            [ reviewRow "一" "1" 1
            , reviewRow "六" "6" 6
            , reviewRow "百" "100" 100
            ]
        , column [ spacing 4 ]
            [ reviewRow "兩" "2" 2
            , reviewRow "七" "7" 7
            , reviewRow "千" "1000" 1000
            ]
        , column [ spacing 4 ]
            [ reviewRow "三" "3" 3
            , reviewRow "八" "8" 8
            , reviewRow "万" "10 000" 10000
            ]
        , column [ spacing 4 ]
            [ reviewRow "四" "4" 4
            , reviewRow "九" "9" 9
            , reviewRow "亿" "100 000 000" 100000000
            ]
        ]
    ]


formattedNumberRaw number =
    let
        format acc ns =
            case List.take 3 ns of
                [] ->
                    acc

                n ->
                    let
                        nextAcc =
                            acc ++ [ String.join "" <| List.reverse n ]
                    in
                    format nextAcc (List.drop 3 ns)
    in
    String.fromInt number
        |> String.split ""
        |> List.reverse
        |> format []
        |> List.reverse
        |> String.join " "


formattedNumberEnglish : Int -> String
formattedNumberEnglish number =
    numberToParts englishLanguage number
        |> String.join " "
        |> Debug.log "???"


type alias LanguageParts =
    { parts : List ( Int, String )
    , bindChar : String
    }


numberToParts : LanguageParts -> Int -> List String
numberToParts language number =
    let
        _ =
            Debug.log " NUMBER " number
    in
    if number == 0 then
        []

    else if number < 10 then
        language.parts
            |> List.foldl
                (\( n, char ) acc ->
                    if n == number then
                        char

                    else
                        acc
                )
                "?"
            |> List.singleton

    else
        let
            ( rest, str ) =
                language.parts
                    |> List.foldl
                        (\( min, char ) ( n, acc ) ->
                            if n == min then
                                let
                                    _ =
                                        Debug.log "HERE" min
                                in
                                if min >= 10 then
                                    ( n - min, acc ++ [ language.bindChar, char ] )

                                else
                                    ( n - min, acc ++ [ char ] )

                            else if n >= min then
                                let
                                    factor =
                                        n
                                            // min
                                            |> Debug.log "factor"

                                    remaining =
                                        n - factor * min

                                    _ =
                                        Debug.log "??" ( acc, n, min )

                                    factorChar =
                                        if n < 100 && factor == 1 && List.length acc == 0 then
                                            [] |> Debug.log "skip"

                                        else
                                            numberToParts language factor
                                in
                                ( remaining, acc ++ factorChar ++ [ char ] ) |> Debug.log "step"

                            else
                                ( n, acc )
                        )
                        ( number, [] )
        in
        str ++ numberToParts language rest


hanziLanguage =
    { parts =
        [ ( 100000000, "亿" )
        , ( 10000, "万" )
        , ( 1000, "千" )
        , ( 100, "百" )
        , ( 10, "十" )
        , ( 9, "九" )
        , ( 8, "八" )
        , ( 7, "七" )
        , ( 6, "六" )
        , ( 5, "五" )
        , ( 4, "四" )
        , ( 3, "三" )
        , ( 2, "兩" )
        , ( 1, "一" )
        ]
    , bindChar = "一"
    }


englishLanguage =
    { parts =
        [ ( 1000000000, "billion" )
        , ( 1000000, "million" )
        , ( 1000, "thousand" )
        , ( 100, "hundred" )
        , ( 90, "ninety" )
        , ( 80, "eighty" )
        , ( 70, "seventy" )
        , ( 60, "sixty" )
        , ( 50, "fifty" )
        , ( 40, "fourty" )
        , ( 30, "thirty" )
        , ( 20, "twenty" )
        , ( 10, "ten" )
        , ( 9, "nine" )
        , ( 8, "eight" )
        , ( 7, "seven" )
        , ( 6, "six" )
        , ( 5, "five" )
        , ( 4, "four" )
        , ( 3, "three" )
        , ( 2, "two" )
        , ( 1, "one" )
        ]
    , bindChar = "and"
    }


numeralFromList list number =
    List.getAt number list
        |> Maybe.withDefault "?"


hanziToPinyinPart hanzi =
    case hanzi of
        "〇" ->
            PinyinPart "ling" Second

        "一" ->
            PinyinPart "yi" First

        "兩" ->
            PinyinPart "liang" Third

        "三" ->
            PinyinPart "san" First

        "四" ->
            PinyinPart "si" Forth

        "五" ->
            PinyinPart "wu" Third

        "六" ->
            PinyinPart "liu" Forth

        "七" ->
            PinyinPart "qi" First

        "八" ->
            PinyinPart "ba" First

        "九" ->
            PinyinPart "jiu" Third

        "十" ->
            PinyinPart "shi" Second

        "百" ->
            PinyinPart "bai" Third

        "千" ->
            PinyinPart "qian" First

        "万" ->
            PinyinPart "wan" Forth

        "亿" ->
            PinyinPart "yi" Forth

        _ ->
            PinyinPart "?" Fifth
