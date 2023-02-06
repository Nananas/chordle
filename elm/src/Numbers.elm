module Numbers exposing (..)

import Backend
import Common
import Element exposing (..)
import Element.Background as Background
import Element.Font
import Icons
import List.Extra as List
import MobileUI
import Random
import Tones exposing (Tone(..))
import UI
import Utils exposing (..)
import WordChain
import Words exposing (PinyinPart)


type Model
    = Initializing
    | Ready GameModel


type Msg
    = OnClickedHome
    | NewNumber Int
    | NextRound
    | UserPressedEnter
    | OnMouseLeaveCharacter
    | OnMouseEnterCharacter ( Int, Int )
    | OnToggleHelp
    | BackendMsg Backend.Msg


type alias GameModel =
    { number : Int
    , hanzi : List String
    , errorMsg : Maybe String
    , roundEnd : Bool
    , showPopupForCharacter : Maybe ( Int, Int )
    , showHelp : Bool
    }


init =
    ( Initializing, cmdRandomNumber )


initReady number =
    Ready
        { number = number
        , hanzi = numberToHanzi number
        , errorMsg = Nothing
        , roundEnd = False
        , showPopupForCharacter = Nothing
        , showHelp = False
        }


cmdRandomNumber =
    let
        firstGen =
            Random.int 0 9

        secondGen =
            Random.int 0 9

        zerosGen =
            Random.int 0 9

        numberGen =
            Random.map3
                (\first second zeros ->
                    (first * 10 + second) * 10 ^ zeros
                )
                firstGen
                secondGen
                zerosGen
    in
    Random.generate NewNumber numberGen


update uuid msg model =
    case ( model, msg ) of
        ( Initializing, NewNumber number ) ->
            ( initReady number, Cmd.none )

        ( Initializing, _ ) ->
            ( model, Cmd.none )

        ( Ready game, NextRound ) ->
            ( model, cmdRandomNumber )

        ( Ready game, OnClickedHome ) ->
            ( model, Cmd.none )

        ( Ready game, OnToggleHelp ) ->
            ( Ready { game | showHelp = not game.showHelp }, Cmd.none )

        ( Ready game, NewNumber number ) ->
            ( Ready { game | number = number, hanzi = numberToHanzi number }, Cmd.none )

        ( Ready game, UserPressedEnter ) ->
            if game.roundEnd then
                ( Initializing
                , Cmd.batch
                    [ Backend.postNumbersRoundEnd uuid game.number
                        |> Cmd.map BackendMsg
                    , cmdRandomNumber
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
            Initializing ->
                UI.spinner

            Ready game ->
                viewGame onMobile game
        ]


viewTopBar : Bool -> Model -> Element Msg
viewTopBar onMobile model =
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

        --, el [ alignLeft ] <| buttonFn "Restart Game" OnRestartClick (Icons.refresh iconSize)
        , el [ alignRight ] <| buttonFn "Help" OnToggleHelp (Icons.questionmark iconSize)

        --, buttonFn "Help" OnToggleHelp (Icons.questionmark iconSize)
        ]


viewGame : Bool -> GameModel -> Element Msg
viewGame onMobile game =
    let
        modalFn =
            if onMobile then
                MobileUI.modal

            else
                UI.modal

        modal =
            if game.showHelp then
                modalFn OnToggleHelp (viewHelp onMobile game.showPopupForCharacter)

            else
                none
    in
    el [ width fill, height fill, inFront modal ] <|
        column [ centerX, centerY, spacing 50 ]
            [ UI.niceTextWith [ centerX ] "How to write this number in 中文?"
            , UI.headingWith [ Element.Font.size 32 ] <| formattedNumber game.number
            , if game.roundEnd then
                viewHanzi onMobile game.hanzi game.showPopupForCharacter 0

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
            [ reviewRow "二" "2" 2
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


formattedNumber number =
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


characters =
    [ ( 100000000, "亿" )
    , ( 10000, "万" )
    , ( 1000, "千" )
    , ( 100, "百" )
    , ( 10, "十" )
    ]


naturalCharacters =
    [ "〇", "一", "二", "三", "四", "五", "六", "七", "八", "九" ]


hanziToPinyinPart hanzi =
    case hanzi of
        "〇" ->
            PinyinPart "ling" Second

        "一" ->
            PinyinPart "yi" First

        "二" ->
            PinyinPart "er" Forth

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


numberToHanzi : Int -> List String
numberToHanzi number =
    if number == 0 then
        []

    else if number < 10 then
        List.getAt number naturalCharacters
            |> Maybe.withDefault "?"
            |> List.singleton

    else
        let
            ( rest, str ) =
                characters
                    |> List.foldl
                        (\( min, char ) ( n, acc ) ->
                            if n >= min then
                                let
                                    factor =
                                        n // min

                                    remaining =
                                        n - factor * min

                                    factorChar =
                                        numberToHanzi factor
                                in
                                ( remaining, acc ++ factorChar ++ [ char ] )

                            else
                                ( n, acc )
                        )
                        ( number, [] )
        in
        str ++ numberToHanzi rest
