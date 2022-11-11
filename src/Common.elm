module Common exposing (..)

import Element exposing (..)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html.Attributes
import MobileUI
import UI
import Words exposing (PinyinPart, formatPinyin)


idInput =
    "text-input"


viewInput : Bool -> { r | currentInput : String } -> { msgUserPressedEnter : msg, msgInputHanzi : String -> msg } -> Element msg
viewInput onMobile { currentInput } { msgUserPressedEnter, msgInputHanzi } =
    el [ width fill, padding 10, height (px 50) ] <|
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
            [ if onMobile then
                if currentInput == "" then
                    el [ Element.Font.size 14, Element.Font.color UI.gray, width fill, height (px 40) ] <|
                        paragraph [ width fill, centerY ]
                            [ text
                                "Use the keyboard to write pinyin with tones (e.g. hao3 for 好), then press 'OK'"
                            ]

                else
                    text <| currentInput

              else
                Element.Input.text
                    [ UI.onEnter msgUserPressedEnter
                    , htmlAttribute <| Html.Attributes.id idInput
                    , Element.Border.width 0
                    ]
                    { onChange = msgInputHanzi
                    , text = currentInput
                    , placeholder =
                        Just <|
                            Element.Input.placeholder [ Element.Font.size 14 ] <|
                                text <|
                                    "Write pinyin with tones here (e.g. hao3 for 好), then press 'OK'"
                                        ++ (if not onMobile then
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
                { onPress = Just msgUserPressedEnter
                , label =
                    el [ centerY, centerX, paddingXY 20 0, spacing 20 ] <|
                        text "Ok"
                }
            ]


viewWrongAnwers : Bool -> List PinyinPart -> Element msg
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
                    , width shrink

                    --, padding 2
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
                    el [ width <| minimum size shrink, Element.Font.center, centerY, Element.Font.size fontsize ] <|
                        case mPart of
                            Just part ->
                                text (formatPinyin part)

                            Nothing ->
                                text "     "
            )
        |> row [ centerX, height (px size), centerY, spacing 5 ]
        |> el [ width fill ]


line =
    el [ width fill, height <| px 1, Element.Background.color <| rgba 0.8 0.8 0.8 1 ] none


viewContainer onMobile showKeyboardOnMobile { popup, topbar, wordlist, bottom, msgKeyboardInput, msgKeyboardBackspace, msgKeyboardClear } =
    let
        contentSpacing =
            if onMobile then
                10

            else
                20
    in
    column [ width fill, height fill, inFront popup ]
        [ topbar
        , el [ scrollbarY, width fill, height fill, paddingXY 10 5 ] <|
            el [ height shrink, width <| maximum 800 fill, centerX, centerY ] <|
                (wordlist
                    |> List.intersperse line
                    |> column
                        [ width fill
                        , centerY
                        , spacing 4
                        , Element.Border.widthEach { top = 2, bottom = 2, left = 0, right = 0 }
                        , Element.Border.color UI.accentColor
                        ]
                    |> el [ width <| fillPortion 1 ]
                )
        , el [ height shrink, width fill, alignBottom, Element.Border.shadow { offset = ( 0, -8 ), size = 0, blur = 8, color = rgba 0 0 0 0.2 } ] <|
            el [ paddingXY 10 0, width fill, height fill ] <|
                column [ width fill, height fill ]
                    [ column [ width <| maximum 800 fill, spacing contentSpacing, centerX ]
                        bottom
                    , if onMobile && showKeyboardOnMobile then
                        el [ alignBottom, width fill, height <| maximum 300 fill ] <| MobileUI.viewKeyboard msgKeyboardInput msgKeyboardBackspace msgKeyboardClear

                      else
                        none
                    ]
        ]


viewWordEnglish onMobile word =
    word.english
        |> String.split "|"
        |> List.map
            (\txt ->
                paragraph
                    [ Element.Font.size
                        (if onMobile then
                            16

                         else
                            20
                        )
                    ]
                    [ text txt ]
            )
        |> column [ spacing 5, alignLeft ]
