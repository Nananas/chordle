module Help exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import MobileUI
import Tones exposing (..)
import UI
import Words exposing (allWords)


viewDesktop : Bool -> msg -> (String -> msg) -> Element msg
viewDesktop showHelp toggleMsg noOpMsg =
    if showHelp then
        UI.modal toggleMsg
            [ el [] <| UI.heading "How to play?"
            , column [ scrollbarY, height fill, spacing 10 ]
                [ column [ spacing 3, width fill ]
                    [ text "1. Write pinyin and tones in the input box"
                    , text "    on the bottom, like this:"
                    , el [ width fill, paddingXY 40 0 ] <|
                        row
                            [ width fill
                            , Border.widthEach
                                { top = 0
                                , left = 0
                                , right = 0
                                , bottom = 2
                                }
                            , spacing 20
                            , Background.color UI.white
                            , centerX
                            , inFront <| el [ Background.color <| rgba255 0 0 0 0, width fill, height fill ] none
                            ]
                            [ Input.text
                                [ Border.width 0
                                , inFront <| row [ centerY, padding 10, Font.color UI.accentColor ] [ el [ width <| px 50 ] none, UI.arrowLeft 24 ]
                                ]
                                { onChange = noOpMsg
                                , text = "xie3"
                                , placeholder = Nothing
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
                                ]
                                { onPress = Nothing
                                , label =
                                    el [ centerY, centerX, paddingXY 20 0, spacing 20 ] <|
                                        text "Ok"
                                }
                            ]
                    ]
                , column [ spacing 3, width fill ]
                    [ text "2. Either press the 'Ok' button or press Enter"
                    , text "    on the keyboard"
                    , el [ width fill, paddingXY 40 0 ] <|
                        row
                            [ width fill
                            , Border.widthEach
                                { top = 0
                                , left = 0
                                , right = 0
                                , bottom = 2
                                }
                            , spacing 20
                            , Background.color UI.white
                            , centerX
                            , Font.color UI.gray
                            , inFront <| el [ Background.color <| rgba255 0 0 0 0, width fill, height fill ] none
                            ]
                            [ Input.text [ Border.width 0 ]
                                { onChange = noOpMsg
                                , text = "xie3"
                                , placeholder = Nothing
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
                                , inFront <| row [ centerY, padding 10, Font.color UI.accentColor ] [ el [ width <| px 20 ] none, column [] [ el [ height <| px 10 ] none, UI.cursorClick 24 ] ]
                                ]
                                { onPress = Nothing
                                , label =
                                    el [ centerY, centerX, paddingXY 20 0, spacing 20 ] <|
                                        text "Ok"
                                }
                            ]
                    ]
                , column [ spacing 3, width fill ]
                    [ text "3. If the pinyin and tone is correct of one or more "
                    , text "    of the characters, the hanzi will show up in green. "
                    , text "    If only the pinyin was correct, the pinyin will show up"
                    , text "    in yellow. Try again with the correct tone"
                    , text "    All the wrong attempt will be shown in gray below the board"
                    , row [ spacing 20, centerX, padding 10 ]
                        [ viewExampleHanziCorrect
                        , viewExampleHanziSimilar
                        , viewExampleHanziIncorrect
                        ]
                    ]
                , column [ spacing 3, width fill ]
                    [ text "4. When all characters are found, or when more than 5 wrong attempts "
                    , text "    were made, the round is over."
                    , text "    All characters are then shown, color coded by their tone."
                    , row [ spacing 20, centerX, padding 10 ]
                        [ viewExampleHanziFinishedRound "听" First False
                        , viewExampleHanziFinishedRound "人" Second False
                        , viewExampleHanziFinishedRound "写" Third True
                        , viewExampleHanziFinishedRound "四" Forth False
                        , viewExampleHanziFinishedRound "吧" Fifth False
                        ]
                    ]
                , column [ spacing 3 ]
                    [ text "5. Now press 'Next' to go to the next round of words"
                    ]
                , column [ paddingXY 0 10 ]
                    [ text <| "The dictionary currently contains a total of " ++ (String.fromInt <| List.length allWords) ++ " words!" ]

                --, paragraph [ paddingXY 0 10 ]
                --[ text "To enable notifications every 30 minutes, press the \"Enable Notifications\" button." ]
                ]
            ]

    else
        none


viewMobile : Bool -> msg -> (String -> msg) -> Element msg
viewMobile showHelp toggleMsg noOpMsg =
    if showHelp then
        el
            [ width fill
            , height fill
            , Font.size 14
            , behindContent <|
                el
                    [ width fill
                    , height fill
                    , alpha 0.5
                    , Background.color UI.white
                    , Element.Events.onClick toggleMsg
                    ]
                    none
            ]
        <|
            column
                [ centerX
                , centerY
                , width fill
                , height fill
                , padding 20
                , Background.color UI.accentColorLight
                , spacing 20
                ]
                [ el [] <| MobileUI.heading "How to play?"
                , paragraph [ spacing 3, width fill ]
                    [ text "1. Write pinyin and tones in the input box"
                    , text "    on the bottom, like this:"
                    , el [ width fill ] <|
                        row
                            [ width fill
                            , Border.widthEach
                                { top = 0
                                , left = 0
                                , right = 0
                                , bottom = 2
                                }
                            , spacing 20
                            , Background.color UI.white
                            , centerX
                            , inFront <| el [ Background.color <| rgba255 0 0 0 0, width fill, height fill ] none
                            ]
                            [ Input.text
                                [ Border.width 0
                                , inFront <| row [ centerY, padding 10, Font.color UI.accentColor ] [ el [ width <| px 50 ] none, UI.arrowLeft 24 ]
                                ]
                                { onChange = noOpMsg
                                , text = "xie3"
                                , placeholder = Nothing
                                , label = Input.labelHidden ""
                                }
                            , Input.button
                                [ mouseOver [ Font.color UI.accentColorHighlight ]
                                , Font.color UI.accentColor
                                , Background.color UI.white
                                , width shrink
                                , Border.rounded 10
                                ]
                                { onPress = Nothing
                                , label =
                                    el [ centerY, centerX, paddingXY 20 0, spacing 20 ] <|
                                        text "Ok"
                                }
                            ]
                    ]
                , column [ spacing 3, width fill ]
                    [ text "2. Either press the 'Ok' button or press Enter"
                    , text "    on the keyboard"
                    , el [ width fill ] <|
                        row
                            [ width fill
                            , Border.widthEach
                                { top = 0
                                , left = 0
                                , right = 0
                                , bottom = 2
                                }
                            , spacing 20
                            , Background.color UI.white
                            , centerX
                            , Font.color UI.gray
                            , inFront <| el [ Background.color <| rgba255 0 0 0 0, width fill, height fill ] none
                            ]
                            [ Input.text [ Border.width 0 ]
                                { onChange = noOpMsg
                                , text = "xie3"
                                , placeholder = Nothing
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
                                , inFront <| row [ centerY, padding 10, Font.color UI.accentColor ] [ el [ width <| px 20 ] none, column [] [ el [ height <| px 10 ] none, UI.cursorClick 24 ] ]
                                ]
                                { onPress = Nothing
                                , label =
                                    el [ centerY, centerX, paddingXY 20 0, spacing 20 ] <|
                                        text "Ok"
                                }
                            ]
                    ]
                , column [ spacing 3, width fill ]
                    [ paragraph []
                        [ text "3. If the pinyin and tone is correct of one or more "
                        , text "    of the characters, the hanzi will show up in green. "
                        , text "    If only the pinyin was correct, the pinyin will show up"
                        , text "    in yellow. Try again with the correct tone"
                        , text "    All the wrong attempt will be shown in gray below the board"
                        ]
                    , row [ spacing 20, centerX, padding 10 ]
                        [ viewExampleHanziCorrect
                        , viewExampleHanziSimilar
                        , viewExampleHanziIncorrect
                        ]
                    ]
                , column [ spacing 3, width fill ]
                    [ paragraph []
                        [ text "4. When all characters are found, or when more than 5 wrong attempts "
                        , text "    were made, the round is over."
                        , text "    All characters are then shown, color coded by their tone."
                        ]
                    , row [ spacing 20, centerX, padding 10 ]
                        [ viewExampleHanziFinishedRound "听" First False
                        , viewExampleHanziFinishedRound "人" Second False
                        , viewExampleHanziFinishedRound "写" Third True
                        , viewExampleHanziFinishedRound "四" Forth False
                        , viewExampleHanziFinishedRound "吧" Fifth False
                        ]
                    ]
                , column [ spacing 3 ]
                    [ text "5. Now press 'Next' to go to the next round of words"
                    ]
                , column [ paddingXY 0 10 ]
                    [ text <| "The dictionary currently contains a total of " ++ (String.fromInt <| List.length allWords) ++ " words" ]
                , el [ alignBottom, centerX ] <| MobileUI.niceButton "Close" toggleMsg Nothing
                ]

    else
        none


viewExampleHanziCorrect : Element msg
viewExampleHanziCorrect =
    el
        [ UI.floating
        , UI.rounded 5
        , paddingXY 20 0
        , width <| px 50
        , height <| px 50
        , Font.color UI.black
        , Font.medium
        , Background.color <| rgb255 152 226 172
        , Border.color <| rgba 0 0 0 0
        , Border.width 2
        ]
    <|
        el [ centerX, centerY, Font.size 20 ] <|
            text "写"


viewExampleHanziSimilar : Element msg
viewExampleHanziSimilar =
    el
        [ UI.floating
        , UI.rounded 5
        , paddingXY 20 0
        , width shrink
        , height <| px 50
        , Font.color UI.black
        , Font.medium
        , Background.color <| rgb255 240 230 110
        , Border.color <| rgba 0 0 0 0
        , Border.width 2
        ]
    <|
        el [ centerX, centerY, Font.size 20 ] <|
            text <|
                Tones.replace "xie1"


viewExampleHanziIncorrect : Element msg
viewExampleHanziIncorrect =
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
            text "hao3"


viewExampleHanziFinishedRound : String -> Tone -> Bool -> Element msg
viewExampleHanziFinishedRound pinyin tone correct =
    el
        [ UI.floating
        , UI.rounded 5
        , paddingXY 20 0
        , width <| px 50
        , height <| px 50
        , Font.color <| toneToColor tone
        , Font.medium
        , Background.color <| UI.white
        , Border.color <|
            if correct then
                rgb255 152 226 172

            else
                rgb255 230 125 125
        , Border.width 2
        ]
    <|
        el [ centerX, centerY, Font.size 20 ] <|
            text pinyin
