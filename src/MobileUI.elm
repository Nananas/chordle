module MobileUI exposing (..)

import Element exposing (..)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Html
import Html.Attributes
import Html.Events
import Icons
import Svg
import Svg.Attributes
import UI


niceButton text onClick mIcon =
    niceButtonWith [] text onClick mIcon


niceButtonWith attr text onClick mIcon =
    Element.Input.button
        ([ mouseOver [ Element.Background.color UI.accentColorHighlight ]
         , Element.Background.color UI.accentColor
         , Element.Font.color UI.white
         , width shrink
         , height <| px 40
         , Element.Border.rounded 20
         ]
            ++ attr
        )
        { onPress = Just onClick
        , label =
            row [ width fill, height fill, paddingXY 20 0, spacing (mIcon |> Maybe.map (\_ -> 20) |> Maybe.withDefault 0) ]
                [ el [ centerY ] <|
                    case mIcon of
                        Nothing ->
                            none

                        Just icon ->
                            icon
                , el
                    [ Element.Font.bold
                    , Element.Font.size 14
                    , htmlAttribute <| Html.Attributes.style "padding-top" "4px"
                    ]
                  <|
                    Element.text text
                ]
        }


simpleIconButton icon onClick =
    Element.Input.button
        [ mouseOver [ Element.Font.color UI.accentColorHighlight ]
        , Element.Font.color UI.accentColor
        , width shrink
        , height <| px 40
        ]
        { onPress = Just onClick
        , label =
            el [ centerY, centerX, paddingXY 20 0, spacing 20 ]
                icon
        }


simpleIconButtonInverted icon onClick =
    Element.Input.button
        [ Element.Font.color UI.white
        , width shrink
        , height <| px 40
        ]
        { onPress = Just onClick
        , label =
            el [ centerY, centerX, paddingXY 20 0, spacing 20 ]
                icon
        }


niceIconButton icon onClick =
    Element.Input.button
        [ mouseOver [ Element.Font.color UI.accentColorHighlight ]
        , Element.Font.color UI.white
        , Element.Background.color UI.accentColor
        , width shrink
        , height <| px 40
        , width <| px 40
        , Element.Border.rounded 10
        ]
        { onPress = Just onClick
        , label =
            el [ centerY, centerX, paddingXY 20 0, spacing 20 ]
                icon
        }


niceTextInput options =
    niceTextInputWith [] options


niceTextInputWith attr { txt, onChange, placeholder, label, icon } =
    Element.Input.text
        ([ Element.Border.widthEach { top = 0, bottom = 2, left = 0, right = 0 }
         , Element.Border.color UI.accentColor
         , height <| px 40
         ]
            ++ attr
        )
        { onChange = onChange
        , text = txt
        , placeholder =
            Just <|
                Element.Input.placeholder [] <|
                    row [ spacing 20 ]
                        [ icon |> Maybe.map (\i -> i 20) |> Maybe.withDefault none
                        , text placeholder
                        ]
        , label = Element.Input.labelHidden label
        }


niceText str =
    niceTextWith [] str


niceTextWith attr str =
    el
        ([ Element.Font.color UI.accentColor
         , centerX
         , centerY
         , Element.Font.bold
         , Element.Font.center
         , UI.uppercase
         ]
            ++ attr
        )
    <|
        Element.text str


heading str =
    el
        [ Element.Font.color UI.accentColor
        , centerX
        , centerY
        , Element.Font.bold
        , Element.Font.center
        , Element.Font.size 16
        ]
    <|
        Element.text str


arrowRight size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.viewBox "0 0 24 24"
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ Svg.path
                    [ Svg.Attributes.strokeLinecap "round"
                    , Svg.Attributes.strokeLinejoin "round"
                    , Svg.Attributes.d "M14 5l7 7m0 0l-7 7m7-7H3"
                    ]
                    []
                ]


arrowLeft size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.viewBox "0 0 24 24"
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ Svg.path
                    [ Svg.Attributes.strokeLinecap "round"
                    , Svg.Attributes.strokeLinejoin "round"
                    , Svg.Attributes.d "M10 19l-7-7m0 0l7-7m-7 7h18"
                    ]
                    []
                ]


arrowDown size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.viewBox "0 0 24 24"
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ Svg.path
                    [ Svg.Attributes.strokeLinecap "round"
                    , Svg.Attributes.strokeLinejoin "round"
                    , Svg.Attributes.d "M19 14l-7 7m0 0l-7-7m7 7V3"
                    ]
                    []
                ]


cursorClick size =
    el [ width <| px size, height <| px size ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.viewBox "0 0 24 24"
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.stroke "currentColor"
                ]
                [ Svg.path
                    [ Svg.Attributes.strokeLinecap "round"
                    , Svg.Attributes.strokeLinejoin "round"
                    , Svg.Attributes.d "M15 15l-2 5L9 9l11 4-5 2zm0 0l5 5M7.188 2.239l.777 2.897M5.136 7.965l-2.898-.777M13.95 4.05l-2.122 2.122m-5.657 5.656l-2.12 2.122"
                    ]
                    []
                ]


keys =
    [ [ "q", "w", "e", "r", "t", "y", "u", "i", "o", "p" ]
    , [ "a", "s", "d", "f", "g", "h", "j", "k", "l" ]
    , [ "z", "x", "c", "v", "b", "n", "m" ]
    ]


tones =
    [ ( Icons.firstTone, "1" )
    , ( Icons.secondTone, "2" )
    , ( Icons.thirdTone, "3" )
    , ( Icons.forthTone, "4" )
    , ( Icons.fifthTone, "5" )
    ]


viewKeyboard : (String -> msg) -> msg -> msg -> Element msg
viewKeyboard onCharMsg onBackspaceMsg onClearMsg =
    keys
        |> List.map
            (\row ->
                row
                    |> List.map
                        (\key ->
                            Element.Input.button
                                [ UI.floating
                                , width fill
                                , height fill
                                , mouseOver [ Element.Background.color UI.accentColorLight ]
                                , Element.Events.onMouseDown <| onCharMsg key
                                ]
                                { onPress = Nothing
                                , label = el [ centerX, centerY ] (text key)
                                }
                        )
                    |> Element.row [ spacing 4, width fill, height fill ]
            )
        |> (::)
            (Element.row [ spacing 4, width fill, height fill ] <|
                List.map
                    (\( icon, ch ) ->
                        Element.Input.button
                            [ UI.floating
                            , width fill
                            , height fill
                            , mouseOver [ Element.Background.color UI.accentColorLight ]
                            , Element.Events.onMouseDown <| onCharMsg ch
                            ]
                            { onPress = Nothing
                            , label = el [ centerX, centerY ] <| icon 20
                            }
                    )
                    tones
            )
        |> (::)
            (Element.row [ spacing 4, width fill, height fill ]
                [ Element.Input.button
                    [ UI.floating
                    , width fill
                    , height fill
                    , mouseOver [ Element.Background.color UI.accentColorLight ]
                    ]
                    { onPress = Just onClearMsg
                    , label = el [ centerX, centerY ] (text "Clear")
                    }
                , Element.Input.button
                    [ UI.floating
                    , width fill
                    , height fill
                    , mouseOver [ Element.Background.color UI.accentColorLight ]
                    ]
                    { onPress = Just onBackspaceMsg
                    , label = el [ centerX, centerY ] (text "Backspace")
                    }
                ]
            )
        |> Element.column
            [ spacing 4
            , width fill
            , height <| minimum 200 fill
            , Element.Border.widthEach { top = 1, bottom = 1, left = 0, right = 0 }
            , Element.Border.color UI.accentColorLight
            ]


modal toggleMsg content =
    el
        [ width fill
        , height fill
        , Element.Font.size 14
        , behindContent <|
            el
                [ width fill
                , height fill
                , alpha 0.5
                , Element.Background.color UI.white
                , Element.Events.onClick toggleMsg
                ]
                none
        ]
    <|
        column
            --[ width fill, height fill, htmlAttribute <| Html.Attributes.style "pointer-events" "none" ] <|
            [ centerX
            , centerY
            , height fill
            , width fill
            , padding 20
            , Element.Background.color UI.accentColorLight
            , spacing 20
            , htmlAttribute <| Html.Attributes.style "pointer-events" "auto"
            , Element.Font.size 14
            ]
            (content
                ++ [ el [ alignBottom, centerX ] <|
                        niceButton "Close" toggleMsg Nothing
                   ]
            )
