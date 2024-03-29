module UI exposing (..)

import Element exposing (..)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Html.Attributes
import Html.Events
import Icons
import Json.Decode
import Svg
import Svg.Attributes



-- palette:
-- https://mycolor.space/?hex=%23845EC2&sub=1


spinner =
    el [ width fill, height fill ] <|
        el [ centerX, centerY ] <|
            el [ htmlAttribute <| Html.Attributes.class "spin" ] none


rounded r =
    Element.Border.rounded r


accentColor =
    rgb255 132 94 194


accentColorHighlight =
    rgb255 179 156 208


accentColorLight =
    rgb255 251 234 255


lightColor =
    rgb255 252 247 255


errorColor =
    rgb255 255 111 145


errorColorLight =
    rgb255 255 131 165


correctColor =
    rgb255 0 201 167


correctColorLight =
    rgb255 120 221 187


black =
    rgb 0 0 0


white =
    rgb 1 1 1


gray =
    rgb 0.5 0.5 0.5


lightGray =
    rgb 0.8 0.8 0.8


veryLightGray =
    rgb 0.9 0.9 0.9


floating =
    Element.Border.shadow { offset = ( 0, 2 ), size = 0, blur = 2, color = rgba 0 0 0 0.2 }


floatingHigh =
    Element.Border.shadow { offset = ( 0, 4 ), size = 0, blur = 4, color = rgba 0 0 0 0.3 }


bottomBorder px =
    Element.Border.widthEach { bottom = px, left = 0, right = 0, top = 0 }


hr =
    el [ width fill, height (px 2), bottomBorder 2 ] none


uppercase =
    --Element.Font.variant Element.Font.smallCaps
    Element.Font.variant Element.Font.slashedZero


niceButton text onClick mIcon =
    niceButtonWith [] text onClick mIcon


niceToggleButton text onClick mIcon state =
    niceButtonWith
        (mouseOver
            [ Element.Background.color accentColorHighlight ]
            :: (if state == True then
                    [ Element.Background.color accentColor ]

                else
                    [ Element.Background.color white, Element.Font.color accentColor ]
               )
            ++ [ Element.Border.width 2
               , Element.Border.color accentColor
               ]
        )
        text
        onClick
        mIcon


niceButtonWith attr text onClick mIcon =
    Element.Input.button
        ([ mouseOver [ Element.Background.color accentColorHighlight ]
         , Element.Background.color accentColor
         , Element.Font.color white
         , width shrink
         , height <| px 40
         , Element.Border.rounded 20
         ]
            ++ attr
        )
        { onPress = Just onClick
        , label =
            row [ width fill, height fill, paddingXY 20 0, spacing (mIcon |> Maybe.map (\_ -> 10) |> Maybe.withDefault 0) ]
                [ el [ centerY ] <|
                    case mIcon of
                        Nothing ->
                            none

                        Just icon ->
                            icon
                , el
                    [ Element.Font.bold

                    --, Element.Font.center
                    --, uppercase
                    , Element.Font.size 20
                    , htmlAttribute <| Html.Attributes.style "padding-top" "4px"
                    ]
                  <|
                    Element.text text
                ]
        }


simpleIconButton =
    simpleIconButtonWith []


simpleIconButtonWith attr icon onClick tooltip =
    Element.Input.button
        ([ mouseOver [ Element.Font.color accentColorHighlight ]
         , Element.Font.color accentColor
         , width shrink
         , height <| px 40
         , htmlAttribute <| Html.Attributes.title tooltip
         ]
            ++ attr
        )
        { onPress = Just onClick
        , label =
            el [ centerY, centerX, paddingXY 20 0, spacing 20 ]
                icon
        }


niceIconButton =
    niceIconButtonWith []


niceIconButtonWith attr icon onClick tooltip =
    Element.Input.button
        ([ mouseOver [ Element.Font.color accentColorHighlight ]
         , Element.Font.color white
         , Element.Background.color accentColor
         , width shrink
         , height <| px 40
         , width <| px 40
         , Element.Border.rounded 20
         , htmlAttribute <| Html.Attributes.title tooltip
         , floating
         ]
            ++ attr
        )
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
         , Element.Border.color accentColor
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
        ([ Element.Font.color accentColor
         , centerX
         , centerY
         , Element.Font.bold
         , Element.Font.center
         , uppercase
         ]
            ++ attr
        )
    <|
        Element.text str


heading =
    headingWith []


headingWith attr str =
    el
        ([ Element.Font.color accentColor
         , centerX
         , centerY
         , Element.Font.bold
         , Element.Font.center
         , Element.Font.size 24
         ]
            ++ attr
        )
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


modal toggleMsg content =
    el
        [ width fill
        , height fill
        , behindContent <|
            el
                [ width fill
                , height fill
                , alpha 0.5
                , Element.Background.color white
                , Element.Events.onClick toggleMsg
                ]
                none
        ]
    <|
        el [ padding 50, width fill, height fill, htmlAttribute <| Html.Attributes.style "pointer-events" "none" ] <|
            column
                [ centerX
                , centerY
                , floatingHigh
                , width <| minimum 500 shrink
                , height <| minimum 400 shrink
                , padding 40
                , Element.Background.color accentColorLight
                , spacing 20
                , rounded 10
                , htmlAttribute <| Html.Attributes.style "pointer-events" "auto"
                ]
                (content
                    ++ [ el [ alignBottom, centerX ] <| niceButton "Close" toggleMsg Nothing ]
                )


viewFooter =
    el [ alignRight, alignBottom, padding 10 ] <|
        newTabLink []
            { url = "https://github.com/Nananas/chordle"
            , label =
                Element.Input.button
                    [ mouseOver [ Element.Font.color accentColorHighlight ]
                    , Element.Font.color accentColor
                    , width shrink
                    , height <| px 40
                    ]
                    { onPress = Nothing
                    , label =
                        el [ centerY, centerX, paddingXY 20 0, spacing 20 ]
                            (Icons.github 32)
                    }
            }


viewLogo txt =
    el [ width fill, height fill ] <|
        el [ centerX, centerY, Element.Font.color white, Element.Font.bold ] <|
            text ("Chordle " ++ txt)


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
