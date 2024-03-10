module Changelog exposing (..)

import Date
import Element exposing (..)
import Element.Background
import Element.Font
import Icons
import MobileUI
import Time exposing (Month(..))
import UI


currentVersion : String
currentVersion =
    List.head changelog
        |> Maybe.map (\( _, v, _ ) -> v)
        |> Maybe.withDefault "0.0.0"


changelog =
    [ ( Date.fromCalendarDate 2024 Mar 10
      , "0.3.7"
      , [ "Added Dictionaries CLT 5 'Book Semester 2' and 'Extra Semester 2'. Improved translation of some words."
        ]
      )
    , ( Date.fromCalendarDate 2024 Feb 3
      , "0.3.6"
      , [ "Added settings menu with choice option for colored pinyin: 'Pleco', 'Chordle' or just black.\n   Default option is 'Chordle'"
        ]
      )
    , ( Date.fromCalendarDate 2023 Dec 28
      , "0.3.5"
      , [ "Added the Shift keyboard shortcut to show pinyin of all characters between rounds"
        ]
      )
    , ( Date.fromCalendarDate 2023 Dec 11
      , "0.3.4"
      , [ "Added 9 more CLT5 semester 1 Extra words"
        ]
      )
    , ( Date.fromCalendarDate 2023 Dec 8
      , "0.3.3"
      , [ "Added the Ctrl+Enter keyboard shortcut to start and end rounds without the need of a mouse"
        ]
      )
    , ( Date.fromCalendarDate 2023 Nov 29
      , "0.3.2"
      , [ "Added option to show only pinyin instead of hanzi (except Numbers game, currently)"
        ]
      )
    , ( Date.fromCalendarDate 2023 Oct 15
      , "0.3.1"
      , [ "Fixed issue with training stats not being loaded properly"
        ]
      )
    , ( Date.fromCalendarDate 2023 Oct 14
      , "0.3.0"
      , [ "Added dictionaries CLT5 Semester 1 & Extra"
        , "Added 130+ CLT5 semester 1 words!"
        , "Added 30+ CLT5 semester 1 Extra words"
        ]
      )
    , ( Date.fromCalendarDate 2023 May 22
      , "0.2.10"
      , [ "Added 6 new words to CLT4 semester 2 Extra"
        ]
      )
    , ( Date.fromCalendarDate 2023 May 14
      , "0.2.9"
      , [ "Fix various issues with English in the Numbers game."
        , "Fixed issues with 二/两 in the Numbers game"
        ]
      )
    , ( Date.fromCalendarDate 2023 May 8
      , "0.2.8"
      , [ "Added 7 new words to CLT4 semester 2 Extra"
        ]
      )
    , ( Date.fromCalendarDate 2023 Apr 22
      , "0.2.7"
      , [ "Added 14 new words to CLT4 semester 2 Extra"
        , "Improved translations for 薄, 瘦 and 油彩."
        ]
      )
    , ( Date.fromCalendarDate 2023 Apr 5
      , "0.2.6"
      , [ "Various tone and translation fixes for CLT4 semester 2 (Extra)" ]
      )
    , ( Date.fromCalendarDate 2023 Mar 28
      , "0.2.5"
      , [ "Added 27 new words to CLT4 semester 2 Extra" ]
      )
    , ( Date.fromCalendarDate 2023 Mar 11
      , "0.2.4"
      , [ "Added English language mode to the Numbers game" ]
      )
    , ( Date.fromCalendarDate 2023 Mar 8
      , "0.2.3"
      , [ "Added words for CLT4 semester 2 Extra" ]
      )
    , ( Date.fromCalendarDate 2023 Feb 23
      , "0.2.1"
      , [ "Added words for CLT4 semester 2 Extra" ]
      )
    , ( Date.fromCalendarDate 2023 Feb 6
      , "0.2.0"
      , [ "Added a numbers game, to practice translating large numbers.\n   This game is currently still unfinished, but should already be playable." ]
      )
    , ( Date.fromCalendarDate 2023 Feb 4
      , "0.1.0"
      , [ "This is the start of the changelog", "Dictionaries added: 'CLT4 Semester 2' and partial 'Extra CLT4 Semester 2'" ]
      )
    ]


view mStorageVersion onMobile toggleMsg =
    let
        modalFn =
            if onMobile then
                MobileUI.modal

            else
                UI.modal
    in
    modalFn toggleMsg
        [ row [ centerX ]
            [ UI.heading "The Changelog:"
            ]
        , changelog
            |> List.map
                (\( date, version, changes ) ->
                    column [ width fill, alignLeft, spacing 20 ]
                        [ row [ width fill ]
                            [ UI.niceTextWith [ alignLeft ] ("v" ++ version)
                            , let
                                showNew =
                                    case mStorageVersion of
                                        Nothing ->
                                            True

                                        Just storageVersion ->
                                            versionToTuple version > versionToTuple storageVersion
                              in
                              if showNew then
                                row [ centerX, spacing 10 ]
                                    [ el [ Element.Font.color UI.accentColor ] <| Icons.sparkle 20
                                    , UI.niceTextWith [] "NEW"
                                    , el [ Element.Font.color UI.accentColor ] <| Icons.sparkle 20
                                    ]

                              else
                                none
                            , el [ alignRight, Element.Font.color UI.gray ] (text <| Date.toIsoString date)
                            ]
                        , column [ width fill, spacing 5 ] <|
                            List.map (\txt -> text <| "•  " ++ txt) changes
                        ]
                )
            |> column [ centerX, padding 20, spacing 20, Element.Background.color UI.lightColor, UI.rounded 20 ]
            |> el [ scrollbarY, height <| minimum 500 fill, width fill ]
        ]


versionToTuple version =
    case String.split "." version of
        [ maj, min, patch ] ->
            ( maj, min, patch )

        _ ->
            ( "?", "?", "?" )
