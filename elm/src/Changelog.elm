module Changelog exposing (..)

import Date
import Element exposing (..)
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
    [ ( Date.fromCalendarDate 2023 Mar 8
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
        [ row [ centerX, spacing 20 ]
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
            |> column [ padding 20, spacing 20, scrollbarY, height <| px 500, width fill ]
        ]


versionToTuple version =
    case String.split "." version of
        [ maj, min, patch ] ->
            ( maj, min, patch )

        _ ->
            ( "?", "?", "?" )
