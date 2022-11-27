module GameTest exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Extra as List
import Main
import Test exposing (..)
import Training
import Words exposing (newWord)


suite : Test
suite =
    let
        words1 =
            [ newWord "啊" "a5" "INTERJECTION: Ah, oh"
            , newWord "爱" "ai4" "VERB: to love"
            ]

        dictionaries =
            Dict.fromList
                [ ( "test1"
                  , [ newWord "啊" "a5" "INTERJECTION: Ah, oh"
                    , newWord "爱" "ai4" "VERB: to love"
                    , newWord "球队" "qiu2dui4" "NOUN: team (ball sport)"
                    , newWord "球迷" "qiu2mi2" "NOUN: fan (of a ball sport)"
                    , newWord "按" "an4" "VERB: to press, to push down"
                    , newWord "点" "dian3" "NOUN: (decimal）point | VERB: to click"
                    , newWord "举行" "ju3xing2" "VERB: to hold (a meeting, ceremony)"
                    ]
                  )
                , ( "test2"
                  , [ newWord "裙子" "qun2zi5" "NOUN: skirt"
                    , newWord "清楚" "qing1chu3" "ADJECTIVE: clear, distinct"
                    , newWord "正式" "zheng4shi4" "ADJECTIVE: formal"
                    , newWord "关门" "guan1men2" "VERB: to close (a door, a shop)"
                    , newWord "累" "lei4" "ADJECTIVE: (be) tired, fatigued"
                    ]
                  )
                ]
    in
    describe "End to end"
        [ test "Words to go 1" <|
            \_ ->
                Training.wordsToGo Dict.empty dictionaries []
                    |> Expect.equal (dictionaries |> Dict.values |> List.concat |> List.length)
        , test "Words to go 2" <|
            \_ ->
                Training.wordsToGo Dict.empty dictionaries words1
                    |> Expect.equal ((dictionaries |> Dict.values |> List.concat |> List.length) - List.length words1)
        , test "Words to go 3" <|
            \_ ->
                let
                    d =
                        Words.allDictNames dictionaries
                            |> List.map (\name -> ( name, False ))
                            |> Dict.fromList
                in
                Training.wordsToGo d dictionaries words1
                    |> Expect.equal 0
        , test "Words to go 4" <|
            \_ ->
                let
                    d =
                        Words.allDictNames dictionaries
                            |> List.map (\name -> ( name, True ))
                            |> Dict.fromList
                in
                Training.wordsToGo d dictionaries []
                    |> Expect.equal (dictionaries |> Dict.values |> List.concat |> List.length)
        ]
