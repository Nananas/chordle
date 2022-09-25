module GameTest exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Extra as List
import Main
import Test exposing (..)
import Words exposing (newWord)


suite : Test
suite =
    let
        words1 =
            [ newWord "啊" "a5" "INTERJECTION: Ah, oh"
            , newWord "爱" "ai4" "VERB: to love"
            ]
    in
    describe "End to end"
        [ test "Words to go 1" <|
            \_ ->
                Main.wordsToGo Dict.empty []
                    |> Expect.equal (Words.allDicts |> List.map Tuple.second |> List.concat |> List.length)
        , test "Words to go 2" <|
            \_ ->
                Main.wordsToGo Dict.empty words1
                    |> Expect.equal ((Words.allDicts |> List.map Tuple.second |> List.concat |> List.length) - List.length words1)
        , test "Words to go 3" <|
            \_ ->
                let
                    d =
                        Words.allDicts
                            |> List.map (\( name, _ ) -> ( name, False ))
                            |> Dict.fromList
                in
                Main.wordsToGo d words1
                    |> Expect.equal 0
        , test "Words to go 4" <|
            \_ ->
                let
                    d =
                        Words.allDicts
                            |> List.map (\( name, _ ) -> ( name, True ))
                            |> Dict.fromList
                in
                Main.wordsToGo d []
                    |> Expect.equal (Words.allDicts |> List.map Tuple.second |> List.concat |> List.length)
        ]
