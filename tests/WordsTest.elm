module WordsTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tones
import Words


suite : Test
suite =
    describe "parsing text into pinyin"
        [ test "pinyin 1" <|
            \_ ->
                Words.splitStringIntoPinyin "yi1"
                    |> Expect.equal (Ok [ { pinyin = "yi", tone = Tones.First } ])
        , test "pinyin 2" <|
            \_ ->
                Words.splitStringIntoPinyin "yi2"
                    |> Expect.equal (Ok [ { pinyin = "yi", tone = Tones.Second } ])
        , test "pinyin 3" <|
            \_ ->
                Words.splitStringIntoPinyin "yi3"
                    |> Expect.equal (Ok [ { pinyin = "yi", tone = Tones.Third } ])
        , test "pinyin 4" <|
            \_ ->
                Words.splitStringIntoPinyin "yi4"
                    |> Expect.equal (Ok [ { pinyin = "yi", tone = Tones.Forth } ])
        , test "pinyin 5" <|
            \_ ->
                Words.splitStringIntoPinyin "yi5"
                    |> Expect.equal (Ok [ { pinyin = "yi", tone = Tones.Fifth } ])
        , test "pinyin 6" <|
            \_ ->
                Words.splitStringIntoPinyin "yi123"
                    |> Expect.equal (Ok [ { pinyin = "yi", tone = Tones.First } ])
        , test "pinyin err 1" <|
            \_ ->
                Words.splitStringIntoPinyin "yi"
                    |> Expect.err
        , test "pinyin err 2" <|
            \_ ->
                Words.splitStringIntoPinyin "yi6"
                    |> Expect.err
        , test "pinyin err 3" <|
            \_ ->
                Words.splitStringIntoPinyin "1"
                    |> Expect.equal (Ok [])
        , test "pinyin err 4" <|
            \_ ->
                Words.splitStringIntoPinyin "-1"
                    |> Expect.equal (Ok [])
        , test "format pinyin 1" <|
            \_ ->
                Words.formatPinyin { pinyin = "yi", tone = Tones.First }
                    |> Expect.equal "yī"
        , test "format pinyin 2" <|
            \_ ->
                Words.formatPinyin { pinyin = "yi", tone = Tones.Second }
                    |> Expect.equal "yí"
        , test "format pinyin 3" <|
            \_ ->
                Words.formatPinyin { pinyin = "yi", tone = Tones.Third }
                    |> Expect.equal "yǐ"
        , test "format pinyin 4" <|
            \_ ->
                Words.formatPinyin { pinyin = "yi", tone = Tones.Forth }
                    |> Expect.equal "yì"
        , test "format pinyin 5" <|
            \_ ->
                Words.formatPinyin { pinyin = "an", tone = Tones.Fifth }
                    |> Expect.equal "ān"
        ]
