module WordsTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tones
import WordChain
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
        , test "pinyin 7" <|
            \_ ->
                Words.splitStringIntoPinyin "da3suan54"
                    |> Expect.equal
                        (Ok
                            [ { pinyin = "da", tone = Tones.Third }
                            , { pinyin = "suan", tone = Tones.FifthOr Tones.Forth }
                            ]
                        )
        , test "pinyin 8" <|
            \_ ->
                Words.splitStringIntoPinyin "xiao3jie53"
                    |> Expect.equal
                        (Ok
                            [ { pinyin = "xiao", tone = Tones.Third }
                            , { pinyin = "jie", tone = Tones.FifthOr Tones.Third }
                            ]
                        )
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
                    |> Expect.err
        , test "pinyin err 4" <|
            \_ ->
                Words.splitStringIntoPinyin "-1"
                    |> Expect.err
        , test "pinyin err 5" <|
            \_ ->
                Words.splitStringIntoPinyin "yi123"
                    |> Expect.err
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
                    |> Expect.equal "an"
        , test "tone check 1" <|
            \_ ->
                Tones.isMatchingTones Tones.First Tones.First
                    |> Expect.equal True
        , test "tone check 2" <|
            \_ ->
                Tones.isMatchingTones Tones.First Tones.Second
                    |> Expect.equal False
        , test "tone check 3" <|
            \_ ->
                Tones.isMatchingTones Tones.Fifth Tones.Fifth
                    |> Expect.equal True
        , test "tone check 4" <|
            \_ ->
                Tones.isMatchingTones Tones.Fifth Tones.First
                    |> Expect.equal False
        , test "tone check 5" <|
            \_ ->
                Tones.isMatchingTones (Tones.FifthOr Tones.First) Tones.First
                    |> Expect.equal True
        , test "tone check 6" <|
            \_ ->
                Tones.isMatchingTones (Tones.FifthOr Tones.First) Tones.Fifth
                    |> Expect.equal True
        , test "tone check 7" <|
            \_ ->
                Tones.isMatchingTones (Tones.FifthOr Tones.First) Tones.Second
                    |> Expect.equal False
        , test "tone check 8" <|
            \_ ->
                Tones.isMatchingTones (Tones.FifthOr Tones.First) (Tones.FifthOr Tones.First)
                    |> Expect.equal True
        , test "tone known 1" <|
            \_ ->
                Words.isCharacterKnown { hanzi = "火", pinyinPart = Words.PinyinPart "huo" Tones.Third }
                    [ Words.PinyinPart "huo" Tones.Third ]
                    |> Expect.equal True
        , test "tone known 2" <|
            \_ ->
                Words.isCharacterKnown { hanzi = "火", pinyinPart = Words.PinyinPart "huo" Tones.Third }
                    [ Words.PinyinPart "huo" Tones.Second ]
                    |> Expect.equal False
        , test "tone known 3" <|
            \_ ->
                Words.isCharacterKnown { hanzi = "算", pinyinPart = Words.PinyinPart "suan" (Tones.FifthOr Tones.Third) }
                    [ Words.PinyinPart "suan" Tones.Third ]
                    |> Expect.equal True
        , test "tone known 4" <|
            \_ ->
                Words.isCharacterKnown { hanzi = "算", pinyinPart = Words.PinyinPart "suan" (Tones.FifthOr Tones.Third) }
                    [ Words.PinyinPart "suan" Tones.Fifth ]
                    |> Expect.equal True
        , test "pinyin valid check 1" <|
            \_ ->
                WordChain.isPinyinValid (Words.PinyinPart "suan" Tones.Fifth)
                    [ ( Words.newWord "打算" "da3suan54" "VERB: to plan, to intend|NOUN: consideration, calculation", 0 ) ]
                    |> Expect.equal True
        , test "pinyin valid check 2" <|
            \_ ->
                WordChain.isPinyinValid (Words.PinyinPart "suan" Tones.Forth)
                    [ ( Words.newWord "打算" "da3suan54" "VERB: to plan, to intend|NOUN: consideration, calculation", 0 ) ]
                    |> Expect.equal True
        , test "pinyin valid check 3" <|
            \_ ->
                WordChain.isPinyinValid (Words.PinyinPart "suan" Tones.First)
                    [ ( Words.newWord "打算" "da3suan54" "VERB: to plan, to intend|NOUN: consideration, calculation", 0 ) ]
                    |> Expect.equal False
        , test "pinyin valid check 4" <|
            \_ ->
                WordChain.isPinyinValid (Words.PinyinPart "xin" Tones.First)
                    [ ( Words.newWord "小心" "xiao3xin51" "ADJECTIVE: careful, mindful|VERB: be careful", 0 ) ]
                    |> Expect.equal True
        , test "pinyin valid check 5" <|
            \_ ->
                WordChain.isPinyinValid (Words.PinyinPart "xin" Tones.Fifth)
                    [ ( Words.newWord "小心" "xiao3xin51" "ADJECTIVE: careful, mindful|VERB: be careful", 0 ) ]
                    |> Expect.equal True
        ]
