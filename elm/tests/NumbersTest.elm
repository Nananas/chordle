module NumbersTest exposing (..)

import Expect
import Numbers
import Test exposing (..)


suite : Test
suite =
    describe "Numbers game"
        [ test "English number formatting" <|
            \_ ->
                [ Numbers.formattedNumberEnglish 100
                , Numbers.formattedNumberEnglish 110
                , Numbers.formattedNumberEnglish 200
                , Numbers.formattedNumberEnglish 220
                , Numbers.formattedNumberEnglish 1000
                , Numbers.formattedNumberEnglish 1100
                , Numbers.formattedNumberEnglish 2000
                , Numbers.formattedNumberEnglish 2200
                , Numbers.formattedNumberEnglish 10000
                , Numbers.formattedNumberEnglish 11000
                , Numbers.formattedNumberEnglish 20000
                , Numbers.formattedNumberEnglish 22000
                , Numbers.formattedNumberEnglish 100000
                , Numbers.formattedNumberEnglish 110000
                , Numbers.formattedNumberEnglish 200000
                , Numbers.formattedNumberEnglish 220000
                , Numbers.formattedNumberEnglish 1000000
                , Numbers.formattedNumberEnglish 1100000
                , Numbers.formattedNumberEnglish 2000000
                , Numbers.formattedNumberEnglish 2200000
                , Numbers.formattedNumberEnglish 10000000
                , Numbers.formattedNumberEnglish 11000000
                , Numbers.formattedNumberEnglish 20000000
                , Numbers.formattedNumberEnglish 22000000
                , Numbers.formattedNumberEnglish 100000000
                , Numbers.formattedNumberEnglish 110000000
                , Numbers.formattedNumberEnglish 200000000
                , Numbers.formattedNumberEnglish 220000000
                , Numbers.formattedNumberEnglish 1000000000
                , Numbers.formattedNumberEnglish 1100000000
                , Numbers.formattedNumberEnglish 2000000000
                , Numbers.formattedNumberEnglish 2200000000
                , Numbers.formattedNumberEnglish 10000000000
                , Numbers.formattedNumberEnglish 11000000000
                , Numbers.formattedNumberEnglish 20000000000
                , Numbers.formattedNumberEnglish 22000000000
                ]
                    |> Expect.equal
                        [ "one hundred"
                        , "one hundred and ten"
                        , "two hundred"
                        , "two hundred and twenty"
                        , "one thousand"
                        , "one thousand one hundred"
                        , "two thousand"
                        , "two thousand two hundred"
                        , "ten thousand"
                        , "eleven thousand"
                        , "twenty thousand"
                        , "twenty two thousand"
                        , "one hundred thousand"
                        , "one hundred and ten thousand"
                        , "two hundred thousand"
                        , "two hundred and twenty thousand"
                        , "one million"
                        , "one million one hundred thousand"
                        , "two million"
                        , "two million two hundred thousand"
                        , "ten million"
                        , "eleven million"
                        , "twenty million"
                        , "twenty two million"
                        , "one hundred million"
                        , "one hundred and ten million"
                        , "two hundred million"
                        , "two hundred and twenty million"
                        , "one billion"
                        , "one billion one hundred million"
                        , "two billion"
                        , "two billion two hundred million"
                        , "ten billion"
                        , "eleven billion"
                        , "twenty billion"
                        , "twenty two billion"
                        ]
        , test "Hanzi number formatting" <|
            \_ ->
                [ Numbers.formattedNumberHanzi 100
                , Numbers.formattedNumberHanzi 110
                , Numbers.formattedNumberHanzi 200
                , Numbers.formattedNumberHanzi 220
                , Numbers.formattedNumberHanzi 1000
                , Numbers.formattedNumberHanzi 1100
                , Numbers.formattedNumberHanzi 2000
                , Numbers.formattedNumberHanzi 2200
                , Numbers.formattedNumberHanzi 10000
                , Numbers.formattedNumberHanzi 11000
                , Numbers.formattedNumberHanzi 20000
                , Numbers.formattedNumberHanzi 22000
                , Numbers.formattedNumberHanzi 100000
                , Numbers.formattedNumberHanzi 110000
                , Numbers.formattedNumberHanzi 200000
                , Numbers.formattedNumberHanzi 220000
                , Numbers.formattedNumberHanzi 1000000
                , Numbers.formattedNumberHanzi 1100000
                , Numbers.formattedNumberHanzi 2000000
                , Numbers.formattedNumberHanzi 2200000
                , Numbers.formattedNumberHanzi 10000000
                , Numbers.formattedNumberHanzi 11000000
                , Numbers.formattedNumberHanzi 20000000
                , Numbers.formattedNumberHanzi 22000000
                , Numbers.formattedNumberHanzi 100000000
                , Numbers.formattedNumberHanzi 110000000
                , Numbers.formattedNumberHanzi 200000000
                , Numbers.formattedNumberHanzi 220000000
                , Numbers.formattedNumberHanzi 1000000000
                , Numbers.formattedNumberHanzi 1100000000
                , Numbers.formattedNumberHanzi 2000000000
                , Numbers.formattedNumberHanzi 2200000000
                , Numbers.formattedNumberHanzi 10000000000
                , Numbers.formattedNumberHanzi 11000000000
                , Numbers.formattedNumberHanzi 20000000000
                , Numbers.formattedNumberHanzi 22000000000
                ]
                    |> Expect.equal
                        [ "一 百"
                        , "一 百 一 十"
                        , "两 百"
                        , "两 百 二 十"
                        , "一 千"
                        , "一 千 一 百"
                        , "两 千"
                        , "两 千 两 百"
                        , "一 万"
                        , "一 万 一 千"
                        , "两 万"
                        , "两 万 两 千"
                        , "一 十 万"
                        , "一 十 一 一 万"
                        , "二 十 万"
                        , "二 十 一 二 万"
                        , "一 百 万"
                        , "一 百 一 十 万"
                        , "两 百 万"
                        , "两 百 二 十 万"
                        , "一 千 万"
                        , "一 千 一 百 万"
                        , "两 千 万"
                        , "两 千 两 百 万"
                        , "一 亿"
                        , "一 亿 一 千 万"
                        , "二 亿"
                        , "二 亿 两 千 万"
                        , "一 十 亿"
                        , "一 十 一 一 亿"
                        , "二 十 亿"
                        , "二 十 一 二 亿"
                        , "一 百 亿"
                        , "一 百 一 十 亿"
                        , "两 百 亿"
                        , "两 百 二 十 亿"
                        ]
        ]
