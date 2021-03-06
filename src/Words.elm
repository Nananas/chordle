module Words exposing (..)

-- ^(\S*)\s*(\S*)\s*(.*)
-- ,Word "$1" "$2" "$3"

import Json.Encode
import List.Extra as List
import Set exposing (Set)
import Tones exposing (..)


type alias Hanzi =
    String


type alias PinyinPart =
    { pinyin : String, tone : Tone }


type PinyinPartsSimilarity
    = CompletelySimilar
    | PinyinSimilar
    | ToneSimilar
    | NothingSimilar


type alias Character =
    { hanzi : Hanzi, pinyinPart : PinyinPart }


type alias Word =
    { characters : List Character
    , english : String
    }


wordToStringParts : Word -> ( String, String, String )
wordToStringParts word =
    let
        hanzi =
            word.characters |> List.map .hanzi |> String.join ""

        pinyin =
            word.characters |> List.map (.pinyinPart >> formatPinyin) |> String.join ""

        english =
            word.english
    in
    ( hanzi, pinyin, english )


charToTone : Char -> Maybe Tone
charToTone c =
    case String.fromList [ c ] |> String.toInt |> Maybe.withDefault 0 of
        1 ->
            Just First

        2 ->
            Just Second

        3 ->
            Just Third

        4 ->
            Just Forth

        5 ->
            Just Fifth

        _ ->
            Nothing


toneToString tone =
    case tone of
        First ->
            "1"

        Second ->
            "2"

        Third ->
            "3"

        Forth ->
            "4"

        Fifth ->
            "5"


pinyinPartsSimilarity : PinyinPart -> PinyinPart -> PinyinPartsSimilarity
pinyinPartsSimilarity p1 p2 =
    if p1 == p2 then
        CompletelySimilar

    else if p1.pinyin == p2.pinyin then
        PinyinSimilar

    else if p1.tone == p2.tone then
        ToneSimilar

    else
        NothingSimilar


similarWords : Word -> List Word -> List Word
similarWords word wordList =
    let
        hanziPartsSet =
            word.characters
                |> List.map .hanzi
                |> Set.fromList
    in
    wordList
        |> List.filter
            (\{ characters } ->
                characters
                    |> List.map .hanzi
                    |> Set.fromList
                    |> Set.intersect hanziPartsSet
                    |> Set.isEmpty
                    |> not
            )



--isWordFullyShown : Word -> Bool
--isWordFullyShown word =
--    let
--        go : List Character -> Bool -> Bool
--        go remaining acc =
--            case remaining of
--                head :: tail ->
--                    if head.show then
--                        acc && go tail True
--                    else
--                        acc && False
--                [] ->
--                    acc
--    in
--    go word.characters True


splitStringIntoPinyin : String -> Result String (List PinyinPart)
splitStringIntoPinyin str =
    let
        fn : Char -> ( List Char, List PinyinPart ) -> ( List Char, List PinyinPart )
        fn char ( acc, parts ) =
            case charToTone char of
                Nothing ->
                    ( char :: acc, parts )

                Just tone ->
                    ( [], { pinyin = String.fromList <| List.reverse <| acc, tone = tone } :: parts )

        result =
            str
                |> String.replace "??" "v"
                |> String.replace "//" ""
                |> String.toLower
                |> String.toList
                |> List.foldl fn ( [], [] )
    in
    case result of
        ( [], parts ) ->
            Ok (List.reverse parts)

        ( any, _ ) ->
            Err "Missing tone number (12345)"


formatPinyin : PinyinPart -> String
formatPinyin pinyinPart =
    pinyinPart.pinyin
        ++ toneToString pinyinPart.tone
        |> Tones.replace



--


emptyWord : Word
emptyWord =
    newWord "" "" ""


newWord : String -> String -> String -> Word
newWord hanzi pinyin english =
    let
        pinyinParts =
            case splitStringIntoPinyin pinyin of
                Err err ->
                    []

                Ok p ->
                    p

        hanziParts =
            hanzi |> String.toLower |> String.split ""

        characters =
            List.zip hanziParts pinyinParts
                |> List.map (\( h, p ) -> { hanzi = h, pinyinPart = p })
    in
    { characters = characters
    , english = english
    }


allWords : List Word
allWords =
    [ newWord "???" "a5" "INTERJECTION: Ah, oh"
    , newWord "???" "ai4" "VERB: to love"
    , newWord "??????" "an1quan2" "NOUN: safety, security"
    , newWord "??????" "ba4ba5" "NOUN: dad"
    , newWord "???" "ba5" "MODAL PARTICLE: (a modal particle)"
    , newWord "???" "bai2" "ADJECTIVE: white"
    , newWord "??????" "bai2bai2" "IDIOM EXPRESSION: bye-bye (transliteration)"
    , newWord "????????????" "bai2ma3wang2zi3" "NOUN: knight in shining armor"
    , newWord "???" "bai3" "NUMERAL: hundred"
    , newWord "???" "ban4" "VERB: to do, to handle"
    , newWord "???" "ban4" "NUMERAL: half"
    , newWord "??????" "ban4fa3" "NOUN: method, way"
    , newWord "??????" "ban4gong1" "VERB PLUS OBJECT: to handle official business, to work (usu. In an office)"
    , newWord "?????????" "ban4gong1shi4" "NOUN: office"
    , newWord "??????" "bang1zhu4" "VERB: to help"
    , newWord "??????" "bao1guo3" "NOUN: bundle, package"
    , newWord "??????" "bao1kuo4" "VERB: include"
    , newWord "???" "bao2" "ADJECTIVE: thin"
    , newWord "??????" "bao4qian4" "ADJECTIVE: sorry"
    , newWord "??????" "bao4zhi3" "NOUN: newspaper"
    , newWord "???" "bei1" "MEASURE WORD: cups or glasses"
    , newWord "??????" "Bei3jing1" "PROPER NOUN: Beijing"
    , newWord "???" "ben3" "MEASURE WORD: books or notebooks"
    , newWord "??????" "ben3zi5" "NOUN: exercise book, notebook"
    , newWord "???" "ben4" "ADJECTIVE: stupid, dull"
    , newWord "???" "bi3" "PREPOSITION: then (indicating comparison)"
    , newWord "???" "bi3" "NOUN: stick-like writing or drawing instruments"
    , newWord "?????????" "bi3ji4ben3" "NOUN: notebook|NOUN: abbreviation for notebook (computer), laptop"
    , newWord "?????????" "Bi3li4shi2" "PROPER NOUN: Belgium"
    , newWord "???" "bian4" "MEASURE WORD: (for an action) number of times"
    , newWord "??????" "bian4tiao2" "NOUN: note"
    , newWord "???" "biao3" "NOUN: (wrist)watch|NOUN: form, list"
    , newWord "???" "bing1" "NOUN: soldier, fighter"
    , newWord "???" "bing4" "NOUN: illness|VERB: to get sick"
    , newWord "??????" "bu2cuo4" "ADJECTIVE: not bad"
    , newWord "??????" "bu2yong4" "ADVERB: need not/no need"
    , newWord "???" "bu4" "NOUN: cloth, fabric"
    , newWord "???" "bu4" "ADVERB: not, no"
    , newWord "???" "bu4" "NOUN: step"
    , newWord "????????????" "bu4//hao3yi4si5" "IDIOM EXPRESSION: sorry"
    , newWord "??????" "bu4xing2" "VERB: to be no way, to be out of question"
    , newWord "???" "cai4" "NOUN: dish"
    , newWord "??????" "cai4dan1" "NOUN: menu"
    , newWord "??????" "can1guan1" "VERB: to visit, have a look around"
    , newWord "??????" "can1jia1" "VERB: to participate, to attend"
    , newWord "??????" "can1ting1" "NOUN: dining hall"
    , newWord "??????" "ce4suo3" "NOUN: toilet"
    , newWord "???" "ceng2" "MEASURE WORD: story, floor"
    , newWord "???" "cha2" "VERB: to check, to look up"
    , newWord "???" "cha2" "NOUN: tea"
    , newWord "??????" "cha2lou2" "NOUN: tearoom, tea house"
    , newWord "???" "cha4" "VERB: to be short of, to lack|ADJECTIVE: not up to standard, bad"
    , newWord "???" "chang2" "ADJECTIVE: long"
    , newWord "???" "chang2" "ADVERB: often"
    , newWord "??????" "chang2chang2" "ADVERB: often"
    , newWord "??????" "chang4//ge1" "VERB: to sing"
    , newWord "???" "che1" "NOUN: car"
    , newWord "??????" "chen4shan1" "NOUN: shirt"
    , newWord "???" "chi1" "VERB: to eat"
    , newWord "??????" "chi1//fan4" "VERB PLUS OBJECT: to eat (a meal)"
    , newWord "??????" "chi1//jing1" "VERB PLUS OBJECT: to be startled, to be shocked"
    , newWord "???" "chu1" "VERB: to go or come out"
    , newWord "??????" "chu1qu4" "VERB: to go, to come out"
    , newWord "??????" "chu1sheng1" "VERB: to be born"
    , newWord "??????" "chu2fang2" "NOUN: kitchen"
    , newWord "???" "chuan1" "VERB: to wear"
    , newWord "???" "chuan2" "NOUN: boat, ship"
    , newWord "???" "chuang2" "NOUN: bed"
    , newWord "?????????" "chuang2tou2gui4" "NOUN: bedside cupboard"
    , newWord "???" "ci2" "NOUN: word"
    , newWord "??????" "ci2dian3" "NOUN: dictionary"
    , newWord "???" "cong2" "PREPOSITION: from, as in from... to..."
    , newWord "???" "dao4" "PREPOSITION: to, as in from... to..."
    , newWord "???" "cuo4" "ADJECTIVE: wrong, erroneous"
    , newWord "???" "da3" "VERB: to play"
    , newWord "??????" "da3//bao1" "VERB PLUS OBJECT: to pack"
    , newWord "??????" "da3//di2" "VERB PLUS OBJECT: to hail a taxi"
    , newWord "??????" "da3//gong1" "VERB PLUS OBJECT: to do manual work for somebody, to have a part-time job"
    , newWord "??????" "da3//zhen1" "VERB: to give an injection, to have an injection"
    , newWord "?????????" "da3dian4hua4" "VERB PLUS OBJECT: to make a phone call"
    , newWord "??????" "da3qiu2" "VERB PLUS OBJECT: to play ball games"
    , newWord "??????" "da3sao3" "VERB: to clean"
    , newWord "??????" "da3zhe2" "VERB PLUS OBJECT: to sell at a discount, to give a discount"
    , newWord "???" "da4" "ADJECTIVE: big, large"
    , newWord "?????????" "da4bai2cai4" "NOUN: big Chinese cabbage"
    , newWord "??????" "da4bian4" "NOUN: stool, faeces"
    , newWord "???" "dai4" "VERB: to bring"
    , newWord "???" "dai4" "VERB: put on, wear (e.g. for masks)"
    , newWord "???" "dan4" "NOUN: egg"
    , newWord "??????" "dan4gao1" "NOUN: cake"
    , newWord "??????" "dang1ran2" "ADVERB: of course"
    , newWord "???" "dao4" "VERB: to arrive"
    , newWord "??????" "De2guo2" "PROPER NOUN: Germany"
    , newWord "???" "de3" "VERB: to have to, to must, to get"
    , newWord "???" "de5" "STRUCTURAL PARTICLE: (a possessive or modifying particle)"
    , newWord "???" "deng3" "VERB: to wait"
    , newWord "??????" "di4di5" "NOUN: younger brother"
    , newWord "??????" "di4fang5" "NOUN: place"
    , newWord "??????" "di4tu2" "NOUN: map"
    , newWord "?????????" "di4yi1ci4" "the first time"
    , newWord "??????" "dian3zhong1" "MEASURE WORD: o'clock"
    , newWord "???" "dian4" "NOUN: shop, store"
    , newWord "???" "dian4" "NOUN: electricity"
    , newWord "??????" "dian4hua4" "NOUN: telephone, phone call"
    , newWord "??????" "dian4nao3" "NOUN: computer"
    , newWord "??????" "dian4shi4" "NOUN: TV"
    , newWord "?????????" "dian4shi4ju4" "NOUN: TV drama (or play)"
    , newWord "??????" "dian4ying3" "NOUN: movie"
    , newWord "????????????" "dian4zi3you2jian4" "NOUN: email"
    , newWord "?????????" "Dong1ao4hui4" "NOUN: the Winter Olympics"
    , newWord "??????" "dong1xi5" "NOUN: thing, stuff"
    , newWord "???" "dong3" "VERB: to understand"
    , newWord "???" "dou1" "ADVERB: both, all"
    , newWord "??????" "du4zi5" "NOUN: belly, abdomen"
    , newWord "???" "duan3" "ADJECTIVE: short"
    , newWord "??????" "duan4lian4" "VERB: to exercise, to work out"
    , newWord "???" "dui4" "ADJECTIVE: right correct|PREPOSITION: with regard to, concerning"
    , newWord "???" "dui4" "NOUN: row of people"
    , newWord "?????????" "dui4bu5qi3" "IDIOM EXPRESSION: I'm sorry"
    , newWord "??????" "dui4mian4" "NOUN: opposite to"
    , newWord "???" "duo1" "ADJECTIVE: many, much|ADVERB: how (many/much)"
    , newWord "??????" "duo1da4" "IDIOM EXPRESSION: how old"
    , newWord "??????" "duo1shao5" "QUESTION PRONOUN: how many, how much"
    , newWord "??????" "er2zi5" "NOUN: son"
    , newWord "?????????" "fa1gao1shao1" "VERB PLUS OBJECT: to have a *high* fever"
    , newWord "??????" "fa1shao1" "VERB PLUS OBJECT: to have a fever"
    , newWord "??????" "fa1yan2" "VERB PLUS OBJECT: to become inflamed"
    , newWord "??????" "fa1zhan3" "VERB: to develop, to advance, to expand, to grow"
    , newWord "???" "fa2" "VERB: to punish, to penalize"
    , newWord "??????" "fa2kuan3" "VERB PLUS OBJECT: to impose a fine or forfeit"
    , newWord "??????" "Fa3guo2" "PROPER NOUN: France"
    , newWord "??????" "fan1yi4" "VERB: to translate, to interpret"
    , newWord "???" "fan4" "NOUN: meal"
    , newWord "??????" "fan4dian4" "NOUN: hotel|NOUN: restaurant"
    , newWord "??????" "fang1bian4" "ADJECTIVE: convenient"
    , newWord "??????" "fang2jian1" "NOUN: room"
    , newWord "??????" "fang2zi5" "NOUN: house"
    , newWord "??????" "fei1chang2" "ADVERB: very, extremely, highly"
    , newWord "??????" "fei1fa3" "ADJECTIVE: illegal"
    , newWord "??????" "fen1qian2" "MEASURE WORD: (Chinese monetary unit, it is equal to 1/100 yuan)"
    , newWord "??????" "fen1zhong1" "MEASURE WORD: minute"
    , newWord "??????" "fen3hong2" "ADJECTIVE: pink"
    , newWord "???" "fen4" "MEASURE WORD: publications such as newspapers"
    , newWord "??????" "fu2wu4" "VERB: give service to, to serve"
    , newWord "?????????" "fu2wu4yuan2" "NOUN: attendant"
    , newWord "?????????" "fu4huo2jie2" "PROPER NOUN: Easter"
    , newWord "??????" "fu4xi2" "VERB: to review"
    , newWord "???" "gai1" "OPTATIVE VERB: should, ought to"
    , newWord "???" "gai3" "VERB: change, correct"
    , newWord "??????" "gan3mao4" "VERB: to have a cold|NOUN: common cold"
    , newWord "???" "gang1" "ADVERB: just, only a short while ago"
    , newWord "??????" "gang1cai2" "ADVERB: just now"
    , newWord "???" "gao1" "ADJECTIVE: tall"
    , newWord "??????" "gao1xing4" "ADJECTIVE: happy, pleased"
    , newWord "??????" "gao4su5" "VERB: to tell"
    , newWord "??????" "ge1ge5" "NOUN: elder brother"
    , newWord "??????" "ge2li2" "VERB: isolate, separate"
    , newWord "???" "gei3" "VERB: to give|PREPOSITION: to, for"
    , newWord "???" "gen1" "PREPOSITION: with|VERB: to follow"
    , newWord "??????" "gong1fen1" "NOUN: centimetre"
    , newWord "??????" "gong1jin1" "MEASURE WORD: kilogram (kg)"
    , newWord "??????" "gong1si1" "NOUN: company"
    , newWord "??????" "gong1yuan2" "NOUN: AD, the Christian era"
    , newWord "??????" "gong1yuan2" "NOUN: park"
    , newWord "??????" "gong1zuo4" "VERB: to work|NOUN: work, job"
    , newWord "?????????" "Gong4chan3dang3" "PROPER NOUN: Communist Party"
    , newWord "???" "gou3" "NOUN: dog"
    , newWord "??????" "gu1niang5" "NOUN: girl"
    , newWord "??????" "gua4//hao4" "VERB: to register (at a hospital, etc.)"
    , newWord "???" "guai1" "ADJECTIVE: (of a child) well-behaved, good"
    , newWord "???" "guan3" "NOUN: term for certain service establishments or places for cultural activities"
    , newWord "??????" "guang1pan2" "NOUN: CD"
    , newWord "??????" "Guang3dong1" "PROPER NOUN: Guangdong Province"
    , newWord "??????" "guang3gao4" "NOUN: advertisement, ad, commercial"
    , newWord "???" "gui4" "ADJECTIVE: expensive, precious"
    , newWord "??????" "gui4xing4" "IDIOM EXPRESSION: your surname (polite form)"
    , newWord "??????" "gui4zi5" "NOUN: cupboard, cabinet"
    , newWord "???" "guo2" "NOUN: country, nation"
    , newWord "??????" "guo2ji2" "NOUN: nationality"
    , newWord "??????" "guo4//qu5" "NOUN: in the past"
    , newWord "??????" "guo4qi1" "VERB PLUS OBJECT: to be overdue"
    , newWord "???" "guo5" "VERB: to spend (time), to celebrate (someone's birthday, a festival, etc.)"
    , newWord "???" "hai2" "ADVERB: in addition, still"
    , newWord "??????" "hai2shi5" "CONJUNCTION: or"
    , newWord "??????" "hai2zi5" "NOUN: child"
    , newWord "??????" "Han4yu3" "NOUN: Chinese language"
    , newWord "??????" "Han4zi4" "NOUN: Chinese character"
    , newWord "???" "hao3" "ADJECTIVE: good well, fine, OK"
    , newWord "??????" "hao3jiu3" "ADJECTIVE: a very long time"
    , newWord "????????????" "hao3jiu3bu4jian4" "IDIOM EXPRESSION: haven't seen (sb.) for a very long time"
    , newWord "???" "hao4" "NOUN: ordinal number, day of the month"
    , newWord "???" "he1" "VERB: drink"
    , newWord "???" "he2" "CONJUNCTION: and"
    , newWord "??????" "he2shi4" "ADJECTIVE: suitable"
    , newWord "??????" "he2ying3" "NOUN: group photo (or picture)|VERB PLUS OBJECT: take a group photo (or picture)"
    , newWord "???" "hei1" "ADJECTIVE: black"
    , newWord "??????" "hei1ke4" "NOUN: hacker"
    , newWord "???" "hen3" "ADVERB: very"
    , newWord "???" "hong2" "ADJECTIVE: red"
    , newWord "????????????" "hong2pu2tao5jiu3" "NOUN: red wine"
    , newWord "??????" "hu4shi5" "NOUN: (hospital) nurse"
    , newWord "??????" "hu4zhao4" "NOUN: passport"
    , newWord "??????" "hua1cai4" "NOUN: broccoli"
    , newWord "??????" "hua1sheng1" "NOUN: peanut"
    , newWord "??????" "hua2qiao2" "NOUN: overseas Chinese"
    , newWord "??????" "hua2ren2" "NOUN: Chinese people"
    , newWord "??????" "hua4ju4" "NOUN: modern drama, stage play"
    , newWord "??????" "hua4xue2" "NOUN: chemistry"
    , newWord "???" "huan4" "VERB: to exchange, to change"
    , newWord "???" "huang2" "ADJECTIVE: yellow"
    , newWord "??????" "huang2se4" "NOUN: yellow (colou)|ADJECTIVE: decadent, obscene, pornographic"
    , newWord "???" "hui2" "VERB: to return"
    , newWord "??????" "hui2da2" "VERB: to answer"
    , newWord "??????" "hui2xin4" "NOUN: reply letter|VERB PLUS OBJECT: to reply (in writing)"
    , newWord "???" "hui4" "NOUN: meeting|OPTATIVE VERB: can, to have the knowledge of"
    , newWord "???" "huo4" "NOUN: goods"
    , newWord "??????" "huo4zhe3" "CONJUNCTION: or, either (not in a question)"
    , newWord "??????" "ji2le5" "extremely"
    , newWord "???" "ji3" "QUESTION PRONOUN: how many, how much"
    , newWord "???" "ji4" "VERB: to post, to mail"
    , newWord "??????" "ji4zhe3" "NOUN: reporter"
    , newWord "???" "jia1" "NOUN: family, home"
    , newWord "?????????" "jia1chang2cai4" "NOUN: home cooking"
    , newWord "???" "jia3" "ADJECTIVE: fake"
    , newWord "???" "jian1" "MEASURE WORD: rooms, houses, etc."
    , newWord "??????" "jian3ce4" "VERB: test, examine, check up (medical)"
    , newWord "???" "jian4" "MEASURE WORD: piece(s)"
    , newWord "???" "jiao1" "VERB: to hand in, to hand over, to pay (the rent, etc.)"
    , newWord "???" "jiao1" "VERB: to teach, to instruct"
    , newWord "???" "jiao4" "VERB: to be called"
    , newWord "??????" "jiao4shou4" "NOUN: professor"
    , newWord "??????" "jiao4yu4" "NOUN: education"
    , newWord "???" "jie2" "NOUN: festival"
    , newWord "??????" "jie3jie5" "NOUN: elder sister"
    , newWord "???" "jie4" "VERB: to borrow"
    , newWord "??????" "jie4shao4" "VERB: to introduce"
    , newWord "?????????" "jie4shu1zheng4" "NOUN: library card"
    , newWord "???" "jin1" "MEASURE WORD: weight, around 500 g"
    , newWord "??????" "jin1nian2" "NOUN: this year"
    , newWord "??????" "jin1tian1" "NOUN: today"
    , newWord "???" "jin4" "VERB: to enter"
    , newWord "??????" "jin4//lai5" "VERB PLUS COMPLEMENT: to come in"
    , newWord "??????" "jing1ji4" "NOUN: economics"
    , newWord "??????" "jing1ju4" "NOUN: Beijing opera"
    , newWord "??????" "jing1li3" "NOUN: manager"
    , newWord "??????" "jing1xi3" "NOUN: pleasant surprise"
    , newWord "???" "jiu3" "NOUN: wine or liquor"
    , newWord "??????" "jiu3dian4" "NOUN: hotel (often used in the name|NOUN: wineshop; public house 2 hotel [in names of hotels]"
    , newWord "???" "jiu4" "ADVERB: exactly, precisely"
    , newWord "???" "ju2" "NOUN: office, bureau"
    , newWord "??????" "ju2huang2" "ADJECTIVE: orange (color)"
    , newWord "??????" "ju2zi5" "NOUN: tangerine"
    , newWord "??????" "ju4hui4" "NOUN: party, get-together"
    , newWord "??????" "jue2de5" "VERB: to feel, to think"
    , newWord "??????" "ka1fei1" "NOUN: coffee"
    , newWord "?????????" "ka1fei1se4" "NOUN: coffee (color)"
    , newWord "???" "kai1" "VERB: to open, to start"
    , newWord "??????" "kai1//che1" "VERB PLUS OBJECT: to drive a car or a train, etc."
    , newWord "??????" "kai1//dao1" "VERB PLUS OBJECT: to have an operation"
    , newWord "??????" "kai1//xue2" "VERB PLUS OBJECT: school opens, term begins"
    , newWord "??????" "kai1shi3" "VERB: to start, to begin"
    , newWord "???" "kan4" "VERB: to watch, to look at"
    , newWord "??????" "kan4//bing4" "VERB PLUS OBJECT: to see a doctor"
    , newWord "???" "kao3" "VERB: to give or take an examination, to test"
    , newWord "??????" "kao3shi4" "VERB PLUS OBJECT: to take an examination|NOUN: examination, exam, test"
    , newWord "??????" "kao3ya1" "NOUN: roast duck"
    , newWord "??????" "ke3ai4" "ADJECTIVE: lovely,cute"
    , newWord "??????" "ke3le4" "NOUN: coke"
    , newWord "??????" "ke3neng2" "OPTATIVE VERB: maybe"
    , newWord "??????" "ke3shi4" "CONJUNCTION: but"
    , newWord "??????" "ke3yi3" "OPTATIVE VERB: may"
    , newWord "???" "ke4" "MEASURE WORD: quarter of an hour"
    , newWord "???" "ke4" "NOUN: class, lesson"
    , newWord "??????" "ke4ben3" "NOUN: textbook"
    , newWord "??????" "ke4ting1" "NOUN: living room"
    , newWord "??????" "kong3pa4" "ADVERB: afraid that..."
    , newWord "???" "kou3" "MEASURE WORD: number of people in a family"
    , newWord "??????" "kou3yu3" "NOUN: spoken language"
    , newWord "??????" "kou3zhao4" "NOUN: surgical mask"
    , newWord "??????" "ku4zi5" "NOUN: trousers, pants"
    , newWord "???" "kuai4" "ADJECTIVE: fast"
    , newWord "??????" "kuai4le4" "ADJECTIVE: happy"
    , newWord "???" "kuan3" "NOUN: a sum of money"
    , newWord "???" "lai2" "VERB: to come"
    , newWord "???" "lan2" "ADJECTIVE: blue"
    , newWord "??????" "lao3hu3" "NOUN: tiger"
    , newWord "??????" "lao3ren2" "NOUN: the elderly, the aged, old man or woman"
    , newWord "??????" "lao3shi1" "NOUN: teacher"
    , newWord "???" "leng3" "ADJECTIVE: cold"
    , newWord "???" "Li3" "PROPER NOUN: (one of the Chinese surnames)"
    , newWord "??????" "li3wu4" "NOUN: gift,present"
    , newWord "??????" "li4shi3" "NOUN: history"
    , newWord "???" "lian3" "NOUN: face (of people or animals)"
    , newWord "???" "lian4" "VERB: to practice"
    , newWord "??????" "lian4xi2" "VERB: to practice|NOUN: exercise"
    , newWord "???" "liang3" "NUMERAL: two"
    , newWord "?????????" "liao3bu5qi3" "IDIOM EXPRESSION: amazing, terrific, extraordinary"
    , newWord "??????" "liao4zi5" "NOUN: material for making clothes"
    , newWord "???" "ling2" "NUMERAL: zero"
    , newWord "??????" "liu2li4" "ADJECTIVE: fluent"
    , newWord "?????????" "liu2xue2sheng1" "NOUN: student studying abroad"
    , newWord "???" "lou2" "NOUN: building, floor, level"
    , newWord "???" "luan4" "ADJECTIVE: disordered, messy"
    , newWord "??????" "l??3xing2" "VERB: to travel"
    , newWord "??????" "l??3you2" "VERB: to travel for tourism"
    , newWord "???" "l??4" "ADJECTIVE: green"
    , newWord "??????" "l??4shi1" "NOUN: lawyer"
    , newWord "??????" "ma1ma5" "NOUN: mom"
    , newWord "??????" "ma2fan5" "VERB: to bother somebody, to trouble somebody"
    , newWord "???" "ma5" "QUESTION PARTICLE: (particle used for a question expecting a yes-no answer)"
    , newWord "???" "mai3" "VERB: to buy"
    , newWord "??????" "mai3dan1" "NOUN: bill, tab"
    , newWord "???" "mai4" "VERB: to sell"
    , newWord "???" "man4" "ADJECTIVE: slow"
    , newWord "???" "mang2" "ADJECTIVE: busy"
    , newWord "???" "mao2" "MEASURE WORD: (Chinese monetary unit, it is equal to 1/10 yuan)"
    , newWord "?????????" "mei2guan1xi5" "IDIOM EXPRESSION: never mind, it doesn't matter"
    , newWord "???" "mei3" "PRONOUN: every, each"
    , newWord "???" "mei3" "ADJECTIVE: beautiful"
    , newWord "??????" "mei3guo2" "PROPER NOUN: the United States of America"
    , newWord "??????" "mei3n??3" "NOUN: beautiful woman"
    , newWord "??????" "mei3shu4" "NOUN: fine arts"
    , newWord "??????" "mei3yuan2" "NOUN: US dollar"
    , newWord "??????" "mei4mei5" "NOUN: younger sister"
    , newWord "???" "men5" "SUFFIX: (used after pronouns ???, ???, ???, ???, ??? or certain nouns to denote plurality)"
    , newWord "??????" "mi3fan4" "NOUN: (cooked) rice"
    , newWord "??????" "mi4ma3" "NOUN: password, cipher, secret code"
    , newWord "??????" "mian3//fei4" "VERB PLUS OBJECT: free; gratis"
    , newWord "???" "mian4" "NOUN: noodles"
    , newWord "??????" "mian4bao1" "NOUN: bread"
    , newWord "??????" "mian4zi5" "NOUN: face|NOUN: esteem, reputation"
    , newWord "??????" "ming2cai4" "NOUN: famous dish"
    , newWord "??????" "ming2pian4" "NOUN: business card"
    , newWord "??????" "ming2tian1" "NOUN: tomorrow"
    , newWord "?????????" "ming2xin4pian4" "NOUN: postcard"
    , newWord "??????" "ming2zi5" "NOUN: name"
    , newWord "??????" "mu4lu4" "NOUN: catalog, list"
    , newWord "???" "na2" "VERB: to take, to hold, to get"
    , newWord "???" "na3" "QUESTION PRONOUN: which"
    , newWord "??????" "na3r5" "QUESTION PRONOUN: where"
    , newWord "???" "na4" "PRONOUN: that"
    , newWord "??????" "na4r5" "PRONOUN: there"
    , newWord "??????" "nai3nai5" "NOUN: (paternal) grandmother"
    , newWord "???" "nan2" "ADJECTIVE: male"
    , newWord "???" "nan2" "ADJECTIVE: difficult"
    , newWord "??????" "nan2bian5" "NOUN: south"
    , newWord "??????" "nan2fang1" "NOUN: south (part of)"
    , newWord "???" "nao3" "NOUN: brain"
    , newWord "???" "ne5" "QUESTION PARTICLE:  (a modal particle used for elliptical questions)"
    , newWord "???" "neng2" "OPTATIVE VERB: can, to be able to"
    , newWord "???" "ni3" "PRONOUN: you"
    , newWord "???" "nian2" "NOUN: year"
    , newWord "??????" "nian2ling2" "NOUN: age"
    , newWord "??????" "nian2qing1" "ADJECTIVE: young"
    , newWord "???" "nian4" "VERB: to read"
    , newWord "???" "nin2" "PRONOUN: you (polite form)"
    , newWord "??????" "niu2nai3" "NOUN: milk"
    , newWord "???" "n??3" "ADJECTIVE: female"
    , newWord "??????" "n??3er2" "NOUN: daughter"
    , newWord "??????" "ou1zhou1" "PROPER NOUN: Europe"
    , newWord "??????" "Ou5yuan2" "NOUN: Euro"
    , newWord "??????" "pai1//zhao4" "VERB PLUS OBJECT: to take pictures"
    , newWord "???" "pai2" "VERB: to arrange, to put in order"
    , newWord "??????" "pai2//dui4" "VERB PLUS OBJECT: form a line, to queue up"
    , newWord "??????" "pang2bian1" "NOUN: next to, side"
    , newWord "??????" "peng2you5" "NOUN: friend"
    , newWord "??????" "pi2jiu3" "NOUN: beer"
    , newWord "??????" "pian2yi5" "ADJECTIVE: cheap, inexpensive"
    , newWord "??????" "piao4liang5" "ADJECTIVE: beautiful, nice"
    , newWord "???" "ping2" "MEASURE WORD: bottle(s)"
    , newWord "??????" "ping2guo3" "NOUN: apple"
    , newWord "??????" "pu3tong1" "ADJECTIVE: common, general"
    , newWord "?????????" "pu3tong1hua4" "NOUN: the common Chinese speech (Mandarin), standard Chinese"
    , newWord "???" "qi1" "NOUN: a period of time"
    , newWord "??????" "qi2pao2" "NOUN: cheongsam, a long close-fitting dress with a high neck and slit skirt"
    , newWord "???" "qi3" "VERB: to get up, to rise"
    , newWord "??????" "qi3//chuang2" "VERB PLUS OBJECT: to get up"
    , newWord "???" "qian1" "NUMERAL: thousand"
    , newWord "???" "qian2" "NOUN: money"
    , newWord "???" "qiao3" "ADJECTIVE: coincidental"
    , newWord "??????" "qin1ai4" "ADJECTIVE: dear"
    , newWord "???" "qing2" "ADJECTIVE: sunny"
    , newWord "???" "qing3" "VERB: please"
    , newWord "??????" "qing3wen4" "VERB: may I ask"
    , newWord "???" "qiu2" "NOUN: ball"
    , newWord "???" "qu4" "VERB: to go"
    , newWord "??????" "qu4nian2" "NOUN: last year"
    , newWord "???" "quan2" "ADJECTIVE: whole"
    , newWord "??????" "quan2shen1" "NOUN: all over the body"
    , newWord "???" "rang4" "VERB: to let, to allow, to make"
    , newWord "??????" "re4nao5" "ADJECTIVE: lively, bustling with noise and excitement"
    , newWord "??????" "re4xin1" "ADJECTIVE: warm-hearted"
    , newWord "???" "ren2" "NOUN: people, person"
    , newWord "??????" "ren2min2" "NOUN: people"
    , newWord "?????????" "ren2min2bi4" "NOUN: RMB, Chinese monetary unit"
    , newWord "??????" "ren2yuan2" "NOUN: personnel, staff"
    , newWord "??????" "ren4shi5" "VERB: to know (somebody)"
    , newWord "??????" "rong2yi4" "ADJECTIVE: easy"
    , newWord "???" "rou4" "NOUN: meat"
    , newWord "??????" "san4//bu4" "VERB PLUS OBJECT: to take a walk, to walk"
    , newWord "??????" "sang3zi5" "NOUN: throat"
    , newWord "???" "sao3" "VERB: to sweep"
    , newWord "??????" "sha1fa1" "NOUN: sofa"
    , newWord "???" "shang1" "NOUN: trade, commerce"
    , newWord "??????" "shang1chang3" "NOUN: market, shopping mall"
    , newWord "??????" "shang1dian4" "NOUN: shop, store"
    , newWord "???" "shang4" "NOUN: above, last"
    , newWord "??????" "shang4//ban1" "VERB PLUS OBJECT: go to work, start work"
    , newWord "??????" "shang4//ke4" "VERB PLUS OBJECT: to go to class (for both students and teachers)"
    , newWord "??????" "shang4//wang3" "VERB PLUS OBJECT: log onto the Internet, go online"
    , newWord "??????" "shang4hai3" "PROPER NOUN: Shanghai"
    , newWord "??????" "shang4wu3" "NOUN: morning"
    , newWord "???" "shao1" "VERB: to burn"
    , newWord "???" "shao3" "ADJECTIVE: few, less"
    , newWord "???" "shei2" "QUESTION PRONOUN: who, whom"
    , newWord "??????" "shen1ti3" "NOUN: body"
    , newWord "??????" "shen2me5" "QUESTION PRONOUN: what"
    , newWord "???" "sheng1" "ADJECTIVE: new|VERB: give birth"
    , newWord "??????" "sheng1//qi4" "VERB PLUS OBJECT: to take offence, get angry"
    , newWord "??????" "sheng1ci2" "NOUN: new word"
    , newWord "??????" "sheng1huo2" "NOUN: life, livelihood"
    , newWord "??????" "sheng1ri4" "NOUN: birthday"
    , newWord "??????" "shi1fu5" "NOUN: master"
    , newWord "??????" "shi2hou5" "NOUN: time, moment"
    , newWord "??????" "shi2jian1" "NOUN: time"
    , newWord "??????" "shi2yong4" "ADJECTIVE: practical"
    , newWord "???" "shi4" "SUFFIX: (used as a suffix to indicate something or somebody belongs to some type or style)"
    , newWord "???" "shi4" "NOUN: room"
    , newWord "???" "shi4" "NOUN: matter, affair, thing"
    , newWord "??????" "shou3ji1" "NOUN: handset, mobile phone"
    , newWord "???" "shou4" "VERB: to sell"
    , newWord "?????????" "shou4huo4yuan2" "NOUN: shop assistant, salesperson"
    , newWord "??????" "shou4mian4" "NOUN: (birthday) longevity noodles"
    , newWord "???" "shu1" "NOUN: book"
    , newWord "??????" "shu1dian4" "NOUN: bookstore"
    , newWord "??????" "shu1fu5" "ADJECTIVE: comfortable, (to feel) well"
    , newWord "??????" "shu1jia4" "NOUN: bookshelf"
    , newWord "??????" "shu1zhuo1" "NOUN: writing desk"
    , newWord "???" "shu3" "VERB: belong to, to be born in the year of (on of the 12 animals)"
    , newWord "???" "shu3" "VERB: to count"
    , newWord "??????" "shu3tiao2" "NOUN: french fries"
    , newWord "??????" "shu4xue2" "NOUN: mathematics"
    , newWord "???" "shuai4" "ADJECTIVE: handsome"
    , newWord "??????" "shuai4ge1" "NOUN: handsome young man"
    , newWord "???" "shuang1" "MEASURE WORD: pair"
    , newWord "???" "shui3" "NOUN: water"
    , newWord "?????????" "shui3dian4fei4" "NOUN: charges for water and electricity"
    , newWord "??????" "shui4//jiao4" "VERB PLUS OBJECT: to sleep"
    , newWord "???" "shuo1" "VERB: to say, to speak"
    , newWord "??????" "si1chou2" "NOUN: silk"
    , newWord "??????" "si1ji1" "NOUN: driver"
    , newWord "???" "si4" "NUMERAL: four"
    , newWord "???" "song4" "NOUN: to give (as a present)"
    , newWord "??????" "su4she4" "NOUN: dormitory"
    , newWord "???" "sui4" "MEASURE WORD: year of age"
    , newWord "??????" "sui4shu5" "NOUN: years of age"
    , newWord "?????????" "sun1n??5r5" "NOUN: granddaughter on son's side"
    , newWord "???" "ta1" "PRONOUN: he, him"
    , newWord "???" "ta1" "PRONOUN: she, her"
    , newWord "???" "tai4" "ADVERB: too, extremely"
    , newWord "?????????" "tai4ji2quan2" "NOUN: tai chi"
    , newWord "???" "tao4" "MEASURE WORD: set, suit, suite"
    , newWord "???" "teng2" "ADJECTIVE: painful"
    , newWord "?????????" "ti3yu4guan3" "NOUN: gym"
    , newWord "???" "tian1" "NOUN: day"
    , newWord "??????" "tian1qi4" "NOUN: weather"
    , newWord "???" "tian2" "VERB: to fill in, to write"
    , newWord "???" "tiao2" "MEASURE WORD: (a measure word for long, narrow objects, such as trousers, skirts, snakes, etc.)"
    , newWord "??????" "tiao4//wu3" "VERB PLUS OBJECT: to dance"
    , newWord "???" "ting1" "VERB: to listen"
    , newWord "??????" "ting1shuo1" "VERB: to be told"
    , newWord "??????" "tong2yi4" "VERB: agree, approve"
    , newWord "??????" "tong2zhi4" "NOUN: comrade|NOUN: common name for gay, homosexual"
    , newWord "???" "tou2" "NOUN: head"
    , newWord "??????" "tou2fa5" "NOUN: hair (on the human head)"
    , newWord "??????" "tu2shu1" "NOUN: books"
    , newWord "?????????" "tu2shu1guan3" "NOUN: library"
    , newWord "??????" "tu3dou4" "NOUN: potato"
    , newWord "???" "wai4" "NOUN: foreign, outside"
    , newWord "??????" "wai4gong1" "NOUN: (maternal) grandfather"
    , newWord "??????" "wai4guo2" "NOUN: foreign country"
    , newWord "??????" "wai4po2" "NOUN: (maternal) grandmother"
    , newWord "??????" "wai4yu3" "NOUN: foreign language"
    , newWord "???" "wan3" "ADJECTIVE: late"
    , newWord "??????" "wan3shang5" "NOUN: evening, night"
    , newWord "???" "wan4" "NUMERAL: ten thousand"
    , newWord "??????" "wang2fu3" "NOUN: palace of a prince, prince's residence"
    , newWord "??????" "wang3zhan4" "NOUN: website"
    , newWord "???" "wang4" "VERB: to forget"
    , newWord "???" "wei4" "MEASURE WORD: person(s) (polite)"
    , newWord "???" "wei4" "INTERJECTION: hello, hey"
    , newWord "???????????????" "wei4ren2min2fu2wu4" "IDIOM EXPRESSION: Serve the People!, CCP political slogan"
    , newWord "?????????" "wei4shen2me5" "QUESTION PRONOUN: why"
    , newWord "??????" "wen2hua4" "NOUN: culture"
    , newWord "??????" "wen2xue2" "NOUN: literature"
    , newWord "???" "wen4" "VERB: to ask"
    , newWord "??????" "wen4da2" "VERB: to ask and answer questions"
    , newWord "??????" "wen4ti2" "NOUN: question"
    , newWord "???" "wo3" "PRONOUN: I, me"
    , newWord "??????" "wo4shi4" "NOUN: bedroom"
    , newWord "??????" "wu4li3" "NOUN: physics"
    , newWord "??????" "xi1//yan1" "VERB PLUS OBJECT: to smoke"
    , newWord "??????" "xi1can1" "NOUN: Western food"
    , newWord "??????" "xi1fang1" "NOUN: west"
    , newWord "??????" "xi1fu2" "NOUN: Western-style clothes, suit"
    , newWord "?????????" "xi1hong2shi4" "NOUN: tomato"
    , newWord "??????" "xi1yao4" "NOUN: Western medicine"
    , newWord "???" "xi3" "VERB: to wash"
    , newWord "??????" "xi3huan5" "VERB: to like"
    , newWord "??????" "xi3zao3" "VERB PLUS OBJECT: to have (or take) a bath"
    , newWord "???" "xi4" "NOUN: faculty, department"
    , newWord "???" "xia4" "NOUN: below, next"
    , newWord "??????" "xia4//ban1" "VERB PLUS OBJECT: get off work"
    , newWord "??????" "xia4wu3" "NOUN: afternoon"
    , newWord "???" "xian1" "ADVERB: first, before"
    , newWord "??????" "xian1sheng5" "NOUN: Mr., sir"
    , newWord "??????" "xian4jin1" "NOUN: cash"
    , newWord "??????" "xian4zai4" "NOUN: now"
    , newWord "??????" "Xiang1gang3" "PROPER NOUN: Hong Kong"
    , newWord "??????" "xiang1jiao1" "NOUN: banana"
    , newWord "??????" "xiang1xin4" "VERB: believe in, trust"
    , newWord "??????" "xiang1zi5" "NOUN: chest, box, case"
    , newWord "???" "xiang3" "VERB: to think, to want (to do something), to miss"
    , newWord "??????" "xiang4sheng5" "NOUN: comic dialogue, crosstalk"
    , newWord "???" "xiao3" "ADJECTIVE: little, small"
    , newWord "?????????" "xiao3bai2cai4" "NOUN: small Chinese cabbage, pak choi"
    , newWord "??????" "xiao3bian4" "NOUN: urine"
    , newWord "??????" "xiao3fei4" "NOUN: tip, gratuity"
    , newWord "??????" "xiao3jie3" "NOUN: Miss, young lady"
    , newWord "??????" "xiao3shi2" "NOUN: hour"
    , newWord "???" "xie2" "NOUN: shoes"
    , newWord "???" "xie3" "VERB: to write"
    , newWord "???" "xie3" "NOUN: blood"
    , newWord "??????" "xie4xie5" "VERB: to thank"
    , newWord "???" "xin1" "ADJECTIVE: new"
    , newWord "????????????" "xin1guan1bing4du2" "NOUN: Covid-19 virus"
    , newWord "??????" "xin1wen2" "NOUN: news"
    , newWord "???" "xin4" "NOUN: letter"
    , newWord "?????????" "xin4yong4ka3" "NOUN: credit card"
    , newWord "??????" "xing1qi1" "NOUN: week"
    , newWord "???" "xing2" "VERB: to go, to walk, to move"
    , newWord "???" "xing4" "VERB: one's surname is...|NOUN: surname"
    , newWord "??????" "xing4bie2" "NOUN: sex, gender"
    , newWord "??????" "xing4ming2" "NOUN: name"
    , newWord "???" "xiu1" "VERB: to repair"
    , newWord "??????" "xiu1xi5" "VERB: to take a rest"
    , newWord "??????" "xu4jie4" "VERB: to renew (a library book), extend a loan"
    , newWord "??????" "xuan3xiu1" "VERB: to take an elective course"
    , newWord "??????" "xue2sheng5" "NOUN: student"
    , newWord "??????" "xue2xi2" "VERB: to learn, to study"
    , newWord "??????" "xue2yuan4" "NOUN: institute, college"
    , newWord "???" "ya2" "NOUN: tooth"
    , newWord "??????" "yan2se4" "NOUN: color"
    , newWord "??????" "yang4zi5" "NOUN: shape, style, model, pattern"
    , newWord "???" "yao4" "NOUN: medicine"
    , newWord "???" "yao4" "VERB: to want|OPTATIVE VERB: must, to want to do something"
    , newWord "??????" "yao4shi5" "NOUN: key"
    , newWord "??????" "ye2ye5" "NOUN: (paternal) grandfather"
    , newWord "???" "ye3" "ADVERB: too, also"
    , newWord "???" "ye4" "MEASURE WORD: page"
    , newWord "?????????" "yi1dian3r5" "NOUN: a little bit"
    , newWord "??????" "yi1fu5" "NOUN: clothes"
    , newWord "??????" "yi1gong4" "ADVERB: altogether"
    , newWord "??????" "yi1qi3" "ADVERB: together"
    , newWord "??????" "yi1sheng1" "NOUN: doctor, physician"
    , newWord "??????" "yi1xia4" "NOUN: (used after a verb to indicate a short, quick, random, informal action)"
    , newWord "??????" "yi1xie1" "MEASURE WORD: a number of, some"
    , newWord "??????" "yi1yuan4" "NOUN: hospital"
    , newWord "??????" "yi2ding4" "ADVERB: must, surely"
    , newWord "?????????" "yi1hui4r5" "MEASURE WORD: a little while"
    , newWord "??????" "yi3hou4" "NOUN: after (in time)"
    , newWord "??????" "yi3jing1" "ADVERB: already"
    , newWord "??????" "yi3qian2" "NOUN: before (in time)"
    , newWord "??????" "yi3wei2" "VERB: think/believe erroneously"
    , newWord "??????" "yi3zi5" "NOUN: chair"
    , newWord "???" "yi4" "NUMERAL: hundred million"
    , newWord "??????" "yin1yang2" "NOUN: philosophy (in Chinese thought) yin and yang"
    , newWord "??????" "yin1yue4" "NOUN: music"
    , newWord "??????" "yin2hang2" "NOUN: bank"
    , newWord "??????" "ying1bang4" "NOUN: pound, sterling"
    , newWord "??????" "ying1gai1" "OPTATIVE VERB: should, ought to"
    , newWord "??????" "Ying1guo2" "PROPER NOUN: Great Britain"
    , newWord "??????" "Ying1yu3" "NOUN: English"
    , newWord "???" "ying3" "NOUN: shadow"
    , newWord "???" "you2" "VERB: to post, to mail"
    , newWord "??????" "you2ju2" "NOUN: post office"
    , newWord "??????" "you2yong3" "VERB PLUS OBJECT: to swim"
    , newWord "???" "you3" "VERB: to have"
    , newWord "??????" "you3//ming2" "ADJECTIVE: famous"
    , newWord "?????????" "you3dian3r5" "ADVERB: somewhat, a bit"
    , newWord "?????????" "you3yi4si5" "IDIOM EXPRESSION: interesting"
    , newWord "??????" "you3yong4" "ADJECTIVE: useful"
    , newWord "???" "you4" "NOUN: the right side, right"
    , newWord "???" "yu3" "NOUN: language"
    , newWord "??????" "yu3fa3" "NOUN: grammar"
    , newWord "??????" "yu3yan2" "NOUN: language"
    , newWord "??????" "yu4ding4" "VERB: to reserve, to book"
    , newWord "??????" "yuan4yi4" "OPTATIVE VERB: to be willing, to be ready"
    , newWord "?????????" "yue4lan3shi4" "NOUN: reading room"
    , newWord "???" "yun2" "NOUN: cloud"
    , newWord "??????" "za2zhi4" "NOUN: magazine"
    , newWord "???" "zai4" "ADVERB: again"
    , newWord "???" "zai4" "VERB: to be (here, there, in, on, at)|PREPOSITION: at, in, on"
    , newWord "??????" "zai4jian4" "IDIOM EXPRESSION: goodbye"
    , newWord "???" "zang4" "ADJECTIVE: dirty"
    , newWord "??????" "zen3me5" "QUESTION PRONOUN: how"
    , newWord "?????????" "zen3me5yang4" "QUESTION PRONOUN: how is..."
    , newWord "???" "zhang1" "PROPER NOUN: (one of the most common surnames)|MEASURE WORD: flat object(s)"
    , newWord "???" "zhao3" "VERB: to look for"
    , newWord "??????" "zhao4pian4" "NOUN: picture, photo"
    , newWord "??????" "zhe2xue2" "NOUN: philosophy"
    , newWord "???" "zhe4" "PRONOUN: this"
    , newWord "???" "zhen1" "ADJECTIVE: real|ADVERB: really"
    , newWord "??????" "zheng3li3" "VERB: to put in order, to arrange, to sort out"
    , newWord "???" "zheng4" "NOUN: certificate, card"
    , newWord "???" "zhi1" "MEASURE WORD: stick-like things such as pens"
    , newWord "??????" "zhi1dao4" "VERB: to know"
    , newWord "??????" "zhi2ye4" "NOUN: occupation, profession"
    , newWord "??????" "zhong1can1" "NOUN: Chinese food"
    , newWord "??????" "Zhong1guo2" "PROPER NOUN: China"
    , newWord "??????" "zhong1shi4" "ADJECTIVE: Chinese style"
    , newWord "??????" "zhong1wu3" "NOUN: noon"
    , newWord "??????" "zhong1xue2" "NOUN: middle school"
    , newWord "??????" "zhong1yao4" "NOUN: traditional Chinese medicine"
    , newWord "???" "zhong3" "MEASURE WORD: kind, type"
    , newWord "???" "zhu4" "VERB: to live (somewhere), to stay (at)"
    , newWord "???" "zhu4" "VERB: to wish"
    , newWord "??????" "zhu4//yuan4" "VERB PLUS OBJECT: to be hospitalized"
    , newWord "??????" "zhu4he4" "VERB: to congratulate"
    , newWord "??????" "zhu4jiao4" "NOUN: teaching assistant"
    , newWord "??????" "zhuan1jia1" "NOUN: expert"
    , newWord "??????" "zhuan1ye4" "NOUN: major, specialty"
    , newWord "??????" "zhuo1zi5" "NOUN: table"
    , newWord "???" "zi3" "ADJECTIVE: purple"
    , newWord "???" "zi4" "NOUN: character"
    , newWord "??????" "zi4ji3" "PRONOUN: oneself"
    , newWord "??????" "zong3shi4" "ADVERB: always"
    , newWord "???" "zou3" "VERB: to walk, to go"
    , newWord "??????" "zou3ba5" "IDIOM EXPRESSION: Let's go"
    , newWord "??????" "zou3//lu4" "VERB PLUS OBJECT: to walk, to go on foot"
    , newWord "???" "zu1" "NOUN: rent (for a house, flat, etc.)"
    , newWord "??????" "zuo2tian1" "NOUN: yesterday"
    , newWord "???" "zuo3" "NOUN: the left side, left"
    , newWord "???" "zuo4" "VERB: to sit"
    , newWord "???" "zuo4" "VERB: to do, to be, to make"
    , newWord "??????" "zuo4jia1" "NOUN: writer"

    --
    , newWord "??????" "qun2zi5" "NOUN: skirt"
    , newWord "??????" "qing1chu3" "ADJECTIVE: clear, distinct"
    , newWord "??????" "zheng4shi4" "ADJECTIVE: formal"
    , newWord "??????" "guan1men2" "VERB: to close (a door, a shop)"
    , newWord "???" "lei4" "ADJECTIVE: (be) tired, fatigued"
    , newWord "??????" "yu4xi2" "VERB: (of students) prepare lessons before class"
    , newWord "??????" "zhun3bei4" "VERB: prepare, get ready|VERB: intend, plan"
    , newWord "?????????" "hua4hua4r5" "VERB PLUS OBJECT: draw a picture"
    , newWord "??????" "nian2dai4" "NOUN: age, time, date|NOUN: decade of a century: ??????.. = 1980s"
    , newWord "???" "jiu4" "ADJECTIVE: past, bygone, old, used, former"
    , newWord "??????" "cong1ming5" "ADJECTIVE: intelligent, clever"
    , newWord "??????" "wan2le5" "VERB: to finish, to be over"
    , newWord "??????" "shui4yi1" "NOUN: pyjamas, nightclothes"
    , newWord "??????" "nei4yi1" "NOUN: underwear, underclothes"
    , newWord "??????" "nei4ku4" "NOUN: underpants, panties"
    , newWord "??????" "zhi2wu4" "NOUN: post, duties, job"
    , newWord "??????" "ye4wan3" "NOUN: night"
    , newWord "??????" "ye4li3" "NOUN: at night"
    , newWord "???" "xiao4" "VERB: smile, laugh"
    , newWord "??????" "zhou1mo4" "NOUN: the weekend"
    ]
