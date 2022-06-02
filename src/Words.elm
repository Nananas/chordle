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
                |> String.replace "ü" "v"
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
    [ newWord "啊" "a5" "INTERJECTION: Ah, oh"
    , newWord "爱" "ai4" "VERB: to love"
    , newWord "安全" "an1quan2" "NOUN: safety, security"
    , newWord "爸爸" "ba4ba5" "NOUN: dad"
    , newWord "吧" "ba5" "MODAL PARTICLE: (a modal particle)"
    , newWord "白" "bai2" "ADJECTIVE: white"
    , newWord "拜拜" "bai2bai2" "IDIOM EXPRESSION: bye-bye (transliteration)"
    , newWord "白马王子" "bai2ma3wang2zi3" "NOUN: knight in shining armor"
    , newWord "百" "bai3" "NUMERAL: hundred"
    , newWord "办" "ban4" "VERB: to do, to handle"
    , newWord "半" "ban4" "NUMERAL: half"
    , newWord "办法" "ban4fa3" "NOUN: method, way"
    , newWord "办公" "ban4gong1" "VERB PLUS OBJECT: to handle official business, to work (usu. In an office)"
    , newWord "办公室" "ban4gong1shi4" "NOUN: office"
    , newWord "帮助" "bang1zhu4" "VERB: to help"
    , newWord "包裹" "bao1guo3" "NOUN: bundle, package"
    , newWord "包括" "bao1kuo4" "VERB: include"
    , newWord "薄" "bao2" "ADJECTIVE: thin"
    , newWord "抱歉" "bao4qian4" "ADJECTIVE: sorry"
    , newWord "报纸" "bao4zhi3" "NOUN: newspaper"
    , newWord "杯" "bei1" "MEASURE WORD: cups or glasses"
    , newWord "北京" "Bei3jing1" "PROPER NOUN: Beijing"
    , newWord "本" "ben3" "MEASURE WORD: books or notebooks"
    , newWord "本子" "ben3zi5" "NOUN: exercise book, notebook"
    , newWord "笨" "ben4" "ADJECTIVE: stupid, dull"
    , newWord "比" "bi3" "PREPOSITION: then (indicating comparison)"
    , newWord "笔" "bi3" "NOUN: stick-like writing or drawing instruments"
    , newWord "笔记本" "bi3ji4ben3" "NOUN: notebook|NOUN: abbreviation for notebook (computer), laptop"
    , newWord "比利时" "Bi3li4shi2" "PROPER NOUN: Belgium"
    , newWord "遍" "bian4" "MEASURE WORD: (for an action) number of times"
    , newWord "便条" "bian4tiao2" "NOUN: note"
    , newWord "表" "biao3" "NOUN: (wrist)watch|NOUN: form, list"
    , newWord "兵" "bing1" "NOUN: soldier, fighter"
    , newWord "病" "bing4" "NOUN: illness|VERB: to get sick"
    , newWord "不错" "bu2cuo4" "ADJECTIVE: not bad"
    , newWord "不用" "bu2yong4" "ADVERB: need not/no need"
    , newWord "布" "bu4" "NOUN: cloth, fabric"
    , newWord "不" "bu4" "ADVERB: not, no"
    , newWord "步" "bu4" "NOUN: step"
    , newWord "不好意思" "bu4//hao3yi4si5" "IDIOM EXPRESSION: sorry"
    , newWord "不行" "bu4xing2" "VERB: to be no way, to be out of question"
    , newWord "菜" "cai4" "NOUN: dish"
    , newWord "菜单" "cai4dan1" "NOUN: menu"
    , newWord "参观" "can1guan1" "VERB: to visit, have a look around"
    , newWord "参加" "can1jia1" "VERB: to participate, to attend"
    , newWord "餐厅" "can1ting1" "NOUN: dining hall"
    , newWord "厕所" "ce4suo3" "NOUN: toilet"
    , newWord "层" "ceng2" "MEASURE WORD: story, floor"
    , newWord "查" "cha2" "VERB: to check, to look up"
    , newWord "茶" "cha2" "NOUN: tea"
    , newWord "插楼" "cha2lou2" "NOUN: tearoom, tea house"
    , newWord "差" "cha4" "VERB: to be short of, to lack|ADJECTIVE: not up to standard, bad"
    , newWord "长" "chang2" "ADJECTIVE: long"
    , newWord "常" "chang2" "ADVERB: often"
    , newWord "常常" "chang2chang2" "ADVERB: often"
    , newWord "唱歌" "chang4//ge1" "VERB: to sing"
    , newWord "车" "che1" "NOUN: car"
    , newWord "衬衫" "chen4shan1" "NOUN: shirt"
    , newWord "吃" "chi1" "VERB: to eat"
    , newWord "吃饭" "chi1//fan4" "VERB PLUS OBJECT: to eat (a meal)"
    , newWord "吃惊" "chi1//jing1" "VERB PLUS OBJECT: to be startled, to be shocked"
    , newWord "出" "chu1" "VERB: to go or come out"
    , newWord "出去" "chu1qu4" "VERB: to go, to come out"
    , newWord "出生" "chu1sheng1" "VERB: to be born"
    , newWord "厨房" "chu2fang2" "NOUN: kitchen"
    , newWord "穿" "chuan1" "VERB: to wear"
    , newWord "船" "chuan2" "NOUN: boat, ship"
    , newWord "床" "chuang2" "NOUN: bed"
    , newWord "床头柜" "chuang2tou2gui4" "NOUN: bedside cupboard"
    , newWord "词" "ci2" "NOUN: word"
    , newWord "词典" "ci2dian3" "NOUN: dictionary"
    , newWord "从" "cong2" "PREPOSITION: from, as in from... to..."
    , newWord "到" "dao4" "PREPOSITION: to, as in from... to..."
    , newWord "错" "cuo4" "ADJECTIVE: wrong, erroneous"
    , newWord "打" "da3" "VERB: to play"
    , newWord "打包" "da3//bao1" "VERB PLUS OBJECT: to pack"
    , newWord "打的" "da3//di2" "VERB PLUS OBJECT: to hail a taxi"
    , newWord "打工" "da3//gong1" "VERB PLUS OBJECT: to do manual work for somebody, to have a part-time job"
    , newWord "打针" "da3//zhen1" "VERB: to give an injection, to have an injection"
    , newWord "打电话" "da3dian4hua4" "VERB PLUS OBJECT: to make a phone call"
    , newWord "打球" "da3qiu2" "VERB PLUS OBJECT: to play ball games"
    , newWord "打扫" "da3sao3" "VERB: to clean"
    , newWord "打折" "da3zhe2" "VERB PLUS OBJECT: to sell at a discount, to give a discount"
    , newWord "大" "da4" "ADJECTIVE: big, large"
    , newWord "大白菜" "da4bai2cai4" "NOUN: big Chinese cabbage"
    , newWord "大便" "da4bian4" "NOUN: stool, faeces"
    , newWord "带" "dai4" "VERB: to bring"
    , newWord "戴" "dai4" "VERB: put on, wear (e.g. for masks)"
    , newWord "蛋" "dan4" "NOUN: egg"
    , newWord "蛋糕" "dan4gao1" "NOUN: cake"
    , newWord "当然" "dang1ran2" "ADVERB: of course"
    , newWord "到" "dao4" "VERB: to arrive"
    , newWord "德国" "De2guo2" "PROPER NOUN: Germany"
    , newWord "得" "de3" "VERB: to have to, to must, to get"
    , newWord "的" "de5" "STRUCTURAL PARTICLE: (a possessive or modifying particle)"
    , newWord "等" "deng3" "VERB: to wait"
    , newWord "弟弟" "di4di5" "NOUN: younger brother"
    , newWord "地方" "di4fang5" "NOUN: place"
    , newWord "地图" "di4tu2" "NOUN: map"
    , newWord "第一次" "di4yi1ci4" "the first time"
    , newWord "点钟" "dian3zhong1" "MEASURE WORD: o'clock"
    , newWord "店" "dian4" "NOUN: shop, store"
    , newWord "电" "dian4" "NOUN: electricity"
    , newWord "电话" "dian4hua4" "NOUN: telephone, phone call"
    , newWord "电脑" "dian4nao3" "NOUN: computer"
    , newWord "电视" "dian4shi4" "NOUN: TV"
    , newWord "电视剧" "dian4shi4ju4" "NOUN: TV drama (or play)"
    , newWord "电影" "dian4ying3" "NOUN: movie"
    , newWord "电子邮件" "dian4zi3you2jian4" "NOUN: email"
    , newWord "冬奥会" "Dong1ao4hui4" "NOUN: the Winter Olympics"
    , newWord "东西" "dong1xi5" "NOUN: thing, stuff"
    , newWord "懂" "dong3" "VERB: to understand"
    , newWord "都" "dou1" "ADVERB: both, all"
    , newWord "肚子" "du4zi5" "NOUN: belly, abdomen"
    , newWord "短" "duan3" "ADJECTIVE: short"
    , newWord "锻炼" "duan4lian4" "VERB: to exercise, to work out"
    , newWord "对" "dui4" "ADJECTIVE: right correct|PREPOSITION: with regard to, concerning"
    , newWord "队" "dui4" "NOUN: row of people"
    , newWord "对不起" "dui4bu5qi3" "IDIOM EXPRESSION: I'm sorry"
    , newWord "对面" "dui4mian4" "NOUN: opposite to"
    , newWord "多" "duo1" "ADJECTIVE: many, much|ADVERB: how (many/much)"
    , newWord "多大" "duo1da4" "IDIOM EXPRESSION: how old"
    , newWord "多少" "duo1shao5" "QUESTION PRONOUN: how many, how much"
    , newWord "儿子" "er2zi5" "NOUN: son"
    , newWord "发高烧" "fa1gao1shao1" "VERB PLUS OBJECT: to have a *high* fever"
    , newWord "发烧" "fa1shao1" "VERB PLUS OBJECT: to have a fever"
    , newWord "发炎" "fa1yan2" "VERB PLUS OBJECT: to become inflamed"
    , newWord "发展" "fa1zhan3" "VERB: to develop, to advance, to expand, to grow"
    , newWord "罚" "fa2" "VERB: to punish, to penalize"
    , newWord "罚款" "fa2kuan3" "VERB PLUS OBJECT: to impose a fine or forfeit"
    , newWord "法国" "Fa3guo2" "PROPER NOUN: France"
    , newWord "翻译" "fan1yi4" "VERB: to translate, to interpret"
    , newWord "饭" "fan4" "NOUN: meal"
    , newWord "饭店" "fan4dian4" "NOUN: hotel|NOUN: restaurant"
    , newWord "方便" "fang1bian4" "ADJECTIVE: convenient"
    , newWord "房间" "fang2jian1" "NOUN: room"
    , newWord "房子" "fang2zi5" "NOUN: house"
    , newWord "非常" "fei1chang2" "ADVERB: very, extremely, highly"
    , newWord "非法" "fei1fa3" "ADJECTIVE: illegal"
    , newWord "分钱" "fen1qian2" "MEASURE WORD: (Chinese monetary unit, it is equal to 1/100 yuan)"
    , newWord "分钟" "fen1zhong1" "MEASURE WORD: minute"
    , newWord "粉红" "fen3hong2" "ADJECTIVE: pink"
    , newWord "份" "fen4" "MEASURE WORD: publications such as newspapers"
    , newWord "服务" "fu2wu4" "VERB: give service to, to serve"
    , newWord "服务员" "fu2wu4yuan2" "NOUN: attendant"
    , newWord "复活节" "fu4huo2jie2" "PROPER NOUN: Easter"
    , newWord "复习" "fu4xi2" "VERB: to review"
    , newWord "该" "gai1" "OPTATIVE VERB: should, ought to"
    , newWord "改" "gai3" "VERB: change, correct"
    , newWord "感冒" "gan3mao4" "VERB: to have a cold|NOUN: common cold"
    , newWord "刚" "gang1" "ADVERB: just, only a short while ago"
    , newWord "刚才" "gang1cai2" "ADVERB: just now"
    , newWord "高" "gao1" "ADJECTIVE: tall"
    , newWord "高兴" "gao1xing4" "ADJECTIVE: happy, pleased"
    , newWord "告诉" "gao4su5" "VERB: to tell"
    , newWord "哥哥" "ge1ge5" "NOUN: elder brother"
    , newWord "隔离" "ge2li2" "VERB: isolate, separate"
    , newWord "给" "gei3" "VERB: to give|PREPOSITION: to, for"
    , newWord "跟" "gen1" "PREPOSITION: with|VERB: to follow"
    , newWord "公分" "gong1fen1" "NOUN: centimetre"
    , newWord "公斤" "gong1jin1" "MEASURE WORD: kilogram (kg)"
    , newWord "公司" "gong1si1" "NOUN: company"
    , newWord "公元" "gong1yuan2" "NOUN: AD, the Christian era"
    , newWord "公园" "gong1yuan2" "NOUN: park"
    , newWord "工作" "gong1zuo4" "VERB: to work|NOUN: work, job"
    , newWord "共产党" "Gong4chan3dang3" "PROPER NOUN: Communist Party"
    , newWord "狗" "gou3" "NOUN: dog"
    , newWord "姑娘" "gu1niang5" "NOUN: girl"
    , newWord "挂号" "gua4//hao4" "VERB: to register (at a hospital, etc.)"
    , newWord "乖" "guai1" "ADJECTIVE: (of a child) well-behaved, good"
    , newWord "馆" "guan3" "NOUN: term for certain service establishments or places for cultural activities"
    , newWord "光盘" "guang1pan2" "NOUN: CD"
    , newWord "广东" "Guang3dong1" "PROPER NOUN: Guangdong Province"
    , newWord "广告" "guang3gao4" "NOUN: advertisement, ad, commercial"
    , newWord "贵" "gui4" "ADJECTIVE: expensive, precious"
    , newWord "贵姓" "gui4xing4" "IDIOM EXPRESSION: your surname (polite form)"
    , newWord "柜子" "gui4zi5" "NOUN: cupboard, cabinet"
    , newWord "国" "guo2" "NOUN: country, nation"
    , newWord "国籍" "guo2ji2" "NOUN: nationality"
    , newWord "过去" "guo4//qu5" "NOUN: in the past"
    , newWord "过期" "guo4qi1" "VERB PLUS OBJECT: to be overdue"
    , newWord "过" "guo5" "VERB: to spend (time), to celebrate (someone's birthday, a festival, etc.)"
    , newWord "还" "hai2" "ADVERB: in addition, still"
    , newWord "还是" "hai2shi5" "CONJUNCTION: or"
    , newWord "孩子" "hai2zi5" "NOUN: child"
    , newWord "汉语" "Han4yu3" "NOUN: Chinese language"
    , newWord "汉字" "Han4zi4" "NOUN: Chinese character"
    , newWord "好" "hao3" "ADJECTIVE: good well, fine, OK"
    , newWord "好久" "hao3jiu3" "ADJECTIVE: a very long time"
    , newWord "好久不见" "hao3jiu3bu4jian4" "IDIOM EXPRESSION: haven't seen (sb.) for a very long time"
    , newWord "号" "hao4" "NOUN: ordinal number, day of the month"
    , newWord "喝" "he1" "VERB: drink"
    , newWord "和" "he2" "CONJUNCTION: and"
    , newWord "合适" "he2shi4" "ADJECTIVE: suitable"
    , newWord "合影" "he2ying3" "NOUN: group photo (or picture)|VERB PLUS OBJECT: take a group photo (or picture)"
    , newWord "黑" "hei1" "ADJECTIVE: black"
    , newWord "黑客" "hei1ke4" "NOUN: hacker"
    , newWord "很" "hen3" "ADVERB: very"
    , newWord "红" "hong2" "ADJECTIVE: red"
    , newWord "红葡萄酒" "hong2pu2tao5jiu3" "NOUN: red wine"
    , newWord "护士" "hu4shi5" "NOUN: (hospital) nurse"
    , newWord "护照" "hu4zhao4" "NOUN: passport"
    , newWord "花菜" "hua1cai4" "NOUN: broccoli"
    , newWord "花生" "hua1sheng1" "NOUN: peanut"
    , newWord "华侨" "hua2qiao2" "NOUN: overseas Chinese"
    , newWord "华人" "hua2ren2" "NOUN: Chinese people"
    , newWord "话剧" "hua4ju4" "NOUN: modern drama, stage play"
    , newWord "化学" "hua4xue2" "NOUN: chemistry"
    , newWord "换" "huan4" "VERB: to exchange, to change"
    , newWord "黄" "huang2" "ADJECTIVE: yellow"
    , newWord "黄色" "huang2se4" "NOUN: yellow (colou)|ADJECTIVE: decadent, obscene, pornographic"
    , newWord "回" "hui2" "VERB: to return"
    , newWord "回答" "hui2da2" "VERB: to answer"
    , newWord "回信" "hui2xin4" "NOUN: reply letter|VERB PLUS OBJECT: to reply (in writing)"
    , newWord "会" "hui4" "NOUN: meeting|OPTATIVE VERB: can, to have the knowledge of"
    , newWord "货" "huo4" "NOUN: goods"
    , newWord "或者" "huo4zhe3" "CONJUNCTION: or, either (not in a question)"
    , newWord "极了" "ji2le5" "extremely"
    , newWord "几" "ji3" "QUESTION PRONOUN: how many, how much"
    , newWord "寄" "ji4" "VERB: to post, to mail"
    , newWord "记者" "ji4zhe3" "NOUN: reporter"
    , newWord "家" "jia1" "NOUN: family, home"
    , newWord "家常菜" "jia1chang2cai4" "NOUN: home cooking"
    , newWord "假" "jia3" "ADJECTIVE: fake"
    , newWord "间" "jian1" "MEASURE WORD: rooms, houses, etc."
    , newWord "检测" "jian3ce4" "VERB: test, examine, check up (medical)"
    , newWord "件" "jian4" "MEASURE WORD: piece(s)"
    , newWord "交" "jiao1" "VERB: to hand in, to hand over, to pay (the rent, etc.)"
    , newWord "教" "jiao1" "VERB: to teach, to instruct"
    , newWord "叫" "jiao4" "VERB: to be called"
    , newWord "教授" "jiao4shou4" "NOUN: professor"
    , newWord "教育" "jiao4yu4" "NOUN: education"
    , newWord "节" "jie2" "NOUN: festival"
    , newWord "姐姐" "jie3jie5" "NOUN: elder sister"
    , newWord "借" "jie4" "VERB: to borrow"
    , newWord "介绍" "jie4shao4" "VERB: to introduce"
    , newWord "借书证" "jie4shu1zheng4" "NOUN: library card"
    , newWord "斤" "jin1" "MEASURE WORD: weight, around 500 g"
    , newWord "今年" "jin1nian2" "NOUN: this year"
    , newWord "今天" "jin1tian1" "NOUN: today"
    , newWord "进" "jin4" "VERB: to enter"
    , newWord "进来" "jin4//lai5" "VERB PLUS COMPLEMENT: to come in"
    , newWord "经济" "jing1ji4" "NOUN: economics"
    , newWord "京剧" "jing1ju4" "NOUN: Beijing opera"
    , newWord "经理" "jing1li3" "NOUN: manager"
    , newWord "惊喜" "jing1xi3" "NOUN: pleasant surprise"
    , newWord "酒" "jiu3" "NOUN: wine or liquor"
    , newWord "酒店" "jiu3dian4" "NOUN: hotel (often used in the name|NOUN: wineshop; public house 2 hotel [in names of hotels]"
    , newWord "就" "jiu4" "ADVERB: exactly, precisely"
    , newWord "局" "ju2" "NOUN: office, bureau"
    , newWord "橘黄" "ju2huang2" "ADJECTIVE: orange (color)"
    , newWord "桔子" "ju2zi5" "NOUN: tangerine"
    , newWord "聚会" "ju4hui4" "NOUN: party, get-together"
    , newWord "觉得" "jue2de5" "VERB: to feel, to think"
    , newWord "咖啡" "ka1fei1" "NOUN: coffee"
    , newWord "咖啡色" "ka1fei1se4" "NOUN: coffee (color)"
    , newWord "开" "kai1" "VERB: to open, to start"
    , newWord "开车" "kai1//che1" "VERB PLUS OBJECT: to drive a car or a train, etc."
    , newWord "开刀" "kai1//dao1" "VERB PLUS OBJECT: to have an operation"
    , newWord "开学" "kai1//xue2" "VERB PLUS OBJECT: school opens, term begins"
    , newWord "开始" "kai1shi3" "VERB: to start, to begin"
    , newWord "看" "kan4" "VERB: to watch, to look at"
    , newWord "看病" "kan4//bing4" "VERB PLUS OBJECT: to see a doctor"
    , newWord "考" "kao3" "VERB: to give or take an examination, to test"
    , newWord "考试" "kao3shi4" "VERB PLUS OBJECT: to take an examination|NOUN: examination, exam, test"
    , newWord "烤鸭" "kao3ya1" "NOUN: roast duck"
    , newWord "可爱" "ke3ai4" "ADJECTIVE: lovely,cute"
    , newWord "可乐" "ke3le4" "NOUN: coke"
    , newWord "可能" "ke3neng2" "OPTATIVE VERB: maybe"
    , newWord "可是" "ke3shi4" "CONJUNCTION: but"
    , newWord "可以" "ke3yi3" "OPTATIVE VERB: may"
    , newWord "刻" "ke4" "MEASURE WORD: quarter of an hour"
    , newWord "课" "ke4" "NOUN: class, lesson"
    , newWord "课本" "ke4ben3" "NOUN: textbook"
    , newWord "客厅" "ke4ting1" "NOUN: living room"
    , newWord "恐怕" "kong3pa4" "ADVERB: afraid that..."
    , newWord "口" "kou3" "MEASURE WORD: number of people in a family"
    , newWord "口语" "kou3yu3" "NOUN: spoken language"
    , newWord "口罩" "kou3zhao4" "NOUN: surgical mask"
    , newWord "裤子" "ku4zi5" "NOUN: trousers, pants"
    , newWord "快" "kuai4" "ADJECTIVE: fast"
    , newWord "快乐" "kuai4le4" "ADJECTIVE: happy"
    , newWord "款" "kuan3" "NOUN: a sum of money"
    , newWord "来" "lai2" "VERB: to come"
    , newWord "蓝" "lan2" "ADJECTIVE: blue"
    , newWord "老虎" "lao3hu3" "NOUN: tiger"
    , newWord "老人" "lao3ren2" "NOUN: the elderly, the aged, old man or woman"
    , newWord "老师" "lao3shi1" "NOUN: teacher"
    , newWord "冷" "leng3" "ADJECTIVE: cold"
    , newWord "李" "Li3" "PROPER NOUN: (one of the Chinese surnames)"
    , newWord "礼物" "li3wu4" "NOUN: gift,present"
    , newWord "历史" "li4shi3" "NOUN: history"
    , newWord "脸" "lian3" "NOUN: face (of people or animals)"
    , newWord "练" "lian4" "VERB: to practice"
    , newWord "练习" "lian4xi2" "VERB: to practice|NOUN: exercise"
    , newWord "两" "liang3" "NUMERAL: two"
    , newWord "了不起" "liao3bu5qi3" "IDIOM EXPRESSION: amazing, terrific, extraordinary"
    , newWord "料子" "liao4zi5" "NOUN: material for making clothes"
    , newWord "零" "ling2" "NUMERAL: zero"
    , newWord "流利" "liu2li4" "ADJECTIVE: fluent"
    , newWord "留学生" "liu2xue2sheng1" "NOUN: student studying abroad"
    , newWord "楼" "lou2" "NOUN: building, floor, level"
    , newWord "乱" "luan4" "ADJECTIVE: disordered, messy"
    , newWord "旅行" "lü3xing2" "VERB: to travel"
    , newWord "旅游" "lü3you2" "VERB: to travel for tourism"
    , newWord "绿" "lü4" "ADJECTIVE: green"
    , newWord "律师" "lü4shi1" "NOUN: lawyer"
    , newWord "妈妈" "ma1ma5" "NOUN: mom"
    , newWord "麻烦" "ma2fan5" "VERB: to bother somebody, to trouble somebody"
    , newWord "吗" "ma5" "QUESTION PARTICLE: (particle used for a question expecting a yes-no answer)"
    , newWord "买" "mai3" "VERB: to buy"
    , newWord "买单" "mai3dan1" "NOUN: bill, tab"
    , newWord "卖" "mai4" "VERB: to sell"
    , newWord "慢" "man4" "ADJECTIVE: slow"
    , newWord "忙" "mang2" "ADJECTIVE: busy"
    , newWord "毛" "mao2" "MEASURE WORD: (Chinese monetary unit, it is equal to 1/10 yuan)"
    , newWord "没关系" "mei2guan1xi5" "IDIOM EXPRESSION: never mind, it doesn't matter"
    , newWord "每" "mei3" "PRONOUN: every, each"
    , newWord "美" "mei3" "ADJECTIVE: beautiful"
    , newWord "美国" "mei3guo2" "PROPER NOUN: the United States of America"
    , newWord "美女" "mei3nü3" "NOUN: beautiful woman"
    , newWord "美术" "mei3shu4" "NOUN: fine arts"
    , newWord "美元" "mei3yuan2" "NOUN: US dollar"
    , newWord "妹妹" "mei4mei5" "NOUN: younger sister"
    , newWord "们" "men5" "SUFFIX: (used after pronouns 我, 你, 他, 她, 它 or certain nouns to denote plurality)"
    , newWord "米饭" "mi3fan4" "NOUN: (cooked) rice"
    , newWord "密码" "mi4ma3" "NOUN: password, cipher, secret code"
    , newWord "免费" "mian3//fei4" "VERB PLUS OBJECT: free; gratis"
    , newWord "面" "mian4" "NOUN: noodles"
    , newWord "面包" "mian4bao1" "NOUN: bread"
    , newWord "面子" "mian4zi5" "NOUN: face|NOUN: esteem, reputation"
    , newWord "名菜" "ming2cai4" "NOUN: famous dish"
    , newWord "名片" "ming2pian4" "NOUN: business card"
    , newWord "明天" "ming2tian1" "NOUN: tomorrow"
    , newWord "明信片" "ming2xin4pian4" "NOUN: postcard"
    , newWord "名字" "ming2zi5" "NOUN: name"
    , newWord "目录" "mu4lu4" "NOUN: catalog, list"
    , newWord "拿" "na2" "VERB: to take, to hold, to get"
    , newWord "哪" "na3" "QUESTION PRONOUN: which"
    , newWord "哪儿" "na3r5" "QUESTION PRONOUN: where"
    , newWord "那" "na4" "PRONOUN: that"
    , newWord "那儿" "na4r5" "PRONOUN: there"
    , newWord "奶奶" "nai3nai5" "NOUN: (paternal) grandmother"
    , newWord "男" "nan2" "ADJECTIVE: male"
    , newWord "难" "nan2" "ADJECTIVE: difficult"
    , newWord "南边" "nan2bian5" "NOUN: south"
    , newWord "南方" "nan2fang1" "NOUN: south (part of)"
    , newWord "脑" "nao3" "NOUN: brain"
    , newWord "呢" "ne5" "QUESTION PARTICLE:  (a modal particle used for elliptical questions)"
    , newWord "能" "neng2" "OPTATIVE VERB: can, to be able to"
    , newWord "你" "ni3" "PRONOUN: you"
    , newWord "年" "nian2" "NOUN: year"
    , newWord "年龄" "nian2ling2" "NOUN: age"
    , newWord "年轻" "nian2qing1" "ADJECTIVE: young"
    , newWord "念" "nian4" "VERB: to read"
    , newWord "您" "nin2" "PRONOUN: you (polite form)"
    , newWord "牛奶" "niu2nai3" "NOUN: milk"
    , newWord "女" "nü3" "ADJECTIVE: female"
    , newWord "女儿" "nü3er2" "NOUN: daughter"
    , newWord "欧洲" "ou1zhou1" "PROPER NOUN: Europe"
    , newWord "欧元" "Ou5yuan2" "NOUN: Euro"
    , newWord "拍照" "pai1//zhao4" "VERB PLUS OBJECT: to take pictures"
    , newWord "排" "pai2" "VERB: to arrange, to put in order"
    , newWord "排队" "pai2//dui4" "VERB PLUS OBJECT: form a line, to queue up"
    , newWord "旁边" "pang2bian1" "NOUN: next to, side"
    , newWord "朋友" "peng2you5" "NOUN: friend"
    , newWord "啤酒" "pi2jiu3" "NOUN: beer"
    , newWord "便宜" "pian2yi5" "ADJECTIVE: cheap, inexpensive"
    , newWord "漂亮" "piao4liang5" "ADJECTIVE: beautiful, nice"
    , newWord "瓶" "ping2" "MEASURE WORD: bottle(s)"
    , newWord "苹果" "ping2guo3" "NOUN: apple"
    , newWord "普通" "pu3tong1" "ADJECTIVE: common, general"
    , newWord "普通话" "pu3tong1hua4" "NOUN: the common Chinese speech (Mandarin), standard Chinese"
    , newWord "期" "qi1" "NOUN: a period of time"
    , newWord "旗袍" "qi2pao2" "NOUN: cheongsam, a long close-fitting dress with a high neck and slit skirt"
    , newWord "起" "qi3" "VERB: to get up, to rise"
    , newWord "起床" "qi3//chuang2" "VERB PLUS OBJECT: to get up"
    , newWord "千" "qian1" "NUMERAL: thousand"
    , newWord "钱" "qian2" "NOUN: money"
    , newWord "巧" "qiao3" "ADJECTIVE: coincidental"
    , newWord "亲爱" "qin1ai4" "ADJECTIVE: dear"
    , newWord "晴" "qing2" "ADJECTIVE: sunny"
    , newWord "请" "qing3" "VERB: please"
    , newWord "请问" "qing3wen4" "VERB: may I ask"
    , newWord "球" "qiu2" "NOUN: ball"
    , newWord "去" "qu4" "VERB: to go"
    , newWord "去年" "qu4nian2" "NOUN: last year"
    , newWord "全" "quan2" "ADJECTIVE: whole"
    , newWord "全身" "quan2shen1" "NOUN: all over the body"
    , newWord "让" "rang4" "VERB: to let, to allow, to make"
    , newWord "热闹" "re4nao5" "ADJECTIVE: lively, bustling with noise and excitement"
    , newWord "热心" "re4xin1" "ADJECTIVE: warm-hearted"
    , newWord "人" "ren2" "NOUN: people, person"
    , newWord "人民" "ren2min2" "NOUN: people"
    , newWord "人民币" "ren2min2bi4" "NOUN: RMB, Chinese monetary unit"
    , newWord "人员" "ren2yuan2" "NOUN: personnel, staff"
    , newWord "认识" "ren4shi5" "VERB: to know (somebody)"
    , newWord "容易" "rong2yi4" "ADJECTIVE: easy"
    , newWord "肉" "rou4" "NOUN: meat"
    , newWord "散步" "san4//bu4" "VERB PLUS OBJECT: to take a walk, to walk"
    , newWord "嗓子" "sang3zi5" "NOUN: throat"
    , newWord "扫" "sao3" "VERB: to sweep"
    , newWord "沙发" "sha1fa1" "NOUN: sofa"
    , newWord "商" "shang1" "NOUN: trade, commerce"
    , newWord "商场" "shang1chang3" "NOUN: market, shopping mall"
    , newWord "商店" "shang1dian4" "NOUN: shop, store"
    , newWord "上" "shang4" "NOUN: above, last"
    , newWord "上班" "shang4//ban1" "VERB PLUS OBJECT: go to work, start work"
    , newWord "上课" "shang4//ke4" "VERB PLUS OBJECT: to go to class (for both students and teachers)"
    , newWord "上网" "shang4//wang3" "VERB PLUS OBJECT: log onto the Internet, go online"
    , newWord "上海" "shang4hai3" "PROPER NOUN: Shanghai"
    , newWord "上午" "shang4wu3" "NOUN: morning"
    , newWord "烧" "shao1" "VERB: to burn"
    , newWord "少" "shao3" "ADJECTIVE: few, less"
    , newWord "谁" "shei2" "QUESTION PRONOUN: who, whom"
    , newWord "身体" "shen1ti3" "NOUN: body"
    , newWord "什么" "shen2me5" "QUESTION PRONOUN: what"
    , newWord "生" "sheng1" "ADJECTIVE: new|VERB: give birth"
    , newWord "生气" "sheng1//qi4" "VERB PLUS OBJECT: to take offence, get angry"
    , newWord "生词" "sheng1ci2" "NOUN: new word"
    , newWord "生活" "sheng1huo2" "NOUN: life, livelihood"
    , newWord "生日" "sheng1ri4" "NOUN: birthday"
    , newWord "师傅" "shi1fu5" "NOUN: master"
    , newWord "时候" "shi2hou5" "NOUN: time, moment"
    , newWord "时间" "shi2jian1" "NOUN: time"
    , newWord "实用" "shi2yong4" "ADJECTIVE: practical"
    , newWord "式" "shi4" "SUFFIX: (used as a suffix to indicate something or somebody belongs to some type or style)"
    , newWord "室" "shi4" "NOUN: room"
    , newWord "事" "shi4" "NOUN: matter, affair, thing"
    , newWord "手机" "shou3ji1" "NOUN: handset, mobile phone"
    , newWord "售" "shou4" "VERB: to sell"
    , newWord "售货员" "shou4huo4yuan2" "NOUN: shop assistant, salesperson"
    , newWord "寿面" "shou4mian4" "NOUN: (birthday) longevity noodles"
    , newWord "书" "shu1" "NOUN: book"
    , newWord "书店" "shu1dian4" "NOUN: bookstore"
    , newWord "舒服" "shu1fu5" "ADJECTIVE: comfortable, (to feel) well"
    , newWord "书架" "shu1jia4" "NOUN: bookshelf"
    , newWord "书桌" "shu1zhuo1" "NOUN: writing desk"
    , newWord "属" "shu3" "VERB: belong to, to be born in the year of (on of the 12 animals)"
    , newWord "数" "shu3" "VERB: to count"
    , newWord "薯条" "shu3tiao2" "NOUN: french fries"
    , newWord "数学" "shu4xue2" "NOUN: mathematics"
    , newWord "帅" "shuai4" "ADJECTIVE: handsome"
    , newWord "帅哥" "shuai4ge1" "NOUN: handsome young man"
    , newWord "双" "shuang1" "MEASURE WORD: pair"
    , newWord "水" "shui3" "NOUN: water"
    , newWord "水电费" "shui3dian4fei4" "NOUN: charges for water and electricity"
    , newWord "睡觉" "shui4//jiao4" "VERB PLUS OBJECT: to sleep"
    , newWord "说" "shuo1" "VERB: to say, to speak"
    , newWord "丝绸" "si1chou2" "NOUN: silk"
    , newWord "司机" "si1ji1" "NOUN: driver"
    , newWord "四" "si4" "NUMERAL: four"
    , newWord "送" "song4" "NOUN: to give (as a present)"
    , newWord "宿舍" "su4she4" "NOUN: dormitory"
    , newWord "岁" "sui4" "MEASURE WORD: year of age"
    , newWord "岁数" "sui4shu5" "NOUN: years of age"
    , newWord "孙女儿" "sun1nü5r5" "NOUN: granddaughter on son's side"
    , newWord "他" "ta1" "PRONOUN: he, him"
    , newWord "她" "ta1" "PRONOUN: she, her"
    , newWord "太" "tai4" "ADVERB: too, extremely"
    , newWord "太极拳" "tai4ji2quan2" "NOUN: tai chi"
    , newWord "套" "tao4" "MEASURE WORD: set, suit, suite"
    , newWord "疼" "teng2" "ADJECTIVE: painful"
    , newWord "体育馆" "ti3yu4guan3" "NOUN: gym"
    , newWord "天" "tian1" "NOUN: day"
    , newWord "天气" "tian1qi4" "NOUN: weather"
    , newWord "填" "tian2" "VERB: to fill in, to write"
    , newWord "条" "tiao2" "MEASURE WORD: (a measure word for long, narrow objects, such as trousers, skirts, snakes, etc.)"
    , newWord "跳舞" "tiao4//wu3" "VERB PLUS OBJECT: to dance"
    , newWord "听" "ting1" "VERB: to listen"
    , newWord "听说" "ting1shuo1" "VERB: to be told"
    , newWord "同意" "tong2yi4" "VERB: agree, approve"
    , newWord "同志" "tong2zhi4" "NOUN: comrade|NOUN: common name for gay, homosexual"
    , newWord "头" "tou2" "NOUN: head"
    , newWord "头发" "tou2fa5" "NOUN: hair (on the human head)"
    , newWord "图书" "tu2shu1" "NOUN: books"
    , newWord "图书馆" "tu2shu1guan3" "NOUN: library"
    , newWord "土豆" "tu3dou4" "NOUN: potato"
    , newWord "外" "wai4" "NOUN: foreign, outside"
    , newWord "外公" "wai4gong1" "NOUN: (maternal) grandfather"
    , newWord "外国" "wai4guo2" "NOUN: foreign country"
    , newWord "外婆" "wai4po2" "NOUN: (maternal) grandmother"
    , newWord "外语" "wai4yu3" "NOUN: foreign language"
    , newWord "晚" "wan3" "ADJECTIVE: late"
    , newWord "晚上" "wan3shang5" "NOUN: evening, night"
    , newWord "万" "wan4" "NUMERAL: ten thousand"
    , newWord "王府" "wang2fu3" "NOUN: palace of a prince, prince's residence"
    , newWord "网站" "wang3zhan4" "NOUN: website"
    , newWord "忘" "wang4" "VERB: to forget"
    , newWord "位" "wei4" "MEASURE WORD: person(s) (polite)"
    , newWord "喂" "wei4" "INTERJECTION: hello, hey"
    , newWord "为人民服务" "wei4ren2min2fu2wu4" "IDIOM EXPRESSION: Serve the People!, CCP political slogan"
    , newWord "为什么" "wei4shen2me5" "QUESTION PRONOUN: why"
    , newWord "文化" "wen2hua4" "NOUN: culture"
    , newWord "文学" "wen2xue2" "NOUN: literature"
    , newWord "问" "wen4" "VERB: to ask"
    , newWord "问答" "wen4da2" "VERB: to ask and answer questions"
    , newWord "问题" "wen4ti2" "NOUN: question"
    , newWord "我" "wo3" "PRONOUN: I, me"
    , newWord "卧室" "wo4shi4" "NOUN: bedroom"
    , newWord "物理" "wu4li3" "NOUN: physics"
    , newWord "吸烟" "xi1//yan1" "VERB PLUS OBJECT: to smoke"
    , newWord "西餐" "xi1can1" "NOUN: Western food"
    , newWord "西方" "xi1fang1" "NOUN: west"
    , newWord "西服" "xi1fu2" "NOUN: Western-style clothes, suit"
    , newWord "西红柿" "xi1hong2shi4" "NOUN: tomato"
    , newWord "西药" "xi1yao4" "NOUN: Western medicine"
    , newWord "洗" "xi3" "VERB: to wash"
    , newWord "喜欢" "xi3huan5" "VERB: to like"
    , newWord "洗澡" "xi3zao3" "VERB PLUS OBJECT: to have (or take) a bath"
    , newWord "系" "xi4" "NOUN: faculty, department"
    , newWord "下" "xia4" "NOUN: below, next"
    , newWord "下班" "xia4//ban1" "VERB PLUS OBJECT: get off work"
    , newWord "下午" "xia4wu3" "NOUN: afternoon"
    , newWord "先" "xian1" "ADVERB: first, before"
    , newWord "先生" "xian1sheng5" "NOUN: Mr., sir"
    , newWord "现金" "xian4jin1" "NOUN: cash"
    , newWord "现在" "xian4zai4" "NOUN: now"
    , newWord "香港" "Xiang1gang3" "PROPER NOUN: Hong Kong"
    , newWord "香蕉" "xiang1jiao1" "NOUN: banana"
    , newWord "相信" "xiang1xin4" "VERB: believe in, trust"
    , newWord "箱子" "xiang1zi5" "NOUN: chest, box, case"
    , newWord "想" "xiang3" "VERB: to think, to want (to do something), to miss"
    , newWord "相声" "xiang4sheng5" "NOUN: comic dialogue, crosstalk"
    , newWord "小" "xiao3" "ADJECTIVE: little, small"
    , newWord "小白菜" "xiao3bai2cai4" "NOUN: small Chinese cabbage, pak choi"
    , newWord "小便" "xiao3bian4" "NOUN: urine"
    , newWord "小费" "xiao3fei4" "NOUN: tip, gratuity"
    , newWord "小姐" "xiao3jie3" "NOUN: Miss, young lady"
    , newWord "小时" "xiao3shi2" "NOUN: hour"
    , newWord "鞋" "xie2" "NOUN: shoes"
    , newWord "写" "xie3" "VERB: to write"
    , newWord "血" "xie3" "NOUN: blood"
    , newWord "谢谢" "xie4xie5" "VERB: to thank"
    , newWord "新" "xin1" "ADJECTIVE: new"
    , newWord "新冠病毒" "xin1guan1bing4du2" "NOUN: Covid-19 virus"
    , newWord "新闻" "xin1wen2" "NOUN: news"
    , newWord "信" "xin4" "NOUN: letter"
    , newWord "信用卡" "xin4yong4ka3" "NOUN: credit card"
    , newWord "星期" "xing1qi1" "NOUN: week"
    , newWord "行" "xing2" "VERB: to go, to walk, to move"
    , newWord "姓" "xing4" "VERB: one's surname is...|NOUN: surname"
    , newWord "性别" "xing4bie2" "NOUN: sex, gender"
    , newWord "姓名" "xing4ming2" "NOUN: name"
    , newWord "修" "xiu1" "VERB: to repair"
    , newWord "休息" "xiu1xi5" "VERB: to take a rest"
    , newWord "续借" "xu4jie4" "VERB: to renew (a library book), extend a loan"
    , newWord "选修" "xuan3xiu1" "VERB: to take an elective course"
    , newWord "学生" "xue2sheng5" "NOUN: student"
    , newWord "学习" "xue2xi2" "VERB: to learn, to study"
    , newWord "学院" "xue2yuan4" "NOUN: institute, college"
    , newWord "牙" "ya2" "NOUN: tooth"
    , newWord "颜色" "yan2se4" "NOUN: color"
    , newWord "样子" "yang4zi5" "NOUN: shape, style, model, pattern"
    , newWord "药" "yao4" "NOUN: medicine"
    , newWord "要" "yao4" "VERB: to want|OPTATIVE VERB: must, to want to do something"
    , newWord "钥匙" "yao4shi5" "NOUN: key"
    , newWord "爷爷" "ye2ye5" "NOUN: (paternal) grandfather"
    , newWord "也" "ye3" "ADVERB: too, also"
    , newWord "页" "ye4" "MEASURE WORD: page"
    , newWord "一点儿" "yi1dian3r5" "NOUN: a little bit"
    , newWord "衣服" "yi1fu5" "NOUN: clothes"
    , newWord "一共" "yi1gong4" "ADVERB: altogether"
    , newWord "一起" "yi1qi3" "ADVERB: together"
    , newWord "医生" "yi1sheng1" "NOUN: doctor, physician"
    , newWord "一下" "yi1xia4" "NOUN: (used after a verb to indicate a short, quick, random, informal action)"
    , newWord "一些" "yi1xie1" "MEASURE WORD: a number of, some"
    , newWord "医院" "yi1yuan4" "NOUN: hospital"
    , newWord "一定" "yi2ding4" "ADVERB: must, surely"
    , newWord "一会儿" "yi1hui4r5" "MEASURE WORD: a little while"
    , newWord "以后" "yi3hou4" "NOUN: after (in time)"
    , newWord "已经" "yi3jing1" "ADVERB: already"
    , newWord "以前" "yi3qian2" "NOUN: before (in time)"
    , newWord "以为" "yi3wei2" "VERB: think/believe erroneously"
    , newWord "椅子" "yi3zi5" "NOUN: chair"
    , newWord "亿" "yi4" "NUMERAL: hundred million"
    , newWord "阴阳" "yin1yang2" "NOUN: philosophy (in Chinese thought) yin and yang"
    , newWord "音乐" "yin1yue4" "NOUN: music"
    , newWord "银行" "yin2hang2" "NOUN: bank"
    , newWord "英镑" "ying1bang4" "NOUN: pound, sterling"
    , newWord "应该" "ying1gai1" "OPTATIVE VERB: should, ought to"
    , newWord "英国" "Ying1guo2" "PROPER NOUN: Great Britain"
    , newWord "英语" "Ying1yu3" "NOUN: English"
    , newWord "影" "ying3" "NOUN: shadow"
    , newWord "邮" "you2" "VERB: to post, to mail"
    , newWord "邮局" "you2ju2" "NOUN: post office"
    , newWord "游泳" "you2yong3" "VERB PLUS OBJECT: to swim"
    , newWord "有" "you3" "VERB: to have"
    , newWord "有名" "you3//ming2" "ADJECTIVE: famous"
    , newWord "有点儿" "you3dian3r5" "ADVERB: somewhat, a bit"
    , newWord "有意思" "you3yi4si5" "IDIOM EXPRESSION: interesting"
    , newWord "有用" "you3yong4" "ADJECTIVE: useful"
    , newWord "友" "you4" "NOUN: the right side, right"
    , newWord "语" "yu3" "NOUN: language"
    , newWord "语法" "yu3fa3" "NOUN: grammar"
    , newWord "语言" "yu3yan2" "NOUN: language"
    , newWord "预订" "yu4ding4" "VERB: to reserve, to book"
    , newWord "愿意" "yuan4yi4" "OPTATIVE VERB: to be willing, to be ready"
    , newWord "阅览室" "yue4lan3shi4" "NOUN: reading room"
    , newWord "云" "yun2" "NOUN: cloud"
    , newWord "杂志" "za2zhi4" "NOUN: magazine"
    , newWord "再" "zai4" "ADVERB: again"
    , newWord "在" "zai4" "VERB: to be (here, there, in, on, at)|PREPOSITION: at, in, on"
    , newWord "再见" "zai4jian4" "IDIOM EXPRESSION: goodbye"
    , newWord "脏" "zang4" "ADJECTIVE: dirty"
    , newWord "怎么" "zen3me5" "QUESTION PRONOUN: how"
    , newWord "怎么样" "zen3me5yang4" "QUESTION PRONOUN: how is..."
    , newWord "张" "zhang1" "PROPER NOUN: (one of the most common surnames)|MEASURE WORD: flat object(s)"
    , newWord "找" "zhao3" "VERB: to look for"
    , newWord "照片" "zhao4pian4" "NOUN: picture, photo"
    , newWord "哲学" "zhe2xue2" "NOUN: philosophy"
    , newWord "这" "zhe4" "PRONOUN: this"
    , newWord "真" "zhen1" "ADJECTIVE: real|ADVERB: really"
    , newWord "整理" "zheng3li3" "VERB: to put in order, to arrange, to sort out"
    , newWord "正" "zheng4" "NOUN: certificate, card"
    , newWord "支" "zhi1" "MEASURE WORD: stick-like things such as pens"
    , newWord "知道" "zhi1dao4" "VERB: to know"
    , newWord "职业" "zhi2ye4" "NOUN: occupation, profession"
    , newWord "中餐" "zhong1can1" "NOUN: Chinese food"
    , newWord "中国" "Zhong1guo2" "PROPER NOUN: China"
    , newWord "中式" "zhong1shi4" "ADJECTIVE: Chinese style"
    , newWord "中午" "zhong1wu3" "NOUN: noon"
    , newWord "中学" "zhong1xue2" "NOUN: middle school"
    , newWord "中药" "zhong1yao4" "NOUN: traditional Chinese medicine"
    , newWord "种" "zhong3" "MEASURE WORD: kind, type"
    , newWord "住" "zhu4" "VERB: to live (somewhere), to stay (at)"
    , newWord "祝" "zhu4" "VERB: to wish"
    , newWord "住院" "zhu4//yuan4" "VERB PLUS OBJECT: to be hospitalized"
    , newWord "祝贺" "zhu4he4" "VERB: to congratulate"
    , newWord "助教" "zhu4jiao4" "NOUN: teaching assistant"
    , newWord "专家" "zhuan1jia1" "NOUN: expert"
    , newWord "专业" "zhuan1ye4" "NOUN: major, specialty"
    , newWord "桌子" "zhuo1zi5" "NOUN: table"
    , newWord "紫" "zi3" "ADJECTIVE: purple"
    , newWord "字" "zi4" "NOUN: character"
    , newWord "自己" "zi4ji3" "PRONOUN: oneself"
    , newWord "总是" "zong3shi4" "ADVERB: always"
    , newWord "走" "zou3" "VERB: to walk, to go"
    , newWord "走吧" "zou3ba5" "IDIOM EXPRESSION: Let's go"
    , newWord "走路" "zou3//lu4" "VERB PLUS OBJECT: to walk, to go on foot"
    , newWord "租" "zu1" "NOUN: rent (for a house, flat, etc.)"
    , newWord "昨天" "zuo2tian1" "NOUN: yesterday"
    , newWord "左" "zuo3" "NOUN: the left side, left"
    , newWord "坐" "zuo4" "VERB: to sit"
    , newWord "做" "zuo4" "VERB: to do, to be, to make"
    , newWord "作家" "zuo4jia1" "NOUN: writer"

    --
    , newWord "裙子" "qun2zi5" "NOUN: skirt"
    , newWord "清楚" "qing1chu3" "ADJECTIVE: clear, distinct"
    , newWord "正式" "zheng4shi4" "ADJECTIVE: formal"
    , newWord "关门" "guan1men2" "VERB: to close (a door, a shop)"
    , newWord "累" "lei4" "ADJECTIVE: (be) tired, fatigued"
    , newWord "预习" "yu4xi2" "VERB: (of students) prepare lessons before class"
    , newWord "准备" "zhun3bei4" "VERB: prepare, get ready|VERB: intend, plan"
    , newWord "画画儿" "hua4hua4r5" "VERB PLUS OBJECT: draw a picture"
    , newWord "年代" "nian2dai4" "NOUN: age, time, date|NOUN: decade of a century: 八十.. = 1980s"
    , newWord "旧" "jiu4" "ADJECTIVE: past, bygone, old, used, former"
    , newWord "聪明" "cong1ming5" "ADJECTIVE: intelligent, clever"
    , newWord "完了" "wan2le5" "VERB: to finish, to be over"
    , newWord "睡衣" "shui4yi1" "NOUN: pyjamas, nightclothes"
    , newWord "内衣" "nei4yi1" "NOUN: underwear, underclothes"
    , newWord "内裤" "nei4ku4" "NOUN: underpants, panties"
    , newWord "职务" "zhi2wu4" "NOUN: post, duties, job"
    , newWord "夜晚" "ye4wan3" "NOUN: night"
    , newWord "夜里" "ye4li3" "NOUN: at night"
    , newWord "笑" "xiao4" "VERB: smile, laugh"
    , newWord "周末" "zhou1mo4" "NOUN: the weekend"
    ]
