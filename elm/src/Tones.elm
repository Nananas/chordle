module Tones exposing (..)

import Element exposing (Color, rgb255)


type Tone
    = First
    | Second
    | Third
    | Forth
    | Fifth
    | FifthOr Tone


type Colors
    = NotChosen
    | ChordleColors
    | PlecoColors
    | NoColors


type alias CustomColors =
    { first : Color, second : Color, third : Color, forth : Color, fifth : Color }


chordleColors : CustomColors
chordleColors =
    { first = rgb255 210 210 100
    , second = rgb255 120 220 140
    , third = rgb255 125 125 240
    , forth = rgb255 210 125 240
    , fifth = rgb255 150 150 150
    }


plecoColors : CustomColors
plecoColors =
    { first = rgb255 210 10 10
    , second = rgb255 10 180 30
    , third = rgb255 25 20 240
    , forth = rgb255 170 60 180
    , fifth = rgb255 120 120 120
    }


toneToColor : Colors -> Tone -> Color
toneToColor toneColors tone =
    let
        pickColor colors t =
            case t of
                First ->
                    colors.first

                Second ->
                    colors.second

                Third ->
                    colors.third

                Forth ->
                    colors.forth

                Fifth ->
                    colors.fifth

                FifthOr other ->
                    pickColor colors other
    in
    case toneColors of
        ChordleColors ->
            pickColor chordleColors tone

        PlecoColors ->
            pickColor plecoColors tone

        NoColors ->
            rgb255 30 30 30

        NotChosen ->
            pickColor chordleColors tone


stringToColors str =
    case str of
        "pleco" ->
            PlecoColors

        "chordle" ->
            ChordleColors

        "black" ->
            NoColors

        _ ->
            NotChosen


colorsToString toneColors =
    case toneColors of
        PlecoColors ->
            Just "pleco"

        ChordleColors ->
            Just "chordle"

        NoColors ->
            Just "black"

        NotChosen ->
            Nothing


isMatchingTones t1 t2 =
    case ( t1, t2 ) of
        ( FifthOr a, FifthOr b ) ->
            a == b

        ( FifthOr _, Fifth ) ->
            True

        ( Fifth, FifthOr _ ) ->
            True

        ( FifthOr x, y ) ->
            x == y

        ( y, FifthOr x ) ->
            x == y

        _ ->
            t1 == t2


replace : String -> String
replace str =
    List.foldl (\toneReplacer s -> toneReplacer s) str toneReplacers


toneReplacers =
    [ String.replace "a5" "a"
    , String.replace "e5" "e"
    , String.replace "i5" "i"
    , String.replace "o5" "o"
    , String.replace "u5" "u"
    , String.replace "ü5" "ü"
    , String.replace "a1" "ā"
    , String.replace "a2" "á"
    , String.replace "a3" "ǎ"
    , String.replace "a4" "à"
    , String.replace "e1" "ē"
    , String.replace "e2" "é"
    , String.replace "e3" "ě"
    , String.replace "e4" "è"
    , String.replace "i1" "ī"
    , String.replace "i2" "í"
    , String.replace "i3" "ǐ"
    , String.replace "i4" "ì"
    , String.replace "o1" "ō"
    , String.replace "o2" "ó"
    , String.replace "o3" "ǒ"
    , String.replace "o4" "ò"
    , String.replace "u1" "ū"
    , String.replace "u2" "ú"
    , String.replace "u3" "ǔ"
    , String.replace "u4" "ù"
    , String.replace "ü1" "ǖ"
    , String.replace "ü2" "ǘ"
    , String.replace "ü3" "ǚ"
    , String.replace "ü4" "ǜ"
    , String.replace "v1" "ǖ"
    , String.replace "v2" "ǘ"
    , String.replace "v3" "ǚ"
    , String.replace "v4" "ǜ"
    , String.replace "an1" "ān"
    , String.replace "an2" "án"
    , String.replace "an3" "ǎn"
    , String.replace "an4" "àn"
    , String.replace "an5" "an"
    , String.replace "ang1" "āng"
    , String.replace "ang2" "áng"
    , String.replace "ang3" "ǎng"
    , String.replace "ang4" "àng"
    , String.replace "ang5" "ang"
    , String.replace "en1" "ēn"
    , String.replace "en2" "én"
    , String.replace "en3" "ěn"
    , String.replace "en4" "èn"
    , String.replace "en5" "en"
    , String.replace "eng1" "ēng"
    , String.replace "eng2" "éng"
    , String.replace "eng3" "ěng"
    , String.replace "eng4" "èng"
    , String.replace "eng5" "eng"
    , String.replace "in1" "īn"
    , String.replace "in2" "ín"
    , String.replace "in3" "ǐn"
    , String.replace "in4" "ìn"
    , String.replace "in5" "in"
    , String.replace "ing1" "īng"
    , String.replace "ing2" "íng"
    , String.replace "ing3" "ǐng"
    , String.replace "ing4" "ìng"
    , String.replace "ong1" "ōng"
    , String.replace "ong2" "óng"
    , String.replace "ong3" "ǒng"
    , String.replace "ong4" "òng"
    , String.replace "ong5" "ong"
    , String.replace "un1" "ūn"
    , String.replace "un2" "ún"
    , String.replace "un3" "ǔn"
    , String.replace "un4" "ùn"
    , String.replace "un5" "un"
    , String.replace "er2" "ér"
    , String.replace "er3" "ěr"
    , String.replace "er4" "èr"
    , String.replace "er5" "er"
    , String.replace "aō" "āo"
    , String.replace "aó" "áo"
    , String.replace "aǒ" "ǎo"
    , String.replace "aò" "ào"
    , String.replace "oū" "ōu"
    , String.replace "oú" "óu"
    , String.replace "oǔ" "ǒu"
    , String.replace "où" "òu"
    , String.replace "aī" "āi"
    , String.replace "aí" "ái"
    , String.replace "aǐ" "ǎi"
    , String.replace "aì" "ài"
    , String.replace "eī" "ēi"
    , String.replace "eí" "éi"
    , String.replace "eǐ" "ěi"
    , String.replace "eì" "èi"
    ]
