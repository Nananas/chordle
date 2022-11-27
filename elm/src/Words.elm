module Words exposing (..)

-- ^(\S*)\s*(\S*)\s*(.*)
-- ,Word "$1" "$2" "$3"

import Dict exposing (Dict)
import Json.Decode
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


type alias Dictionary =
    List Word


type alias Dictionaries =
    Dict String Dictionary


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

        FifthOr other ->
            toneToString other


pinyinPartsSimilarity : PinyinPart -> PinyinPart -> PinyinPartsSimilarity
pinyinPartsSimilarity p1 p2 =
    if p1 == p2 then
        CompletelySimilar

    else if p1.pinyin == p2.pinyin then
        if Tones.isMatchingTones p1.tone p2.tone then
            CompletelySimilar

        else
            PinyinSimilar

    else if Tones.isMatchingTones p1.tone p2.tone then
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


similarWordsList : List Word -> List Word -> List Word
similarWordsList words wordList =
    let
        hanziPartsSet =
            words
                |> List.map .characters
                |> List.concat
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


singleWordsList : List Word -> List Word
singleWordsList wordList =
    let
        hanziSet =
            wordList
                |> List.map .characters
                |> List.concat
                |> List.map .hanzi
                |> Set.fromList

        recurse acc words =
            case words of
                [] ->
                    acc

                head :: tail ->
                    if
                        head.characters
                            |> List.any (\ch -> Set.member ch.hanzi hanziSet)
                    then
                        recurse (head :: acc) tail

                    else
                        recurse acc tail
    in
    recurse [] wordList


splitStringIntoPinyin : String -> Result String (List PinyinPart)
splitStringIntoPinyin str =
    let
        recurse : List Char -> List PinyinPart -> List Char -> ( List Char, List PinyinPart )
        recurse acc parts charsLeft =
            case charsLeft of
                [] ->
                    ( acc, parts )

                [ one ] ->
                    case ( charToTone one, acc ) of
                        ( Just _, [] ) ->
                            ( [ one ], parts )

                        ( Nothing, _ ) ->
                            ( one :: acc, parts )

                        ( Just someTone, _ ) ->
                            ( [], { pinyin = String.fromList <| List.reverse <| acc, tone = someTone } :: parts )

                one :: two :: tail ->
                    case ( charToTone one, charToTone two, acc ) of
                        ( Just _, _, [] ) ->
                            recurse [] parts (two :: tail)

                        ( Just Fifth, Nothing, _ ) ->
                            recurse [] ({ pinyin = String.fromList <| List.reverse <| acc, tone = Fifth } :: parts) (two :: tail)

                        ( Just Fifth, Just Fifth, _ ) ->
                            recurse [] ({ pinyin = String.fromList <| List.reverse <| acc, tone = Fifth } :: parts) tail

                        ( Just Fifth, Just someTone, _ ) ->
                            recurse [] ({ pinyin = String.fromList <| List.reverse <| acc, tone = FifthOr someTone } :: parts) tail

                        ( Just someTone, _, _ ) ->
                            recurse [] ({ pinyin = String.fromList <| List.reverse <| acc, tone = someTone } :: parts) (two :: tail)

                        _ ->
                            recurse (one :: acc) parts (two :: tail)

        result =
            str
                |> String.replace "ü" "v"
                |> String.replace "//" ""
                |> String.toLower
                |> String.toList
                |> List.filter Char.isAlphaNum
                |> recurse [] []
    in
    case result of
        ( [], [] ) ->
            Err "Incorrect format, should be pinyin followed by a tone number (12345)"

        ( [], parts ) ->
            Ok (List.reverse parts)

        ( any, _ ) ->
            Err ("Incorrect format, should be pinyin followed by a tone number (12345), but got " ++ String.fromList (List.reverse any))


formatPinyin : PinyinPart -> String
formatPinyin pinyinPart =
    pinyinPart.pinyin
        ++ toneToString pinyinPart.tone
        |> Tones.replace


isCharacterKnown : Character -> List PinyinPart -> Bool
isCharacterKnown character parts =
    case
        parts
            |> List.find
                (\{ pinyin, tone } ->
                    pinyin
                        == character.pinyinPart.pinyin
                        && Tones.isMatchingTones tone character.pinyinPart.tone
                )
    of
        Nothing ->
            False

        Just _ ->
            True


isCharacterSimilar : Character -> List PinyinPart -> Bool
isCharacterSimilar character parts =
    List.any (\part -> part.pinyin == character.pinyinPart.pinyin) parts


isWordFullyKnown : Word -> List PinyinPart -> Bool
isWordFullyKnown word parts =
    word.characters
        |> List.filter (\character -> not <| isCharacterKnown character parts)
        |> List.length
        |> (==) 0



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


allWords : Dict String Bool -> Dictionaries -> List Word
allWords dictsActive dictionaries =
    dictionaries
        |> Dict.toList
        |> List.filter (\( name, _ ) -> Dict.get name dictsActive |> Maybe.withDefault True)
        |> List.map Tuple.second
        |> List.concat


allDictNames dictionaries =
    dictionaries
        |> Dict.keys


wordsFileDecoder =
    let
        wordDecoder =
            Json.Decode.andThen
                (\el ->
                    case el of
                        hanzi :: pinyin :: english :: _ ->
                            Json.Decode.succeed (newWord hanzi pinyin english)

                        _ ->
                            Json.Decode.fail "Bad word format"
                )
                (Json.Decode.list Json.Decode.string)

        dictDecoder =
            Json.Decode.dict (Json.Decode.list wordDecoder)
    in
    Json.Decode.field "dictionaries" dictDecoder


wordsFromJson str =
    Json.Decode.decodeString wordsFileDecoder str
        |> Result.withDefault Dict.empty
