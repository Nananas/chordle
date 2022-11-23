module WordChain exposing (..)

import Element exposing (..)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import List.Extra as List
import Random
import Random.List
import Tones
import UI
import Utils
import Words exposing (..)


type alias WordChain =
    List ( Word, Int )


isPinyinValid : PinyinPart -> WordChain -> Bool
isPinyinValid pinyinPart wordChain =
    wordChain
        |> List.find
            (\( word, _ ) ->
                List.find (\character -> pinyinPartsSimilarity character.pinyinPart pinyinPart == CompletelySimilar) word.characters
                    |> Maybe.map (\_ -> True)
                    |> Maybe.withDefault False
            )
        |> Maybe.map (\_ -> True)
        |> Maybe.withDefault False


isPinyinSimilar : PinyinPart -> WordChain -> Bool
isPinyinSimilar pinyinPart wordChain =
    wordChain
        |> List.find
            (\( word, _ ) ->
                List.find (\character -> pinyinPartsSimilarity character.pinyinPart pinyinPart == PinyinSimilar) word.characters
                    |> Maybe.map (\_ -> True)
                    |> Maybe.withDefault False
            )
        |> Maybe.map (\_ -> True)
        |> Maybe.withDefault False


wrongAnswersOf : WordChain -> List PinyinPart -> List PinyinPart
wrongAnswersOf wordChain parts =
    parts
        |> List.filter (\part -> not <| isPinyinValid part wordChain)



-- RANDOM


singleChainGenerator : ( Int, Int ) -> List Word -> Random.Generator WordChain
singleChainGenerator ( from, to ) dictionary =
    let
        startWordGenerator : Random.Generator Word
        startWordGenerator =
            Random.List.choose dictionary
                |> Random.map (Tuple.first >> Maybe.withDefault emptyWord)

        dictionaryWithoutWord : List Word -> Word -> List Word
        dictionaryWithoutWord dict word =
            List.remove word dict
    in
    Random.map2
        (\startWord wordCount ->
            ( dictionaryWithoutWord dictionary startWord
                |> similarWords startWord
            , startWord
            , wordCount
            )
        )
        startWordGenerator
        (Random.int from to)
        |> Random.andThen
            (\( wordList, startWord, wordCount ) ->
                Random.List.choices wordCount wordList
                    |> Random.map
                        (\( selected, _ ) ->
                            startWord
                                :: selected
                                |> List.map (\w -> ( w, 0 ))
                        )
            )


excludedHanzi =
    [ "子", "中", "学" ]


multiChainGenerator : Int -> List Word -> Random.Generator WordChain
multiChainGenerator maximumAmount dictionary =
    let
        randomSimilarWordFrom : List Word -> List Word -> Random.Generator ( Maybe Word, List Word )
        randomSimilarWordFrom words dict =
            similarWordsList words dict
                |> Random.List.choose

        dictionaryWithoutWord : List Word -> Word -> List Word
        dictionaryWithoutWord dict word =
            List.remove word dict

        isExcluded word =
            word.characters
                |> List.any (\ch -> List.member ch.hanzi excludedHanzi)

        one : List Word -> Random.Generator ( List Word, List Word )
        one dict =
            dict
                -- Exclude some very common characters from the initial search word
                |> List.filter (not << isExcluded)
                |> Random.List.choose
                |> Random.map
                    (\( mWord, dictWithout ) ->
                        case mWord of
                            Nothing ->
                                ( [], dictWithout )

                            Just word ->
                                ( [ word ], dictWithout )
                    )

        recurse : Int -> ( List Word, List Word ) -> Random.Generator ( List Word, List Word )
        recurse limit ( acc, dict ) =
            if limit < 0 then
                Random.constant ( acc, dict )

            else if List.length acc > maximumAmount then
                Random.constant ( acc, dict )

            else
                similarWordsList acc dict
                    |> Random.List.choose
                    |> Random.andThen
                        (\( mNewWord, _ ) ->
                            case mNewWord of
                                Nothing ->
                                    one dict
                                        |> Random.andThen (recurse (limit - 1))

                                Just newWord ->
                                    recurse (limit - 1) ( newWord :: acc, dictionaryWithoutWord dict newWord )
                        )
    in
    one dictionary
        |> Random.andThen (recurse 20)
        |> Random.map Tuple.first
        |> Random.map offsetWords
        |> Random.map sortWords


offsetWords : List Word -> List ( Word, Int, Hanzi )
offsetWords words =
    let
        hasOffsetWith : Word -> ( Word, Int, Hanzi ) -> Maybe ( Word, Int, Hanzi )
        hasOffsetWith word ( other, otherIndex, _ ) =
            word.characters
                |> List.indexedFoldl
                    (\charIndex { hanzi } mResult ->
                        case mResult of
                            Nothing ->
                                Utils.indexedFind (\oChar -> oChar.hanzi == hanzi) other.characters
                                    |> Maybe.map (\( i, e ) -> ( word, i + otherIndex - charIndex, e.hanzi ))

                            _ ->
                                mResult
                    )
                    Nothing

        shiftedWords =
            words
                |> List.foldr
                    (\word acc ->
                        case acc of
                            [] ->
                                [ ( word, 0, List.map .hanzi word.characters |> String.concat ) ]

                            _ ->
                                (List.findMap (hasOffsetWith word) acc
                                    |> Maybe.withDefault ( word, List.length acc * 10, List.map .hanzi word.characters |> String.concat )
                                )
                                    :: acc
                    )
                    []

        minimumIndex =
            shiftedWords
                |> List.map (\( _, i, _ ) -> i)
                |> List.minimum
                |> Maybe.withDefault 0
    in
    shiftedWords
        |> List.map (\( w, i, h ) -> ( w, i - minimumIndex, h ))


sortWords : List ( Word, Int, Hanzi ) -> List ( Word, Int )
sortWords words =
    let
        comp : ( Word, Int, Hanzi ) -> ( Word, Int, Hanzi ) -> Order
        comp ( w1, i1, h1 ) ( w2, i2, h2 ) =
            compare h1 h2
    in
    words
        |> List.sortWith comp
        |> List.map (\( w, i, _ ) -> ( w, i ))



-- VIEW


type CharacterState
    = Known -- e.g. when guessed correctly but game is still ongoing
    | Similar
    | Unknown
    | Show Bool -- e.g. when game is done, bool = correct/incorrect


type alias ViewSingleHanziOptions msg =
    { state : CharacterState
    , showPopup : Maybe ( Int, Int )
    , onMouseEnterCharacterMsg : ( Int, Int ) -> msg
    , onMouseLeaveCharacterMsg : msg
    , wordId : Int
    , id : Int
    , character : Character
    }


viewSingleHanzi : Bool -> ViewSingleHanziOptions msg -> Element msg
viewSingleHanzi onMobile { state, showPopup, onMouseEnterCharacterMsg, onMouseLeaveCharacterMsg, wordId, id, character } =
    let
        fontsize =
            if onMobile then
                16

            else
                20

        size =
            if onMobile then
                40

            else
                50

        popup =
            case showPopup of
                Nothing ->
                    none

                Just ( wid, i ) ->
                    if wid == wordId && id == i then
                        column [ centerX ]
                            [ el [ height <| px 10 ] none
                            , el
                                [ Element.Background.color UI.white
                                , padding 10
                                , centerX
                                , Element.Font.color UI.gray
                                , UI.floatingHigh
                                , Element.Border.rounded 10
                                , Element.Font.size 16
                                , Element.Font.regular
                                ]
                              <|
                                text <|
                                    formatPinyin character.pinyinPart
                            ]

                    else
                        none
    in
    el
        ([ UI.floating
         , UI.rounded 5
         , paddingXY 20 0

         --, width <| maximum 50 shrink
         , width <|
            case state of
                Similar ->
                    shrink

                _ ->
                    px size

         --    Known
         --if known then
         --    px 50
         --else if similar then
         --    shrink
         --else
         , height <| px size
         , Element.Font.color <|
            case state of
                Show _ ->
                    Tones.toneToColor character.pinyinPart.tone

                _ ->
                    UI.black
         , Element.Font.medium
         , Element.Background.color <|
            case state of
                Known ->
                    rgb255 152 226 172

                Similar ->
                    rgb255 240 230 110

                _ ->
                    rgb 1 1 1
         , Element.Border.color <|
            case state of
                Show correct ->
                    if correct then
                        rgb255 152 226 172

                    else
                        rgb255 230 125 125

                _ ->
                    rgba 0 0 0 0
         , Element.Border.width 2
         , below popup
         ]
            ++ (case state of
                    Show _ ->
                        [ Element.Events.onMouseEnter (onMouseEnterCharacterMsg ( wordId, id ))
                        , Element.Events.onMouseLeave onMouseLeaveCharacterMsg
                        ]

                    _ ->
                        []
               )
        )
    <|
        el [ centerX, centerY, Element.Font.size fontsize ] <|
            text <|
                case state of
                    Similar ->
                        character.pinyinPart.pinyin

                    Unknown ->
                        ""

                    _ ->
                        character.hanzi
