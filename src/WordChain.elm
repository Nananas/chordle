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
import Words exposing (..)


type alias WordChain =
    List Word


isPinyinValid : PinyinPart -> WordChain -> Bool
isPinyinValid pinyinPart wordChain =
    wordChain
        |> List.find
            (\word ->
                List.find (\character -> pinyinPartsSimilarity character.pinyinPart pinyinPart == CompletelySimilar) word.characters
                    |> Maybe.map (\_ -> True)
                    |> Maybe.withDefault False
            )
        |> Maybe.map (\_ -> True)
        |> Maybe.withDefault False


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

        --oneSimilarWordGenerator : List Word -> Word -> Random.Generator Word
        --oneSimilarWordGenerator fromList similarTo =
        --    similarWords similarTo fromList
        --        |> Random.List.choose
        --        |> Random.map (Tuple.first >> Maybe.withDefault emptyWord)
        go wordList similarWord acc count =
            if count <= 0 then
                acc

            else
                similarWords similarWord wordList
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
                    |> Random.map (\( selected, _ ) -> startWord :: selected)
            )


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

        one : List Word -> Random.Generator ( List Word, List Word )
        one dict =
            Random.List.choose dict
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


viewSingleHanzi : ViewSingleHanziOptions msg -> Element msg
viewSingleHanzi { state, showPopup, onMouseEnterCharacterMsg, onMouseLeaveCharacterMsg, wordId, id, character } =
    let
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
                    px 50

         --    Known
         --if known then
         --    px 50
         --else if similar then
         --    shrink
         --else
         , height <| px 50
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
        el [ centerX, centerY, Element.Font.size 20 ] <|
            text <|
                case state of
                    Similar ->
                        character.pinyinPart.pinyin

                    Unknown ->
                        ""

                    _ ->
                        character.hanzi
