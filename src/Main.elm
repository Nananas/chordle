module Main exposing (..)

import Browser
import Browser.Dom
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import List.Extra as List
import Process
import Random
import Random.List
import Storage
import String.Extra as String
import Task
import UI
import Words exposing (..)


idNextButton =
    "next-btn"


idInput =
    "text-input"


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--


type Msg
    = StorageLoaded Storage.Storage
    | NewWordChain WordChain
    | InputHanzi String
    | UserPressedEnter
    | ToNextWord
    | NoOp
    | OnGiveUpClicked
    | OnRestartDictionaryClick
      --
    | OnMouseEnterCharacter ( Int, Int )
    | OnMouseLeaveCharacter



--| MODEL


type Session
    = LoadingStorage
    | Initializing (List Word)
    | Ready Model


type alias WordChain =
    List Word


type alias Model =
    { dictionary : List Word

    --
    , wordChain : List Word
    , answers : List PinyinPart
    , currentInput : String
    , gameState : GameState

    --
    , showPopupForCharacter : Maybe ( Int, Int )
    }


type GameState
    = NotDone
    | TooManyWrongAnswers
    | FilledInCorrectly



--


init : () -> ( Session, Cmd Msg )
init _ =
    ( LoadingStorage
    , Cmd.batch
        [ Process.sleep 100
            |> Task.andThen (\_ -> Browser.Dom.focus idInput)
            |> Task.attempt (\_ -> NoOp)
        , Storage.loadStorage "dictionary"
        ]
    )



--


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    case ( session, msg ) of
        ( LoadingStorage, StorageLoaded storage ) ->
            let
                words : List String
                words =
                    storage.json
                        |> Json.Decode.decodeValue (Json.Decode.list Json.Decode.string)
                        |> Result.toMaybe
                        |> Maybe.withDefault (allWords |> List.map (\word -> word.characters |> List.map .hanzi |> String.join ""))

                dictionary =
                    allWords
                        |> List.filter
                            (\word ->
                                List.member
                                    (word.characters
                                        |> List.map .hanzi
                                        |> String.join ""
                                    )
                                    words
                            )
            in
            ( Initializing dictionary, Random.generate NewWordChain (wordChainGenerator dictionary) )

        ( Initializing dictionary, NewWordChain wordChain ) ->
            ( Ready
                { dictionary = wordChain |> List.foldl (\w d -> List.remove w d) dictionary
                , wordChain = wordChain
                , answers = []
                , currentInput = ""
                , gameState = NotDone
                , showPopupForCharacter = Nothing
                }
            , Storage.setStorage
                { name = "dictionary"
                , json =
                    Json.Encode.list
                        (\word ->
                            word.characters
                                |> List.map .hanzi
                                |> String.join ""
                                |> Json.Encode.string
                        )
                        dictionary
                }
            )

        ( Ready model, NewWordChain wordChain ) ->
            let
                dictionary =
                    wordChain |> List.foldl (\w d -> List.remove w d) model.dictionary
            in
            ( Ready
                { dictionary = dictionary
                , wordChain = wordChain
                , answers = []
                , currentInput = ""
                , gameState = NotDone
                , showPopupForCharacter = Nothing
                }
            , Storage.setStorage
                { name = "dictionary"
                , json =
                    Json.Encode.list
                        (\word ->
                            word.characters
                                |> List.map .hanzi
                                |> String.join ""
                                |> Json.Encode.string
                        )
                        dictionary
                }
            )

        ( Ready model, InputHanzi txt ) ->
            ( Ready
                { model | currentInput = txt }
            , Cmd.none
            )

        ( Ready model, UserPressedEnter ) ->
            let
                txt =
                    String.toCodePoints model.currentInput
                        |> List.filter (\code -> code <= 126)
                        |> String.fromCodePoints

                ( newModel, cmd ) =
                    processInput { model | currentInput = txt }
                        |> processGameFinished
            in
            ( Ready { newModel | currentInput = "" }
            , cmd
            )

        ( Ready model, ToNextWord ) ->
            ( session
            , Cmd.batch
                [ Random.generate NewWordChain (wordChainGenerator model.dictionary)
                , Browser.Dom.focus idInput
                    |> Task.attempt (\_ -> NoOp)
                ]
            )

        ( Ready model, OnGiveUpClicked ) ->
            ( Ready { model | gameState = TooManyWrongAnswers }, Cmd.none )

        ( Ready model, OnRestartDictionaryClick ) ->
            ( Ready { model | gameState = NotDone, dictionary = allWords }
            , Cmd.batch
                [ Random.generate NewWordChain (wordChainGenerator allWords)
                , Browser.Dom.focus idInput
                    |> Task.attempt (\_ -> NoOp)
                ]
            )

        --
        ( Ready model, OnMouseEnterCharacter id ) ->
            ( Ready { model | showPopupForCharacter = Just id }, Cmd.none )

        ( Ready model, OnMouseLeaveCharacter ) ->
            ( Ready { model | showPopupForCharacter = Nothing }, Cmd.none )

        --
        ( LoadingStorage, _ ) ->
            ( session, Cmd.none )

        ( Initializing _, _ ) ->
            ( session, Cmd.none )

        ( Ready _, NoOp ) ->
            ( session, Cmd.none )

        ( Ready _, StorageLoaded _ ) ->
            ( session, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Session -> Sub Msg
subscriptions _ =
    Storage.storageLoaded StorageLoaded



-- VIEW


view : Session -> Html.Html Msg
view session =
    layout [ width fill, height fill ] <|
        case session of
            Ready model ->
                column [ centerX, centerY, spacing 50 ]
                    [ row [ centerX, spacing 20 ]
                        [ UI.niceText <| String.fromInt <| List.length model.dictionary
                        , UI.niceButton "Restart" OnRestartDictionaryClick Nothing
                        ]
                    , column [ spacing 20 ]
                        (List.indexedMap
                            (\wordId word ->
                                row [ width <| px 600, spacing 20 ]
                                    [ el [] <| viewWordAnswers model wordId word
                                    , el [ alignLeft ] <| viewWordEnglish word
                                    ]
                            )
                            model.wordChain
                        )
                    , viewWrongAnwers <| allWrongAnswers model
                    , el [ centerX ] <| UI.niceButton "I give up, show me the answers" OnGiveUpClicked Nothing
                    , case model.gameState of
                        NotDone ->
                            viewInput model

                        _ ->
                            UI.niceButtonWith [ centerX, htmlAttribute <| Html.Attributes.id idNextButton ] "Next" ToNextWord Nothing
                    ]

            _ ->
                UI.spinner


viewWordAnswers : Model -> Int -> Word -> Element Msg
viewWordAnswers model wordId word =
    word.characters
        |> List.indexedMap (viewSingleHanzi model wordId)
        |> row [ spacing 10 ]


viewWordEnglish word =
    word.english
        |> String.split "|"
        |> List.map text
        |> row [ spacing 5, alignLeft ]


viewSingleHanzi : Model -> Int -> Int -> Character -> Element Msg
viewSingleHanzi model wordId id character =
    let
        popup =
            case model.showPopupForCharacter of
                Nothing ->
                    none

                Just ( wid, i ) ->
                    if wid == wordId && id == i then
                        column [ centerX ]
                            [ el [ height <| px 10 ] none
                            , el
                                [ Background.color UI.white
                                , padding 10
                                , centerX
                                , Font.color UI.gray
                                , UI.floatingHigh
                                , Border.rounded 10
                                , Font.size 16
                                , Font.regular
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
         , width
            (shrink
                |> minimum 50
            )
         , height <| px 50
         , Font.color <|
            case model.gameState of
                NotDone ->
                    UI.black

                _ ->
                    toneToColor character.pinyinPart.tone
         , Font.medium
         , Background.color <|
            case model.gameState of
                NotDone ->
                    if isCharacterKnown model character then
                        rgb255 152 226 172

                    else if isCharacterSimilar model character then
                        rgb255 240 230 110

                    else
                        rgb 1 1 1

                _ ->
                    -- TODO: tones
                    rgb 1 1 1
         , Border.color <|
            case model.gameState of
                NotDone ->
                    rgba 0 0 0 0

                _ ->
                    if isCharacterKnown model character then
                        rgb255 152 226 172

                    else
                        rgb255 230 125 125
         , Border.width 2
         , below popup
         ]
            ++ (case model.gameState of
                    NotDone ->
                        []

                    _ ->
                        [ Element.Events.onMouseEnter (OnMouseEnterCharacter ( wordId, id ))
                        , Element.Events.onMouseLeave OnMouseLeaveCharacter
                        ]
               )
        )
    <|
        el [ centerX, centerY, Font.size 20 ] <|
            text <|
                case model.gameState of
                    NotDone ->
                        if isCharacterKnown model character then
                            character.hanzi

                        else if isCharacterSimilar model character then
                            character.pinyinPart.pinyin

                        else
                            ""

                    _ ->
                        character.hanzi


toneToColor tone =
    case tone of
        First ->
            rgb255 210 210 100

        Second ->
            rgb255 120 220 140

        Third ->
            rgb255 125 125 240

        Forth ->
            rgb255 210 125 240

        Fifth ->
            rgb255 150 150 150


viewInput : Model -> Element Msg
viewInput model =
    Input.text
        [ onEnter UserPressedEnter
        , htmlAttribute <| Html.Attributes.id idInput
        ]
        { onChange = InputHanzi
        , text = model.currentInput
        , placeholder = Nothing
        , label = Input.labelHidden ""
        }


viewWrongAnwers : List PinyinPart -> Element Msg
viewWrongAnwers wrongAnswers =
    wrongAnswers
        |> List.map
            (\part ->
                el
                    [ UI.floating
                    , UI.rounded 5
                    , height <| px 50
                    , paddingXY 10 0
                    , Background.color <|
                        rgb255 226 226 226
                    ]
                <|
                    el [ centerX, centerY, Font.size 20 ] <|
                        text (formatPinyin part)
            )
        |> row [ spacing 10, height <| px 50, centerX ]



--


wordChainGenerator : List Word -> Random.Generator WordChain
wordChainGenerator dictionary =
    let
        startWordGenerator : Random.Generator Word
        startWordGenerator =
            Random.List.choose dictionary
                |> Random.map (Tuple.first >> Maybe.withDefault emptyWord)

        dictionaryWithoutWord : List Word -> Word -> List Word
        dictionaryWithoutWord dict word =
            List.remove word dict

        oneSimilarWordGenerator : List Word -> Word -> Random.Generator Word
        oneSimilarWordGenerator fromList similarTo =
            similarWords similarTo fromList
                |> Random.List.choose
                |> Random.map (Tuple.first >> Maybe.withDefault emptyWord)

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
        (Random.int 2 4)
        |> Random.andThen
            (\( wordList, startWord, wordCount ) ->
                Random.List.choices wordCount wordList
                    |> Random.map (\( selected, _ ) -> startWord :: selected)
            )


processInput : Model -> Model
processInput model =
    case
        model.currentInput
            |> splitStringIntoPinyin
    of
        Ok pinyinParts ->
            { model | answers = pinyinParts ++ model.answers |> List.unique }

        Err err ->
            model


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


isWordFullyKnown : Model -> Word -> Bool
isWordFullyKnown model word =
    word.characters
        |> List.filter (\character -> not <| List.member character.pinyinPart model.answers)
        |> List.length
        |> (==) 0


isCharacterKnown : Model -> Character -> Bool
isCharacterKnown model character =
    List.member character.pinyinPart model.answers


isCharacterSimilar : Model -> Character -> Bool
isCharacterSimilar model character =
    List.any (\part -> part.pinyin == character.pinyinPart.pinyin) model.answers


allWrongAnswers : Model -> List PinyinPart
allWrongAnswers model =
    model.answers
        |> List.filter (\part -> not <| isPinyinValid part model.wordChain)


processGameFinished : Model -> ( Model, Cmd Msg )
processGameFinished model =
    let
        focusNextBtn =
            Browser.Dom.focus idNextButton
                |> Task.attempt (\_ -> NoOp)
    in
    if List.length (allWrongAnswers model) > 5 then
        ( { model | gameState = TooManyWrongAnswers }, Cmd.none )

    else
        let
            finished =
                model.wordChain
                    |> List.foldl
                        (\word acc -> isWordFullyKnown model word && acc)
                        True
        in
        if finished then
            ( { model | gameState = FilledInCorrectly }, focusNextBtn )

        else
            ( { model | gameState = NotDone }, focusNextBtn )



--


onEnter : msg -> Element.Attribute msg
onEnter msg =
    htmlAttribute
        (Html.Events.on "keyup"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.Decode.succeed msg

                        else
                            Json.Decode.fail "Not the enter key"
                    )
            )
        )
