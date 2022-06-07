module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Help
import Html
import Html.Attributes
import Html.Events
import Icons
import Json.Decode
import Json.Encode
import List.Extra as List
import MobileUI
import Notifications
import Process
import Random
import Random.List
import Storage
import String.Extra as String
import Task
import Time
import Tones exposing (..)
import UI
import Words exposing (..)


inactivityTime =
    30 * 60 * 1000


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
    = OnGetViewport Browser.Dom.Viewport
    | OnResizeViewport Int Int
    | StorageLoaded Storage.Storage
    | NewWordChain WordChain
    | InputHanzi String
    | UserPressedEnter
    | ToNextWord
    | OnGiveUpClicked
    | OnRestartDictionaryClick
    | OnToggleHelp
    | OnToggleScreenKeyboard
    | OnToggleDictionaryModal
    | OnToggleDictionaryShowAllWords
      --
    | OnMouseEnterCharacter ( Int, Int )
    | OnMouseLeaveCharacter
    | OnTick Time.Posix
    | OnNotificationPermissionChanged String
    | OnToggleNotifications
      --
    | OnActivity Time.Posix
      --
    | NoOp
    | NoOpString String
    | KeyboardInput String
    | KeyboardBackspace
    | KeyboardClear



--| MODEL


type Session
    = LoadingDeviceType
    | LoadingStorage Device
    | Initializing (List Word) Device
    | Ready Model


type Device
    = Mobile
    | Desktop


type alias WordChain =
    List Word


type alias Model =
    { device : Device

    --
    , dictionary : List Word

    --
    , wordChain : List Word
    , answers : List PinyinPart
    , currentInput : String
    , gameState : GameState

    --
    , showPopupForCharacter : Maybe ( Int, Int )
    , showTodoUpdate : Maybe Int
    , showTodoUpdateTimer : Int
    , showHelp : Bool
    , showDictionaryModal : Bool
    , unhideDictionary : Bool

    --
    , useScreenKeyboardOnMobile : Bool
    , keyboardKeyFeedback : Maybe String

    --
    , notificationsEnabled : Maybe Bool
    , lastActivity : Time.Posix
    }


type GameState
    = NotDone
    | TooManyWrongAnswers
    | FilledInCorrectly



--


init : () -> ( Session, Cmd Msg )
init _ =
    ( LoadingDeviceType
    , Browser.Dom.getViewport
        |> Task.perform OnGetViewport
    )



--


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    case ( session, msg ) of
        ( LoadingDeviceType, OnGetViewport { viewport } ) ->
            let
                device =
                    if viewport.width < 1200 then
                        Mobile

                    else
                        Desktop
            in
            ( LoadingStorage device, Storage.loadStorage "dictionary" )

        ( Ready model, OnResizeViewport width height ) ->
            let
                device =
                    if width < 1200 then
                        Mobile

                    else
                        Desktop
            in
            ( Ready { model | device = device }, Cmd.none )

        ( LoadingStorage device, StorageLoaded storage ) ->
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
            ( Initializing dictionary device, Random.generate NewWordChain (wordChainGenerator dictionary) )

        ( Initializing dictionary device, NewWordChain wordChain ) ->
            ( Ready
                { device = device
                , dictionary = wordChain |> List.foldl (\w d -> List.remove w d) dictionary
                , wordChain = wordChain
                , answers = []
                , currentInput = ""
                , gameState = NotDone
                , showPopupForCharacter = Nothing
                , showTodoUpdate = Nothing
                , showTodoUpdateTimer = 0
                , showHelp = False
                , showDictionaryModal = False
                , unhideDictionary = False
                , notificationsEnabled = Nothing
                , lastActivity = Time.millisToPosix 0
                , useScreenKeyboardOnMobile = True
                , keyboardKeyFeedback = Nothing
                }
            , Cmd.batch
                [ Storage.setStorage
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
                , Process.sleep 100
                    |> Task.andThen (\_ -> Browser.Dom.focus idInput)
                    |> Task.attempt (\_ -> NoOp)
                ]
            )

        ( Ready model, OnToggleNotifications ) ->
            case model.notificationsEnabled of
                Nothing ->
                    ( Ready { model | notificationsEnabled = Just False }, Notifications.requestPermissions () )

                Just enabled ->
                    ( Ready { model | notificationsEnabled = Just <| not enabled }, Cmd.none )

        ( Ready model, OnNotificationPermissionChanged "granted" ) ->
            case model.notificationsEnabled of
                Nothing ->
                    ( Ready { model | notificationsEnabled = Just False }, Cmd.none )

                Just _ ->
                    ( Ready { model | notificationsEnabled = Just True }, onActivity )

        ( Ready model, OnActivity now ) ->
            ( Ready { model | lastActivity = now }, Cmd.none )

        ( Ready model, NewWordChain wordChain ) ->
            let
                dictionary =
                    wordChain |> List.foldl (\w d -> List.remove w d) model.dictionary
            in
            ( Ready
                { model
                    | dictionary = dictionary
                    , wordChain = wordChain
                    , answers = []
                    , currentInput = ""
                    , gameState = NotDone
                    , showPopupForCharacter = Nothing
                    , showTodoUpdate = Nothing
                    , showTodoUpdateTimer = 0
                    , showHelp = False
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

        ( Ready model, KeyboardInput char ) ->
            ( Ready { model | currentInput = model.currentInput ++ char }, Cmd.none )

        ( Ready model, KeyboardBackspace ) ->
            ( Ready { model | currentInput = String.dropRight 1 model.currentInput }, Cmd.none )

        ( Ready model, KeyboardClear ) ->
            ( Ready { model | currentInput = "" }, Cmd.none )

        ( Ready model, OnToggleScreenKeyboard ) ->
            ( Ready { model | useScreenKeyboardOnMobile = not model.useScreenKeyboardOnMobile }, Cmd.none )

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
            ( Ready model
            , Cmd.batch
                [ Random.generate NewWordChain (wordChainGenerator model.dictionary)
                , Browser.Dom.focus idInput
                    |> Task.attempt (\_ -> NoOp)
                ]
            )

        ( Ready model, OnGiveUpClicked ) ->
            ( Ready
                { model
                    | gameState = TooManyWrongAnswers
                    , dictionary = model.wordChain ++ model.dictionary |> List.unique
                    , showTodoUpdate = Just (List.length model.wordChain)
                    , showTodoUpdateTimer = 2
                }
            , Cmd.none
            )

        ( Ready model, OnRestartDictionaryClick ) ->
            ( Ready { model | gameState = NotDone, dictionary = allWords }
            , Cmd.batch
                [ Random.generate NewWordChain (wordChainGenerator allWords)
                , Browser.Dom.focus idInput
                    |> Task.attempt (\_ -> NoOp)
                ]
            )

        ( Ready model, OnToggleHelp ) ->
            ( Ready { model | showHelp = not model.showHelp }, Cmd.none )

        ( Ready model, OnToggleDictionaryModal ) ->
            ( Ready { model | showDictionaryModal = not model.showDictionaryModal }, Cmd.none )

        ( Ready model, OnToggleDictionaryShowAllWords ) ->
            ( Ready { model | unhideDictionary = not model.unhideDictionary }, Cmd.none )

        --
        ( Ready model, OnMouseEnterCharacter id ) ->
            ( Ready { model | showPopupForCharacter = Just id }, Cmd.none )

        ( Ready model, OnMouseLeaveCharacter ) ->
            ( Ready { model | showPopupForCharacter = Nothing }, Cmd.none )

        ( Ready model, OnTick now ) ->
            let
                withUpdateShowTodo m =
                    if m.showTodoUpdateTimer > 0 then
                        { m | showTodoUpdateTimer = m.showTodoUpdateTimer - 1 }

                    else
                        { m | showTodoUpdate = Nothing }

                withUpdateNotifications m =
                    case m.notificationsEnabled of
                        Just True ->
                            if (Time.posixToMillis now - Time.posixToMillis m.lastActivity) > inactivityTime then
                                ( { m | lastActivity = now }, Notifications.showNotification "Time for a CHORDLE!" )

                            else
                                ( m, Cmd.none )

                        _ ->
                            ( m, Cmd.none )
            in
            model
                |> withUpdateShowTodo
                |> withUpdateNotifications
                |> liftModel Ready

        --
        ( LoadingDeviceType, _ ) ->
            ( session, Cmd.none )

        ( LoadingStorage _, _ ) ->
            ( session, Cmd.none )

        ( Initializing _ _, _ ) ->
            ( session, Cmd.none )

        ( Ready _, _ ) ->
            ( session, Cmd.none )


onActivity =
    Task.perform OnActivity Time.now


liftModel fn ( model, cmd ) =
    ( fn model, cmd )



-- SUBSCRIPTIONS


subscriptions : Session -> Sub Msg
subscriptions session =
    Sub.batch
        [ Storage.storageLoaded StorageLoaded
        , case session of
            Ready _ ->
                Time.every 1000 OnTick

            _ ->
                Sub.none
        , Notifications.permissionChanged OnNotificationPermissionChanged
        , Browser.Events.onResize OnResizeViewport
        ]



-- VIEW


view : Session -> Html.Html Msg
view session =
    layoutWith { options = [ focusStyle { backgroundColor = Nothing, borderColor = Nothing, shadow = Nothing } ] } [ width fill, height fill ] <|
        case session of
            Ready model ->
                case model.device of
                    Mobile ->
                        viewMobile model

                    Desktop ->
                        viewDesktop model

            _ ->
                UI.spinner


viewMobile model =
    let
        modals =
            el
                [ width fill
                , height fill

                --, inFront <|
                --    if model.showDictionaryModal then
                --        UI.modal OnToggleDictionaryModal
                --            [ el [] <| UI.heading "Dictionary"
                --            , model.dictionary
                --                |> List.map (\word -> word.characters |> List.map .hanzi |> String.join "," |> text)
                --                |> column [ scrollbarY ]
                --            ]
                --    else
                --        none
                ]
            <|
                Help.viewMobile model.showHelp OnToggleHelp NoOpString
    in
    column [ width fill, height fill ]
        [ mobileViewTopBar model
        , row [ width fill, height fill, inFront modals ]
            [ el [ height fill, width fill ] <|
                column [ width fill, height fill, spacing 20, paddingXY 0 10 ]
                    [ column [ width fill, height shrink, spacing 20, UI.floating, padding 10 ]
                        (List.indexedMap
                            (\wordId word ->
                                row [ width fill, spacing 20 ]
                                    [ el [ width <| fillPortion 1 ] <| viewWordAnswers model wordId word
                                    , el [ width <| fillPortion 1 ] <| viewWordEnglishMobile word
                                    ]
                            )
                            model.wordChain
                        )
                    , viewWrongAnwers <| allWrongAnswers model
                    , case model.gameState of
                        NotDone ->
                            el [ centerX ] <| UI.niceButton "I give up, show me the answers" OnGiveUpClicked Nothing

                        _ ->
                            el [ centerX, height <| px 40 ] none
                    , case model.gameState of
                        NotDone ->
                            viewInput model

                        _ ->
                            UI.niceButtonWith [ centerX, htmlAttribute <| Html.Attributes.id idNextButton ] "Next" ToNextWord Nothing
                    , if model.useScreenKeyboardOnMobile then
                        el [ alignBottom, width fill, height <| maximum 300 fill ] <| MobileUI.viewKeyboard KeyboardInput KeyboardBackspace KeyboardClear

                      else
                        none
                    ]
            ]
        ]


viewDesktop model =
    let
        modals =
            row [ width fill, height fill ]
                [ if model.showDictionaryModal then
                    UI.modal OnToggleDictionaryModal
                        [ row [ width fill ]
                            [ el [ alignLeft ] <| UI.heading "Dictionary"
                            , el [ alignRight ] <|
                                UI.niceButton
                                    (if model.unhideDictionary then
                                        "Hide all words"

                                     else
                                        "Show all words"
                                    )
                                    OnToggleDictionaryShowAllWords
                                    (Just <|
                                        if model.unhideDictionary then
                                            Icons.eyeOff 20

                                        else
                                            Icons.eye 20
                                    )
                            ]
                        , Words.allWords
                            |> List.map
                                (\word ->
                                    Words.wordToStringParts word
                                        |> (\( hanzi, pinyin, english ) ->
                                                let
                                                    wordAlreadyDone =
                                                        not <| List.member word model.dictionary

                                                    maybeHide str =
                                                        if model.unhideDictionary || wordAlreadyDone then
                                                            str

                                                        else
                                                            "."
                                                in
                                                row
                                                    [ width fill
                                                    , spacing 20
                                                    , Font.color <|
                                                        if wordAlreadyDone then
                                                            UI.correctColor

                                                        else
                                                            UI.black
                                                    ]
                                                    [ paragraph [ width <| fillPortion 1, Font.size 24, Font.alignRight ] [ text hanzi ]
                                                    , paragraph [ width <| fillPortion 2, Font.size 16, Font.center ] [ text <| maybeHide pinyin ]
                                                    , paragraph [ width <| fillPortion 3, Font.size 16 ] [ text <| maybeHide english ]
                                                    ]
                                           )
                                )
                            |> column [ scrollbarY, height fill, width fill, padding 10, spacing 10 ]
                        ]

                  else
                    none
                , Help.viewDesktop model.showHelp OnToggleHelp NoOpString
                ]
    in
    column [ width fill, height fill ]
        [ viewTopBar model
        , row [ width fill, height fill, inFront modals ]
            [ el [ height fill, width <| fillPortion 1 ] none
            , el [ height fill, width <| fillPortion 5 ] <|
                column [ centerX, centerY, spacing 50 ]
                    [ column [ spacing 20, UI.floating, padding 40 ]
                        (List.indexedMap
                            (\wordId word ->
                                row [ width <| px 600, spacing 20 ]
                                    [ el [ width <| fillPortion 1 ] <| viewWordAnswers model wordId word
                                    , el [ width <| fillPortion 1 ] <| viewWordEnglish word
                                    ]
                            )
                            model.wordChain
                        )
                    , viewWrongAnwers <| allWrongAnswers model
                    , case model.gameState of
                        NotDone ->
                            el [ centerX ] <| UI.niceButton "I give up, show me the answers" OnGiveUpClicked Nothing

                        _ ->
                            el [ centerX, height <| px 40 ] none
                    , case model.gameState of
                        NotDone ->
                            viewInput model

                        _ ->
                            UI.niceButtonWith [ centerX, htmlAttribute <| Html.Attributes.id idNextButton ] "Next" ToNextWord Nothing
                    ]
            , el [ height fill, width <| fillPortion 1 ] none
            ]
        ]


viewLogo =
    el [ width fill, height fill ] <|
        el [ centerX, centerY, Font.color UI.white, Font.bold ] <|
            text "Chordle"


viewTopBar : Model -> Element Msg
viewTopBar model =
    row [ height <| px 50, width fill, Background.color UI.accentColor, paddingXY 20 0, spacing 20, behindContent viewLogo ]
        [ el
            [ alignLeft
            , onRight <|
                el [] <|
                    case model.showTodoUpdate of
                        Nothing ->
                            text <| ""

                        Just u ->
                            el
                                [ Font.color <|
                                    if u < 0 then
                                        UI.correctColor

                                    else
                                        UI.errorColor
                                , Font.bold
                                ]
                            <|
                                text <|
                                    "  "
                                        ++ (if u > 0 then
                                                "+"

                                            else
                                                ""
                                           )
                                        ++ String.fromInt u
            ]
          <|
            UI.niceTextWith [ Font.color UI.white ] <|
                "Words to go: "
                    ++ (String.fromInt <| List.length model.dictionary)
        , el [ alignLeft ] <| UI.niceButton "Restart Game" OnRestartDictionaryClick (Just <| Icons.refresh 20)
        , el [ alignRight ] <|
            UI.niceButton
                (case model.notificationsEnabled of
                    Nothing ->
                        "Notifications"

                    Just True ->
                        "Disable Notifications"

                    Just False ->
                        "Enable Notifications"
                )
                OnToggleNotifications
                (Just <| Icons.bell 20)
        , UI.niceButton "Dictionary" OnToggleDictionaryModal (Just <| Icons.translate 20)
        , UI.niceButton "Help" OnToggleHelp (Just <| Icons.questionmark 20)
        ]


mobileViewTopBar : Model -> Element Msg
mobileViewTopBar model =
    row [ height <| px 50, width fill, Background.color UI.accentColor ]
        [ el [ width fill ] <|
            el
                [ centerX
                , onRight <|
                    el [] <|
                        case model.showTodoUpdate of
                            Nothing ->
                                text <| ""

                            Just u ->
                                el
                                    [ Font.color <|
                                        if u < 0 then
                                            UI.correctColor

                                        else
                                            UI.errorColor
                                    , Font.bold
                                    ]
                                <|
                                    text <|
                                        "  "
                                            ++ (if u > 0 then
                                                    "+"

                                                else
                                                    ""
                                               )
                                            ++ String.fromInt u
                ]
            <|
                MobileUI.niceTextWith [ Font.color UI.white ] <|
                    "Words to go: "
                        ++ (String.fromInt <| List.length model.dictionary)
        , MobileUI.simpleIconButtonInverted (Icons.questionmark 20) OnToggleHelp
            |> el [ alignRight ]
        ]


viewWordAnswers : Model -> Int -> Word -> Element Msg
viewWordAnswers model wordId word =
    word.characters
        |> List.indexedMap (viewSingleHanzi model wordId)
        |> row [ spacing 10 ]


viewWordEnglish word =
    word.english
        |> String.split "|"
        |> List.map (\txt -> paragraph [] [ text txt ])
        |> column [ spacing 5, alignLeft ]


viewWordEnglishMobile word =
    word.english
        |> String.split "|"
        |> List.map (\txt -> paragraph [ Font.size 16 ] [ text txt ])
        |> column [ spacing 5, alignLeft ]


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

        known =
            isCharacterKnown model character

        similar =
            isCharacterSimilar model character
    in
    el
        ([ UI.floating
         , UI.rounded 5
         , paddingXY 20 0

         --, width <| maximum 50 shrink
         , width <|
            if known then
                px 50

            else if similar then
                shrink

            else
                px 50
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
                    if known then
                        rgb255 152 226 172

                    else if similar then
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
                    if known then
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
                        if known then
                            character.hanzi

                        else if similar then
                            character.pinyinPart.pinyin

                        else
                            ""

                    _ ->
                        character.hanzi


viewInput : Model -> Element Msg
viewInput model =
    el [ width fill, padding 10 ] <|
        row
            [ width fill
            , Border.widthEach
                { top = 0
                , left = 0
                , right = 0
                , bottom = 2
                }
            , spacing 20
            ]
            [ if model.device == Desktop then
                Input.text
                    [ onEnter UserPressedEnter
                    , htmlAttribute <| Html.Attributes.id idInput
                    , Border.width 0
                    ]
                    { onChange = InputHanzi
                    , text = model.currentInput
                    , placeholder =
                        Just <|
                            Input.placeholder [ Font.size 14 ] <|
                                text <|
                                    "Write pinyin with tones here (e.g. hao3 for 好), then press 'OK'"
                                        ++ (if isOnDesktop model then
                                                " or the Enter key"

                                            else
                                                ""
                                           )
                    , label = Input.labelHidden ""
                    }

              else
                text model.currentInput
            , Input.button
                [ mouseOver [ Font.color UI.accentColorHighlight ]
                , Font.color UI.accentColor
                , Background.color UI.white
                , width shrink
                , height <| px 40
                , width <| px 40
                , Border.rounded 10
                , alignRight
                ]
                { onPress = Just UserPressedEnter
                , label =
                    el [ centerY, centerX, paddingXY 20 0, spacing 20 ] <|
                        text "Ok"
                }
            ]


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


isOnDesktop model =
    case model.device of
        Mobile ->
            False

        Desktop ->
            True


isOnMobile model =
    not <| isOnDesktop model


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
        ( { model
            | gameState = TooManyWrongAnswers
            , dictionary = model.wordChain ++ model.dictionary |> List.unique
            , showTodoUpdate = Just (List.length model.wordChain)
            , showTodoUpdateTimer = 2
          }
        , Cmd.none
        )

    else
        let
            finished =
                model.wordChain
                    |> List.foldl
                        (\word acc -> isWordFullyKnown model word && acc)
                        True
        in
        if finished then
            ( { model
                | gameState = FilledInCorrectly
                , showTodoUpdate = Just -(List.length model.wordChain)
                , showTodoUpdateTimer = 2
              }
            , case ( model.device, model.useScreenKeyboardOnMobile ) of
                ( Mobile, True ) ->
                    Cmd.none

                _ ->
                    focusNextBtn
            )

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
