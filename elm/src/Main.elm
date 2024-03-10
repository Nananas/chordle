module Main exposing (..)

import Backend
import Browser
import Browser.Dom
import Browser.Events
import Changelog
import Daily
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input
import Help
import Html
import Html.Attributes
import Icons
import Json.Decode
import Json.Encode
import List.Extra as List
import MobileUI
import Numbers
import Random
import Responsive
import Storage
import Task
import Tones
import Training
import UI
import Utils exposing (..)
import Uuid.Barebones exposing (uuidStringGenerator)
import WordChain
import Words


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
    | UUIDGenerated String
    | OnStorageLoaded Storage.Storage
    | OnGetBackend (Result Backend.Error ( Words.WordsFile, Backend.PageStats ))
    | ClickedToggleDictionary String Bool
    | ClickedToggleDictionaryModal
    | ClickedToggleSettings
    | ClickedToggleChangelog
    | ClickedToggleToneColors
    | ClickedToggleShowHanziAs Bool
    | ChooseToneColors Tones.Colors
    | SetDictionaryModalShowIndex Int
    | ClickedChooseDaily
    | ClickedChooseTraining
    | ClickedChooseNumbers
    | TrainingMsg Training.Msg
    | DailyMsg Daily.Msg
    | NumbersMsg Numbers.Msg
    | NoOp



--| MODEL


type ActiveModal
    = NoModal
    | DictionaryModal
    | SettingsModal
    | ChangelogModal
    | ToneColorsModal


type alias Model =
    { device : Device
    , state : State
    , uuid : Maybe String
    , wordsFile : Words.WordsFile
    , activeDicts : List String
    , activeModal : ActiveModal
    , showingCurrentDictionaryIndex : Int
    , oldChangelogVersion : Maybe String
    , pageStats : Maybe Backend.PageStats
    , showHanziAsPinyin : Bool
    , toneColors : Tones.Colors
    }


type State
    = LoadingId
    | LoadingViewport
    | LoadingBackend
    | LoadingActiveDicts
    | LoadingSettings
    | ChooseGameType
    | Training Training.Model
    | Daily Daily.Model
    | Numbers Numbers.Model
    | Error String


type alias Settings =
    { version : String
    , toneColors : Maybe String
    }



--


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = LoadingId
      , device = desktop
      , uuid = Nothing
      , wordsFile = Words.WordsFile [] Dict.empty
      , activeDicts = []
      , activeModal = NoModal
      , showingCurrentDictionaryIndex = 0
      , oldChangelogVersion = Nothing
      , pageStats = Nothing
      , showHanziAsPinyin = False
      , toneColors = Tones.NotChosen
      }
    , Storage.loadStorage "id"
    )



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.state ) of
        ( OnStorageLoaded { name, json }, LoadingId ) ->
            if name == "id" then
                let
                    decoder =
                        Json.Decode.field "uuid" Json.Decode.string

                    decoded =
                        Json.Decode.decodeValue decoder json
                in
                case decoded of
                    Ok uuid ->
                        ( { model | uuid = Just uuid, state = LoadingViewport }
                        , Browser.Dom.getViewport
                            |> Task.perform OnGetViewport
                        )

                    Err _ ->
                        ( model, Random.generate UUIDGenerated uuidStringGenerator )

            else
                ( model, Cmd.none )

        ( OnStorageLoaded { name, json }, LoadingActiveDicts ) ->
            if name == "active-dicts" then
                let
                    decoder =
                        Json.Decode.list Json.Decode.string

                    decoded =
                        Json.Decode.decodeValue decoder json
                in
                case decoded of
                    Ok activeDicts ->
                        ( { model
                            | activeDicts =
                                activeDicts
                                    |> List.filter (\d -> Dict.member d model.wordsFile.parts)
                            , state = LoadingSettings
                          }
                        , Storage.loadStorage "settings"
                        )

                    Err _ ->
                        ( { model
                            | activeDicts = []
                            , state = LoadingSettings
                          }
                        , Storage.loadStorage "settings"
                        )

            else
                ( model, Cmd.none )

        ( OnStorageLoaded { name, json }, LoadingSettings ) ->
            if name == "settings" then
                let
                    decoder =
                        Json.Decode.map2 Settings
                            (Json.Decode.field "version" Json.Decode.string)
                            (Json.Decode.maybe (Json.Decode.field "tone-colors" Json.Decode.string))

                    decoded =
                        Json.Decode.decodeValue decoder json
                in
                case decoded of
                    Ok { version, toneColors } ->
                        let
                            loadedToneColors =
                                toneColors
                                    |> Maybe.map Tones.stringToColors
                                    |> Maybe.withDefault Tones.NotChosen
                        in
                        if Changelog.currentVersion /= version then
                            ( { model
                                | activeModal = ChangelogModal
                                , state = ChooseGameType
                                , oldChangelogVersion = Just version
                                , toneColors = loadedToneColors
                              }
                            , saveSettingsToStorage loadedToneColors
                            )

                        else
                            ( { model
                                | oldChangelogVersion = Just version
                                , state = ChooseGameType
                                , toneColors = loadedToneColors
                                , activeModal =
                                    if loadedToneColors == Tones.NotChosen then
                                        ToneColorsModal

                                    else
                                        NoModal
                              }
                            , saveSettingsToStorage loadedToneColors
                            )

                    Err _ ->
                        ( { model
                            | activeModal = ChangelogModal
                            , state = ChooseGameType
                          }
                        , saveSettingsToStorage Tones.NotChosen
                        )

            else
                ( model, Cmd.none )

        ( OnGetBackend (Ok ( wordsFile, pageStats )), LoadingBackend ) ->
            ( { model
                | wordsFile = wordsFile
                , state = LoadingActiveDicts
                , pageStats = Just pageStats
              }
            , Storage.loadStorage "active-dicts"
            )

        ( OnGetBackend (Err err), LoadingBackend ) ->
            ( { model
                | state = Error ("Could not load data: " ++ err)
              }
            , Cmd.none
            )

        ( OnGetBackend _, _ ) ->
            ( model, Cmd.none )

        -- Forward storage subscriptions
        -- doing this in subscriptions does not seem to work well
        ( OnStorageLoaded storage, Training _ ) ->
            update (TrainingMsg <| Training.OnStorageLoaded storage) model

        ( OnStorageLoaded storage, Daily _ ) ->
            update (DailyMsg <| Daily.OnStorageLoaded storage) model

        ( OnStorageLoaded _, _ ) ->
            ( model, Cmd.none )

        ( UUIDGenerated uuid, _ ) ->
            ( { model
                | uuid =
                    Just uuid
              }
            , Cmd.batch
                [ Browser.Dom.getViewport
                    |> Task.perform OnGetViewport
                , Storage.setStorage { name = "id", json = Json.Encode.object [ ( "uuid", Json.Encode.string uuid ) ] }
                ]
            )

        ( OnGetViewport { viewport }, _ ) ->
            ( { model
                | device = classifyViewport viewport
                , state = LoadingBackend
              }
            , fetchBackend
            )

        ( OnResizeViewport width height, _ ) ->
            ( { model | device = classifyViewport { width = width, height = height } }, Cmd.none )

        ( ClickedToggleDictionaryModal, ChooseGameType ) ->
            ( { model
                | activeModal = model.activeModal |> toggleModal DictionaryModal
                , showingCurrentDictionaryIndex = 0
              }
            , Cmd.none
            )

        ( ClickedToggleDictionaryModal, _ ) ->
            ( model, Cmd.none )

        ( ClickedToggleDictionary dictName toAdd, ChooseGameType ) ->
            let
                activeDicts =
                    if toAdd then
                        dictName :: model.activeDicts

                    else
                        List.filter (\name -> name /= dictName) model.activeDicts
            in
            ( { model | activeDicts = activeDicts }
            , Storage.setStorage
                { name = "active-dicts"
                , json =
                    Json.Encode.list
                        Json.Encode.string
                        activeDicts
                }
            )

        ( SetDictionaryModalShowIndex newIndex, ChooseGameType ) ->
            ( { model
                | showingCurrentDictionaryIndex =
                    max 0 (min newIndex (List.length model.wordsFile.structure - 1))
              }
            , Cmd.none
            )

        ( ClickedToggleSettings, _ ) ->
            ( { model | activeModal = model.activeModal |> toggleModal SettingsModal }, Cmd.none )

        ( ClickedToggleChangelog, _ ) ->
            ( { model
                | activeModal =
                    if model.activeModal == ChangelogModal && model.toneColors == Tones.NotChosen then
                        ToneColorsModal

                    else if model.activeModal /= ChangelogModal then
                        ChangelogModal

                    else
                        NoModal
              }
            , Cmd.none
            )

        ( ClickedToggleToneColors, _ ) ->
            ( { model | activeModal = model.activeModal |> toggleModal ToneColorsModal }, Cmd.none )

        ( ClickedToggleShowHanziAs showHanziAsPinyin, _ ) ->
            ( { model | showHanziAsPinyin = showHanziAsPinyin }, Cmd.none )

        ( SetDictionaryModalShowIndex _, _ ) ->
            ( model, Cmd.none )

        ( ClickedToggleDictionary _ _, _ ) ->
            ( model, Cmd.none )

        ( ChooseToneColors newToneColors, _ ) ->
            ( { model | toneColors = newToneColors }
            , saveSettingsToStorage newToneColors
            )

        ( ClickedChooseDaily, _ ) ->
            Daily.init
                |> liftBoth Daily DailyMsg
                |> setStateIn model

        ( ClickedChooseTraining, _ ) ->
            Training.init
                |> liftBoth Training TrainingMsg
                |> setStateIn model

        ( ClickedChooseNumbers, _ ) ->
            Numbers.init
                |> liftBoth Numbers NumbersMsg
                |> setStateIn model

        --
        ( TrainingMsg Training.OnClickedHome, _ ) ->
            ( { model | state = ChooseGameType }, Cmd.none )

        ( TrainingMsg trainingMsg, Training trainingModel ) ->
            Training.update model.uuid model.wordsFile.parts model.activeDicts trainingMsg trainingModel
                |> liftBoth Training TrainingMsg
                |> setStateIn model

        ( TrainingMsg _, _ ) ->
            ( model, Cmd.none )

        ( DailyMsg Daily.OnClickedHome, _ ) ->
            ( { model | state = ChooseGameType }, Cmd.none )

        ( DailyMsg dailyMsg, Daily dailyModel ) ->
            Daily.update model.uuid model.wordsFile.parts model.activeDicts dailyMsg dailyModel
                |> liftBoth Daily DailyMsg
                |> setStateIn model

        ( DailyMsg _, _ ) ->
            ( model, Cmd.none )

        ( NumbersMsg Numbers.OnClickedHome, _ ) ->
            ( { model | state = ChooseGameType }, Cmd.none )

        ( NumbersMsg numbersMsg, Numbers numbersModel ) ->
            Numbers.update model.uuid numbersMsg numbersModel
                |> liftBoth Numbers NumbersMsg
                |> setStateIn model

        ( NumbersMsg _, _ ) ->
            ( model, Cmd.none )

        ( NoOp, _ ) ->
            ( model, Cmd.none )


fetchBackend =
    Task.map2 Tuple.pair
        Backend.getWords
        Backend.getPageStats
        |> Task.attempt OnGetBackend


setStateIn model ( state, cmd ) =
    ( { model | state = state }, cmd )


toggleModal newModal oldModal =
    if oldModal == newModal then
        NoModal

    else
        newModal


saveSettingsToStorage toneColors =
    Storage.setStorage
        { name = "settings"
        , json =
            Json.Encode.object
                ([ ( "version", Json.Encode.string Changelog.currentVersion )
                 ]
                    ++ (case Tones.colorsToString toneColors of
                            Just tc ->
                                [ ( "tone-colors", Json.Encode.string tc ) ]

                            Nothing ->
                                []
                       )
                )
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize OnResizeViewport
        , case model.state of
            Training trainingModel ->
                Training.subscriptions trainingModel
                    |> Sub.map TrainingMsg

            Daily _ ->
                Daily.subscriptions
                    |> Sub.map DailyMsg

            _ ->
                Sub.none
        , Storage.storageLoaded OnStorageLoaded
        ]



-- VIEW


view : Model -> Html.Html Msg
view model =
    layoutWith { options = [ focusStyle { backgroundColor = Nothing, borderColor = Nothing, shadow = Nothing } ] } [ width fill, height fill ] <|
        case model.state of
            ChooseGameType ->
                let
                    onMobile =
                        isOnMobile model.device
                in
                viewChooseGameType onMobile model

            Training trainingModel ->
                Training.view model.device model.showHanziAsPinyin model.toneColors model.wordsFile.parts model.activeDicts trainingModel
                    |> Element.map TrainingMsg

            Daily dailyModel ->
                Daily.view model.device model.showHanziAsPinyin model.toneColors dailyModel
                    |> Element.map DailyMsg

            Numbers numbersModel ->
                Numbers.view model.device model.toneColors numbersModel
                    |> Element.map NumbersMsg

            Error msg ->
                el [ centerX, centerY ] <|
                    column [ alignLeft, spacing 30 ] <|
                        [ UI.niceText "Something went wrong:"
                        , UI.niceTextWith [ Font.color UI.errorColorLight ] msg
                        , UI.niceText "Please try again later..."
                        ]

            _ ->
                UI.spinner


viewSettingsModal onMobile =
    let
        modalFn =
            if onMobile then
                MobileUI.modal

            else
                UI.modal
    in
    modalFn ClickedToggleSettings
        [ row [ centerX ]
            [ UI.heading "Settings"
            ]
        , column [ centerY, width fill, spacing 20 ]
            [ el [ centerX ] <| Responsive.button onMobile "Set Tone colors" ClickedToggleToneColors Icons.colorPicker
            , el [ centerX ] <| Responsive.button onMobile "Changelog" ClickedToggleChangelog Icons.code
            ]
        ]


viewToneColorsModal onMobile toneColors =
    let
        modalFn =
            if onMobile then
                MobileUI.modal

            else
                UI.modal

        buttonFn =
            if onMobile then
                \_ onClick icon -> MobileUI.simpleIconButtonInverted icon onClick

            else
                \str onClick icon -> UI.niceButton str onClick (Just icon)

        iconSize =
            if onMobile then
                16

            else
                20

        colorSet newToneColors =
            row [ width fill, spacing 5 ]
                [ Help.viewExampleHanziFinishedRound newToneColors "听" Tones.First True
                , Help.viewExampleHanziFinishedRound newToneColors "人" Tones.Second True
                , Help.viewExampleHanziFinishedRound newToneColors "写" Tones.Third True
                , Help.viewExampleHanziFinishedRound newToneColors "四" Tones.Forth True
                , Help.viewExampleHanziFinishedRound newToneColors "吧" Tones.Fifth True
                ]

        colorSetBtn newToneColors =
            if toneColors == newToneColors then
                el [ height (px 50) ] <| UI.niceText "(current)"

            else
                el [ height (px 50) ] <| Responsive.button onMobile "Select" (ChooseToneColors newToneColors) Icons.colorPicker
    in
    modalFn ClickedToggleToneColors
        [ row [ centerX ]
            [ UI.heading "Tone Colors"
            ]
        , row [ spacing 10 ]
            [ column [ spacing 20 ]
                [ el [ height (px 50) ] <| UI.niceText "Chordle: "
                , el [ height (px 50) ] <| UI.niceText "Pleco: "
                , el [ height (px 50) ] <| UI.niceText "No color: "
                ]
            , column [ spacing 20 ]
                [ colorSet Tones.ChordleColors
                , colorSet Tones.PlecoColors
                , colorSet Tones.NoColors
                ]
            , column [ spacing 20 ]
                [ colorSetBtn Tones.ChordleColors
                , colorSetBtn Tones.PlecoColors
                , colorSetBtn Tones.NoColors
                ]
            ]
        ]


viewChooseGameType : Bool -> Model -> Element Msg
viewChooseGameType onMobile model =
    let
        modal =
            case model.activeModal of
                DictionaryModal ->
                    viewDictionaryModal onMobile model.wordsFile model.activeDicts model.showingCurrentDictionaryIndex

                SettingsModal ->
                    viewSettingsModal onMobile

                ChangelogModal ->
                    Changelog.view model.oldChangelogVersion onMobile ClickedToggleChangelog

                ToneColorsModal ->
                    viewToneColorsModal onMobile model.toneColors

                NoModal ->
                    none

        attrs =
            if List.isEmpty model.activeDicts then
                [ Background.color UI.lightGray, mouseOver [] ]

            else
                []

        orDisabled msg =
            if List.isEmpty model.activeDicts then
                NoOp

            else
                msg
    in
    column [ width fill, height fill, inFront modal ]
        [ viewTopBar onMobile model.wordsFile.parts model.activeDicts (model.activeModal == DictionaryModal)
        , el [ width fill, height fill ] <|
            column [ centerX, centerY, spacing 100 ]
                [ column [ centerX, spacing 20 ]
                    [ Responsive.row onMobile
                        [ spacing 50 ]
                        [ el [] <| UI.niceButtonWith attrs "Daily Chordle" (orDisabled ClickedChooseDaily) (Just <| Icons.calendar 24)
                        , el [ centerX ] <| UI.niceButtonWith attrs "Training" (orDisabled ClickedChooseTraining) (Just <| Icons.academicCap 24)
                        , el [ centerX ] <| UI.niceButtonWith attrs "Numbers" (orDisabled ClickedChooseNumbers) (Just <| Icons.calculator 24)
                        ]
                    ]
                ]
        , viewShowHanziAs model.showHanziAsPinyin
        , el [ height (px 20) ] none -- spacing
        , case model.pageStats of
            Just stats ->
                let
                    totalActivity =
                        stats.activity |> List.maximum |> Maybe.withDefault 0

                    activityChart =
                        stats.activity
                            |> List.map
                                (\a ->
                                    column [ width fill, height fill ]
                                        [ el
                                            [ width <| fillPortion 1
                                            , htmlAttribute <|
                                                Html.Attributes.style "height"
                                                    (String.fromFloat (toFloat a / toFloat totalActivity * 100) ++ "%")
                                            , Background.color UI.accentColorLight
                                            , alignBottom
                                            , inFront <|
                                                el
                                                    [ Font.size 0
                                                    , mouseOver [ Font.color UI.accentColorHighlight, Font.size 20 ]
                                                    , Font.color UI.accentColorLight
                                                    , width fill
                                                    , height fill
                                                    ]
                                                    (el
                                                        [ centerX
                                                        , centerY
                                                        ]
                                                        (text (String.fromInt a))
                                                    )
                                            ]
                                            none
                                        ]
                                )
                            |> row [ width fill, height fill, spacing 5 ]
                in
                el [ width fill, height shrink, padding 30, behindContent activityChart ] <|
                    column [ centerX, spacing 10, htmlAttribute <| Html.Attributes.style "pointer-events" "none" ]
                        [ UI.niceText "Global number of games played:"
                        , UI.niceTextWith [ Font.size 18 ] <| "Daily: " ++ String.fromInt stats.dailyCount
                        , UI.niceTextWith [ Font.size 18 ] <| "Training: " ++ String.fromInt stats.trainingCount
                        , UI.niceTextWith [ Font.size 18 ] <| "Numbers: " ++ String.fromInt stats.numbersCount
                        ]

            _ ->
                none
        ]


viewShowHanziAs : Bool -> Element Msg
viewShowHanziAs showHanziAsPinyin =
    let
        checkbox checked =
            el
                [ UI.rounded 4
                , Border.width 3
                , Border.color UI.accentColorHighlight
                , Font.color UI.accentColor
                , Font.size 20
                , Font.medium
                ]
            <|
                if checked then
                    el [ padding 10 ] (text "Pinyin")

                else
                    el [ padding 10 ] (text "汉字")
    in
    el [ width fill ] <|
        column [ centerX, spacing 10, Font.color UI.accentColorHighlight ]
            [ UI.hr
            , Element.Input.checkbox []
                { onChange = ClickedToggleShowHanziAs
                , icon = checkbox
                , checked = showHanziAsPinyin
                , label = Element.Input.labelLeft [ centerY ] (UI.niceText "Show words in: ")
                }
            , UI.hr
            ]


viewTopBar : Bool -> Words.Dictionaries -> List String -> Bool -> Element Msg
viewTopBar onMobile dictionaries activeDicts showDictionaryModal =
    let
        buttonFn =
            if onMobile then
                \_ onClick icon -> MobileUI.simpleIconButtonInverted icon onClick

            else
                \str onClick icon -> UI.niceButton str onClick (Just icon)

        iconSize =
            if onMobile then
                16

            else
                20

        helpMessage =
            if List.length activeDicts == 0 && not showDictionaryModal then
                column
                    []
                    [ el [ height (px 50) ] none
                    , row [ height <| px 50 ]
                        [ el [ width (px 250) ] none
                        , el
                            [ htmlAttribute <| Html.Attributes.class "flashy-colors"
                            ]
                          <|
                            Icons.arrowRightUp 42
                        ]
                    , el
                        [ Background.color UI.lightColor
                        , Border.width 4
                        , UI.rounded 25
                        , UI.floating
                        , htmlAttribute <| Html.Attributes.class "flashy-colors"
                        , paddingXY 20 10
                        ]
                      <|
                        UI.niceText "Select a dictionary first"
                    ]

            else
                none
    in
    row
        ([ height <| px 50, width fill, Background.color UI.accentColor ]
            ++ (if onMobile then
                    [ spacing 4, behindContent <| UI.viewLogo "" ]

                else
                    [ paddingXY 20 0, spacing 20, behindContent <| UI.viewLogo "" ]
               )
        )
        [ el [ alignRight, onLeft helpMessage ] <| buttonFn "Dictionaries" ClickedToggleDictionaryModal (Icons.translate iconSize)
        , el [ alignRight ] <| buttonFn "Settings" ClickedToggleSettings (Icons.menu iconSize)
        ]


viewDictionaryModal : Bool -> Words.WordsFile -> List String -> Int -> Element Msg
viewDictionaryModal onMobile wordsFile activeDicts showingCurrentDictionaryIndex =
    let
        modalFn =
            if onMobile then
                MobileUI.modal

            else
                UI.modal

        totalWordCount =
            wordsFile.parts
                |> Dict.foldl
                    (\k v acc ->
                        if List.member k activeDicts then
                            acc + List.length v

                        else
                            acc
                    )
                    0

        activeDictsCount =
            List.length activeDicts

        singleDictionaryCard subtitle dictName enabled words onAddMsg =
            let
                icon =
                    if enabled then
                        Icons.xmark 20

                    else
                        Icons.plus 20
            in
            el
                [ width
                    (px <|
                        if onMobile then
                            300

                        else
                            400
                    )
                , height <|
                    if onMobile then
                        px 150

                    else
                        px 200
                , UI.floating
                , UI.rounded 10
                , Background.color UI.lightColor
                , inFront <|
                    el [ alignRight, alignBottom, padding 10 ] <|
                        UI.niceIconButtonWith [ UI.floatingHigh ] icon onAddMsg "Add"
                ]
            <|
                column [ width fill, height fill, padding 20, spacing 10 ]
                    [ row
                        [ alignTop
                        , width fill
                        , Font.color UI.accentColor
                        ]
                        [ el [ alignLeft ] <|
                            UI.headingWith
                                [ Font.color <|
                                    if enabled then
                                        UI.accentColor

                                    else
                                        UI.accentColorHighlight
                                ]
                                subtitle
                        , el [ alignRight, Font.color UI.accentColor ] <| text <| String.fromInt (List.length words) ++ " words"
                        ]
                    , el [ scrollbarY, height fill, width fill, padding 5 ] <|
                        (wordsFile.parts
                            |> Dict.get dictName
                            |> Maybe.withDefault []
                            |> List.map (Words.wordHanzi >> text)
                            |> wrappedRow
                                [ spacing 10
                                , Font.color <|
                                    if enabled then
                                        UI.black

                                    else
                                        UI.gray
                                ]
                        )
                    ]

        chevronAttr =
            [ height (px 40), UI.rounded 20, width (px 40) ]

        noChevronOr chevron show =
            if show then
                chevron

            else
                el [ width (px 40) ] none

        dot colored =
            el
                [ Font.color <|
                    if colored then
                        UI.accentColor

                    else
                        UI.accentColorHighlight
                ]
            <|
                Icons.dot 10
    in
    modalFn ClickedToggleDictionaryModal
        [ UI.heading <| "Active Dictionaries"
        , paragraph [ width <| maximum 850 fill ]
            [ text "You can add one or more dictionaries below. The words from these dictionaries are then available to play the "
            , el [ Font.bold ] (text "Daily")
            , text " and "
            , el [ Font.bold ] (text "Training")
            , text " with."
            ]
        , if activeDictsCount == 0 then
            UI.niceTextWith
                [ htmlAttribute <| Html.Attributes.class "flashy-colors"
                ]
                "Please add at least one dictionary!"

          else
            UI.niceText <| "You have added " ++ String.fromInt activeDictsCount ++ " of " ++ (String.fromInt <| Dict.size wordsFile.parts) ++ " dictionaries."
        , row [ centerX, spacing 10 ]
            [ (showingCurrentDictionaryIndex > 0)
                |> noChevronOr (UI.niceIconButtonWith chevronAttr (Icons.chevronLeft 20) (SetDictionaryModalShowIndex (showingCurrentDictionaryIndex - 1)) "")
            , wordsFile.structure
                |> List.getAt showingCurrentDictionaryIndex
                |> Maybe.map
                    (\( title, sub ) ->
                        column [ spacing 10, width <| maximum 850 fill ]
                            [ UI.headingWith [] title
                            , Responsive.wrappedRow onMobile [ spacing 10 ] <|
                                List.map
                                    (\( subtitle, dictName ) ->
                                        let
                                            enabled =
                                                List.member dictName activeDicts

                                            words =
                                                Dict.get dictName wordsFile.parts
                                                    |> Maybe.withDefault []
                                        in
                                        singleDictionaryCard subtitle dictName enabled words (ClickedToggleDictionary dictName (not enabled))
                                    )
                                    sub
                            ]
                    )
                |> Maybe.withDefault none

            --wordsFile.parts
            --|> Dict.toList
            --|> List.getAt showingCurrentDictionaryIndex
            --|> Maybe.map
            --    (\( dictName, words ) ->
            --        let
            --            enabled =
            --                List.member dictName activeDicts
            --            txt =
            --                if enabled then
            --                    "Remove"
            --                else
            --                    "Add"
            --        in
            --        column []
            --            [ singleDictionaryCard dictName enabled words
            --            , el [ centerX, padding 10 ] <|
            --                UI.niceButton txt (ClickedToggleDictionary dictName (not enabled)) Nothing
            --            ]
            --    )
            --|> Maybe.withDefault none
            , (showingCurrentDictionaryIndex < List.length wordsFile.structure - 1)
                |> noChevronOr (UI.niceIconButtonWith chevronAttr (Icons.chevronRight 20) (SetDictionaryModalShowIndex (showingCurrentDictionaryIndex + 1)) "")
            ]
        , row [ centerX, spacing 10 ]
            (List.range 0 (List.length wordsFile.structure - 1)
                |> List.map (\i -> dot (i == showingCurrentDictionaryIndex))
            )
        , el [ height (px 20) ] none
        , UI.niceText <| "Total number of words: " ++ String.fromInt totalWordCount
        ]



--
--
