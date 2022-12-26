module Main exposing (..)

import Backend
import Browser
import Browser.Dom
import Browser.Events
import Daily
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import Help
import Html
import Html.Attributes
import Html.Events
import Icons
import Json.Decode
import Json.Encode
import List.Extra as List
import MobileUI
import Process
import Random
import Random.List
import Set
import Storage
import String.Extra as String
import Task
import Time
import Tones exposing (..)
import Training
import UI
import Utils exposing (..)
import Uuid.Barebones exposing (uuidStringGenerator)
import Words exposing (..)


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
    | OnGetWords Backend.GetWordsResponse
    | ClickedToggleDictionary String Bool
    | ClickedToggleDictionaryModal
    | SetDictionaryModalShowIndex Int
    | ClickedChooseDaily
    | ClickedChooseTraining
    | TrainingMsg Training.Msg
    | DailyMsg Daily.Msg
    | NoOp



--| MODEL


type alias Model =
    { device : Device
    , state : State
    , uuid : Maybe String
    , wordsFile : Words.WordsFile
    , activeDicts : List String
    , showDictionaryModal : Bool
    , showingCurrentDictionaryIndex : Int
    }


type State
    = LoadingId
    | LoadingViewport
    | LoadingWords
    | LoadingActiveDicts
    | ChooseGameType
    | Training Training.Model
    | Daily Daily.Model
    | Error String



--


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = LoadingId
      , device = desktop
      , uuid = Nothing
      , wordsFile = Words.WordsFile [] Dict.empty
      , activeDicts = []
      , showDictionaryModal = False
      , showingCurrentDictionaryIndex = 0
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

                    Err err ->
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
                            , state = ChooseGameType
                          }
                        , Cmd.none
                        )

                    Err err ->
                        ( { model
                            | activeDicts = []
                            , state = ChooseGameType
                          }
                        , Cmd.none
                        )

            else
                ( model, Cmd.none )

        ( OnGetWords (Ok wordsFile), LoadingWords ) ->
            ( { model
                | wordsFile = wordsFile
                , state = LoadingActiveDicts
              }
            , Storage.loadStorage "active-dicts"
            )

        ( OnGetWords (Err err), LoadingWords ) ->
            ( { model | state = Error "I could not load the dictionaries." }, Cmd.none )

        ( OnGetWords x, _ ) ->
            ( model, Cmd.none )

        -- Forward storage subscriptions
        -- doing this in subscriptions does not seem to work well
        ( OnStorageLoaded storage, Training _ ) ->
            update (TrainingMsg <| Training.OnStorageLoaded storage) model

        ( OnStorageLoaded storage, Daily _ ) ->
            update (DailyMsg <| Daily.OnStorageLoaded storage) model

        ( OnStorageLoaded storage, _ ) ->
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
                , state = LoadingWords
              }
            , Backend.getWords OnGetWords
            )

        ( OnResizeViewport width height, _ ) ->
            ( { model | device = classifyViewport { width = width, height = height } }, Cmd.none )

        ( ClickedToggleDictionaryModal, ChooseGameType ) ->
            ( { model
                | showDictionaryModal = not model.showDictionaryModal
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

        ( SetDictionaryModalShowIndex _, _ ) ->
            ( model, Cmd.none )

        ( ClickedToggleDictionary _ _, _ ) ->
            ( model, Cmd.none )

        ( ClickedChooseDaily, _ ) ->
            Daily.init
                |> liftBoth Daily DailyMsg
                |> setStateIn model

        ( ClickedChooseTraining, _ ) ->
            Training.init
                |> liftBoth Training TrainingMsg
                |> setStateIn model

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

        ( NoOp, _ ) ->
            ( model, Cmd.none )


setStateIn model ( state, cmd ) =
    ( { model | state = state }, cmd )



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
    let
        onMobile =
            isOnMobile model.device
    in
    layoutWith { options = [ focusStyle { backgroundColor = Nothing, borderColor = Nothing, shadow = Nothing } ] } [ width fill, height fill ] <|
        case model.state of
            ChooseGameType ->
                viewChooseGameType onMobile model.wordsFile model.activeDicts model.showDictionaryModal model.showingCurrentDictionaryIndex

            Training trainingModel ->
                Training.view model.device model.wordsFile.parts model.activeDicts trainingModel
                    |> Element.map TrainingMsg

            Daily dailyModel ->
                Daily.view model.device dailyModel
                    |> Element.map DailyMsg

            Error msg ->
                el [ centerX, centerY ] <|
                    column [ alignLeft, spacing 30 ] <|
                        [ UI.niceText "Something went wrong:"
                        , UI.niceTextWith [ Font.color UI.errorColorLight ] msg
                        , UI.niceText "Please try again later..."
                        ]

            _ ->
                UI.spinner


viewChooseGameType : Bool -> Words.WordsFile -> List String -> Bool -> Int -> Element Msg
viewChooseGameType onMobile wordsFile activeDicts showModal showingCurrentDictionaryIndex =
    let
        modal =
            if showModal then
                viewDictionaryModal onMobile wordsFile activeDicts showingCurrentDictionaryIndex

            else
                none

        attrs =
            if List.isEmpty activeDicts then
                [ Background.color UI.lightGray, mouseOver [] ]

            else
                []

        orDisabled msg =
            if List.isEmpty activeDicts then
                NoOp

            else
                msg
    in
    column [ width fill, height fill, inFront modal ]
        [ viewTopBar onMobile wordsFile.parts activeDicts showModal
        , el [ width fill, height fill ] <|
            column [ centerX, centerY, spacing 100 ]
                [ column [ centerX, spacing 20 ]
                    [ row [ centerX, spacing 50 ]
                        [ el [] <| UI.niceButtonWith attrs "Daily Chordle" (orDisabled ClickedChooseDaily) (Just <| Icons.calendar 24)
                        , el [] <| UI.niceButtonWith attrs "Training" (orDisabled ClickedChooseTraining) (Just <| Icons.academicCap 24)
                        ]
                    ]
                ]
        ]


viewTopBar : Bool -> Words.Dictionaries -> List String -> Bool -> Element Msg
viewTopBar onMobile dictionaries activeDicts showModal =
    let
        buttonFn =
            if onMobile then
                \str onClick icon -> MobileUI.simpleIconButtonInverted icon onClick

            else
                \str onClick icon -> UI.niceButton str onClick (Just icon)

        iconSize =
            if onMobile then
                16

            else
                20

        helpMessage =
            if List.length activeDicts == 0 && not showModal then
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
        ]


viewDictionaryModal : Bool -> Words.WordsFile -> List String -> Int -> Element Msg
viewDictionaryModal onMobile wordsFile activeDicts showingCurrentDictionaryIndex =
    let
        modalFn =
            if onMobile then
                MobileUI.modal

            else
                UI.modal

        container =
            if onMobile then
                column

            else
                row

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
                        px 200

                    else
                        px 300
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
        , paragraph [ width <| maximum 700 fill ]
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
                        column [ spacing 10 ]
                            [ UI.headingWith [] title
                            , UI.columnOrRow onMobile [ spacing 10 ] <|
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
