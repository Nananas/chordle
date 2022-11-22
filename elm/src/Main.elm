module Main exposing (..)

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
import Notifications
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
    | TrainingMsg Training.Msg
    | DailyMsg Daily.Msg
    | ClickedChooseDaily
    | ClickedChooseTraining
    | OnStorageLoaded Storage.Storage



--| MODEL


type alias Model =
    { device : Device
    , state : State
    , uuid : Maybe String
    }


type State
    = Loading
    | ChooseGameType
    | Training Training.Model
    | Daily Daily.Model



--


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = Loading, device = desktop, uuid = Nothing }
    , Cmd.batch
        [ Storage.loadStorage "id"
        ]
    )



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.state ) of
        ( OnStorageLoaded { name, json }, Loading ) ->
            let
                decoder =
                    Json.Decode.field "uuid" Json.Decode.string

                decoded =
                    Json.Decode.decodeValue decoder json
            in
            case decoded of
                Ok uuid ->
                    ( { model | uuid = Just uuid }
                    , Browser.Dom.getViewport
                        |> Task.perform OnGetViewport
                    )

                Err err ->
                    ( model, Random.generate UUIDGenerated uuidStringGenerator )

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
                , state = ChooseGameType
              }
            , Cmd.none
            )

        ( OnResizeViewport width height, _ ) ->
            ( { model | device = classifyViewport { width = width, height = height } }, Cmd.none )

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
            Training.update model.uuid trainingMsg trainingModel
                |> liftBoth Training TrainingMsg
                |> setStateIn model

        ( TrainingMsg _, _ ) ->
            ( model, Cmd.none )

        ( DailyMsg Daily.OnClickedHome, _ ) ->
            ( { model | state = ChooseGameType }, Cmd.none )

        ( DailyMsg dailyMsg, Daily dailyModel ) ->
            Daily.update model.uuid dailyMsg dailyModel
                |> liftBoth Daily DailyMsg
                |> setStateIn model

        ( DailyMsg _, _ ) ->
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
    layoutWith { options = [ focusStyle { backgroundColor = Nothing, borderColor = Nothing, shadow = Nothing } ] } [ width fill, height fill ] <|
        case model.state of
            Loading ->
                UI.spinner

            ChooseGameType ->
                viewChooseGameType

            Training trainingModel ->
                Training.view model.device trainingModel
                    |> Element.map TrainingMsg

            Daily dailyModel ->
                Daily.view model.device dailyModel
                    |> Element.map DailyMsg


viewChooseGameType =
    el [ width fill, height fill ] <|
        row [ centerX, centerY, spacing 50 ]
            [ UI.niceButton "Daily Chordle" ClickedChooseDaily (Just <| Icons.calendar 24)
            , UI.niceButton "Training" ClickedChooseTraining (Just <| Icons.academicCap 24)
            ]



--
--
