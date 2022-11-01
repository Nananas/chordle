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
    | TrainingMsg Training.Msg
    | DailyMsg Daily.Msg
    | ClickedChooseDaily
    | ClickedChooseTraining



--| MODEL


type alias Model =
    { device : Device
    , state : State
    }


type State
    = LoadingDeviceType
    | ChooseGameType
    | Training Training.Model
    | Daily Daily.Model



--


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = LoadingDeviceType, device = desktop }
    , Browser.Dom.getViewport
        |> Task.perform OnGetViewport
    )



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.state ) of
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
            Training.update trainingMsg trainingModel
                |> liftBoth Training TrainingMsg
                |> setStateIn model

        ( TrainingMsg _, _ ) ->
            ( model, Cmd.none )

        ( DailyMsg Daily.OnClickedHome, _ ) ->
            ( { model | state = ChooseGameType }, Cmd.none )

        ( DailyMsg dailyMsg, Daily dailyModel ) ->
            Daily.update dailyMsg dailyModel
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
        ]



-- VIEW


view : Model -> Html.Html Msg
view model =
    layoutWith { options = [ focusStyle { backgroundColor = Nothing, borderColor = Nothing, shadow = Nothing } ] } [ width fill, height fill ] <|
        case model.state of
            LoadingDeviceType ->
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
