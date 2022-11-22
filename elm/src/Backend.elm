module Backend exposing (..)

import DailyProgress exposing (..)
import Dict
import Http
import Json.Decode
import Json.Encode


eventUrl =
    "./event"


type Msg
    = NoOp (Result Http.Error ())


type alias Uuid =
    Maybe String


postDaily : Uuid -> { state : GameOverState, attempts : Int, mistakes : Int, rata : Int } -> Cmd Msg
postDaily uuid { state, attempts, mistakes, rata } =
    let
        resultToString r =
            case r of
                Succeeded ->
                    "success"

                Failed ->
                    "fail"

                GiveUp ->
                    "give-up"

        ( progress, result ) =
            case state of
                First dp ->
                    ( "first", resultToString dp )

                Other dp ->
                    ( "other", resultToString dp )

        body =
            Json.Encode.object
                [ ( "page", Json.Encode.string "daily" )
                , ( "uuid", Json.Encode.string (Maybe.withDefault "<unknown>" uuid) )
                , ( "event"
                  , Json.Encode.object
                        [ ( "progress", Json.Encode.string progress )
                        , ( "result", Json.Encode.string result )
                        , ( "attempts", Json.Encode.int attempts )
                        , ( "mistakes", Json.Encode.int mistakes )
                        , ( "rata", Json.Encode.int rata )
                        ]
                  )
                ]
    in
    Http.post
        { url = eventUrl
        , body = Http.jsonBody body
        , expect = Http.expectWhatever NoOp
        }


postTrainingFinished uuid { dictsActive, gameStats } =
    let
        activeDicts : List String
        activeDicts =
            dictsActive
                |> Dict.filter (\k v -> v)
                |> Dict.keys

        body =
            Json.Encode.object
                [ ( "page", Json.Encode.string "training" )
                , ( "uuid", Json.Encode.string (Maybe.withDefault "<unknown>" uuid) )
                , ( "event"
                  , Json.Encode.object
                        [ ( "dicts-active", Json.Encode.list Json.Encode.string activeDicts )
                        , ( "attempts", Json.Encode.int gameStats.attempts )
                        , ( "correct", Json.Encode.int gameStats.correct )
                        , ( "retries", Json.Encode.int gameStats.retries )
                        ]
                  )
                ]
    in
    Http.post
        { url = eventUrl
        , body = Http.jsonBody body
        , expect = Http.expectWhatever NoOp
        }
