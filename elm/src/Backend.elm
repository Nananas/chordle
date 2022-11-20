module Backend exposing (..)

import DailyProgress exposing (..)
import Http
import Json.Decode
import Json.Encode


eventUrl =
    "./event"


type Msg
    = NoOp (Result Http.Error ())


postDaily : { state : GameOverState, attempts : Int, mistakes : Int, rata : Int } -> Cmd Msg
postDaily { state, attempts, mistakes, rata } =
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
