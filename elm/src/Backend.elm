module Backend exposing (..)

import DailyProgress exposing (..)
import Http
import Json.Decode
import Json.Encode
import Words


eventUrl =
    "./event"


type Msg
    = NoOp (Result Http.Error ())


type alias Uuid =
    Maybe String


postDaily : Uuid -> List String -> { state : GameOverState, attempts : Int, mistakes : Int, rata : Int } -> Cmd Msg
postDaily uuid activeDicts { state, attempts, mistakes, rata } =
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
                , ( "details"
                  , Json.Encode.object
                        [ ( "dicts-active", Json.Encode.list Json.Encode.string activeDicts )
                        , ( "progress", Json.Encode.string progress )
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


postTrainingRoundEnd uuid { dictsActive, nrWordsFound, wasSuccess, mistakes } =
    let
        body =
            Json.Encode.object
                [ ( "page", Json.Encode.string "training" )
                , ( "uuid", Json.Encode.string (Maybe.withDefault "<unknown>" uuid) )
                , ( "details"
                  , Json.Encode.object
                        [ ( "dicts-active", Json.Encode.list Json.Encode.string dictsActive )
                        , ( "nr-words-found", Json.Encode.int nrWordsFound )
                        , ( "success", Json.Encode.bool wasSuccess )
                        , ( "mistakes", Json.Encode.int mistakes )
                        ]
                  )
                ]
    in
    Http.post
        { url = eventUrl
        , body = Http.jsonBody body
        , expect = Http.expectWhatever NoOp
        }


postTrainingFinished uuid activeDicts { gameStats } =
    let
        body =
            Json.Encode.object
                [ ( "page", Json.Encode.string "training" )
                , ( "uuid", Json.Encode.string (Maybe.withDefault "<unknown>" uuid) )
                , ( "details"
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


postNumbersRoundEnd uuid number modeStr =
    let
        body =
            Json.Encode.object
                [ ( "page", Json.Encode.string "numbers" )
                , ( "uuid", Json.Encode.string (Maybe.withDefault "<unknown>" uuid) )
                , ( "details"
                  , Json.Encode.object
                        [ ( "number", Json.Encode.int number )
                        , ( "mode", Json.Encode.string modeStr )
                        ]
                  )
                ]
    in
    Http.post
        { url = eventUrl
        , body = Http.jsonBody body
        , expect = Http.expectWhatever NoOp
        }


type alias GetWordsResponse =
    Result Http.Error Words.WordsFile


getWords =
    Http.task
        { method = "GET"
        , headers = []
        , url = "dictionaries.json"
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| Words.wordsFileDecoder
        , timeout = Nothing
        }


type alias PageStats =
    { dailyCount : Int
    , trainingCount : Int
    , numbersCount : Int
    , activity : List Int
    }


pageStatsDecoder =
    Json.Decode.map4 PageStats
        (Json.Decode.field "daily_count" Json.Decode.int)
        (Json.Decode.field "training_count" Json.Decode.int)
        (Json.Decode.field "numbers_count" Json.Decode.int)
        (Json.Decode.map (Maybe.withDefault []) <| Json.Decode.maybe (Json.Decode.field "page_events_per_day" (Json.Decode.list Json.Decode.int)))


getPageStats =
    Http.task
        { method = "GET"
        , headers = []
        , url = "stats"
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| pageStatsDecoder
        , timeout = Nothing
        }


type alias Error =
    String


handleJsonResponse : Json.Decode.Decoder a -> Http.Response String -> Result Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ _ ->
            Err "Bad URL"

        Http.Timeout_ ->
            Err "Timeout"

        Http.BadStatus_ { statusCode } _ ->
            Err ("Bad status (" ++ String.fromInt statusCode ++ ")")

        Http.NetworkError_ ->
            Err "Network error"

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Err _ ->
                    Err "Bad JSON"

                Ok result ->
                    Ok result
