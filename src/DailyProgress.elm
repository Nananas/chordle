module DailyProgress exposing (..)

import Dict exposing (Dict)


type DayProgress
    = Succeeded
    | Failed
    | GiveUp


type alias Progress =
    -- Key is Rata Die format
    Dict Int DayProgress


type GameOverState
    = First DayProgress
    | Other DayProgress
