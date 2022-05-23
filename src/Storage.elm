port module Storage exposing (..)

import Json.Decode


type alias Storage =
    { name : String
    , json : Json.Decode.Value
    }


port setStorage : Storage -> Cmd msg


port loadStorage : String -> Cmd msg


port storageLoaded : (Storage -> msg) -> Sub msg
