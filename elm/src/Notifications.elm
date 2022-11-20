port module Notifications exposing (..)


port requestPermissions : () -> Cmd msg


port showNotification : String -> Cmd msg


port permissionChanged : (String -> msg) -> Sub msg
