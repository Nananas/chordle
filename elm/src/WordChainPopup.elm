module WordChainPopup exposing (..)

import Browser.Events
import Json.Decode


type Msg
    = OnMouseEnterCharacter ( Int, Int )
    | OnMouseLeaveCharacter
    | OnShiftKey Bool


type State
    = ShowNoPopup
    | ShowPopupOf ( Int, Int )
    | ShowPopupOfAll (Maybe ( Int, Int )) -- Keep track of previous state


toMaybe popupState =
    case popupState of
        ShowPopupOf ( wid, i ) ->
            Just ( wid, i )

        ShowPopupOfAll x ->
            x

        _ ->
            Nothing


update : Msg -> State -> State
update msg popupState =
    case msg of
        OnMouseEnterCharacter id ->
            ShowPopupOf id

        OnMouseLeaveCharacter ->
            ShowNoPopup

        OnShiftKey True ->
            ShowPopupOfAll (toMaybe popupState)

        OnShiftKey False ->
            toMaybe popupState
                |> Maybe.map ShowPopupOf
                |> Maybe.withDefault ShowNoPopup


shiftKeyDecoder : Bool -> Json.Decode.Decoder Msg
shiftKeyDecoder state =
    Json.Decode.andThen
        (\key ->
            if key == "Shift" then
                Json.Decode.succeed (OnShiftKey state)

            else
                Json.Decode.fail "other"
        )
        (Json.Decode.field "key" Json.Decode.string)


subscriptions =
    Sub.batch
        [ Browser.Events.onKeyDown (shiftKeyDecoder True)
        , Browser.Events.onKeyUp (shiftKeyDecoder False)
        ]
