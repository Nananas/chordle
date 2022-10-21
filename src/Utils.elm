module Utils exposing (..)

import Element exposing (..)
import Html.Events
import Json.Decode


liftModel fn ( model, cmd ) =
    ( fn model, cmd )


liftBoth fnM fnC ( model, cmd ) =
    ( fnM model, Cmd.map fnC cmd )



--


minDesktopViewportWidth =
    800


maxSmallDesktopViewportWidth =
    1200


classifyViewport : { a | width : number, height : number } -> Device
classifyViewport viewport =
    if viewport.width < minDesktopViewportWidth then
        mobile
        --else if viewport.width < maxSmallDesktopViewportWidth then

    else
        desktop


desktop =
    { class = Desktop, orientation = Landscape }


mobile =
    { class = Phone, orientation = Portrait }


isOnDesktop : Device -> Bool
isOnDesktop device =
    case device.class of
        Phone ->
            False

        _ ->
            True


isOnMobile : Device -> Bool
isOnMobile device =
    not <| isOnDesktop device



--


onEnter : msg -> Element.Attribute msg
onEnter msg =
    htmlAttribute
        (Html.Events.on "keyup"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.Decode.succeed msg

                        else
                            Json.Decode.fail "Not the enter key"
                    )
            )
        )


indexedFind : (a -> Bool) -> List a -> Maybe ( Int, a )
indexedFind =
    indexedFindHelp 0


indexedFindHelp : Int -> (a -> Bool) -> List a -> Maybe ( Int, a )
indexedFindHelp index predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just ( index, x )

            else
                indexedFindHelp (index + 1) predicate xs
