module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


initModel : Int
initModel = 0 


type Msg = Increment | Decrement

update: Msg -> Int -> Int
update msg model =
    case msg of
        Increment ->
            model + 1
        Decrement ->
            model - 1



view : Int -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]

main =
    Browser.sandbox
        {   init = initModel,
            view  = view,
            update = update
        }