module Main exposing (..)

import Browser
import Html exposing (Html, div, text)
import HW1_Def exposing (Bag, insert)
import Debug

testBag: Bag String
testBag =
    insert "Nadir" [ ("Nadir", 1), ("William", 2)]


main: Program () () ()

main = 
    Browser.sandbox
    {
        init = ()
        , update = \_ model -> model
        -- \_ says  ia m getting a value but since it is not important to me, ignore it
        , view = \_ -> div [] [ text( Debug.toString testBag)]

    }