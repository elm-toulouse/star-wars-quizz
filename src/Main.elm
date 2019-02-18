module Main exposing (main)

import Browser exposing (Document, document)
import Html exposing (..)


main : Program Flags Model Msg
main =
    document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    ()


type alias Msg =
    ()


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ _ =
    ( (), Cmd.none )


view : Model -> Document Msg
view _ =
    let
        title =
            "Elm Toulouse #2"

        body =
            []
    in
    { title = title, body = body }
