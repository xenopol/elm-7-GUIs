module Main exposing (Model, Msg(..), init, main, update)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)



-- Main


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- Model


type alias Model =
    Int


init : Model
init =
    0



-- Update


type Msg
    = Increment
    | Decrement
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1

        Reset ->
            0



-- View


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "wrapper" ]
            [ button [ class "button", onClick Decrement ] [ text "-" ]
            , div [ class "counter" ] [ text <| String.fromInt model ]
            , button [ class "button", onClick Increment ] [ text "+" ]
            ]
        , button [ class "button", onClick Reset ] [ text "Reset" ]
        ]
