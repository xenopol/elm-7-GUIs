module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, fieldset, input, label, legend, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onInput)



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
    { celsius : String
    , fahrenheit : String
    }


init : Model
init =
    Model "" ""



-- Update


type Msg
    = ConvertCelsius String
    | ConvertFahrenheit String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ConvertCelsius value ->
            { model
                | celsius = value
                , fahrenheit =
                    value
                        |> convertCToF
                        |> getTempWithDefault model.fahrenheit
            }

        ConvertFahrenheit value ->
            { model
                | fahrenheit = value
                , celsius =
                    value
                        |> convertFToC
                        |> getTempWithDefault model.celsius
            }


convertCToF : String -> Maybe Float
convertCToF temp =
    String.toFloat temp
        |> Maybe.map (\n -> (n * 9 / 5) + 32)


convertFToC : String -> Maybe Float
convertFToC temp =
    String.toFloat temp
        |> Maybe.map (\n -> (n - 32) * 5 / 9)


getTempWithDefault : String -> Maybe Float -> String
getTempWithDefault default temp =
    temp
        |> Maybe.map String.fromFloat
        |> Maybe.withDefault default



-- View


view : Model -> Html Msg
view { celsius, fahrenheit } =
    fieldset [ class "container" ]
        [ legend [] [ text "Temperature Converter" ]
        , div
            [ class "input-container" ]
            [ label [] [ text "Celsius" ]
            , input [ type_ "text", onInput ConvertCelsius, value celsius ] []
            ]
        , div [ class "separator" ] [ text "⇄" ]
        , div [ class "input-container" ]
            [ label [] [ text "Fahrenheit" ]
            , input [ type_ "text", onInput ConvertFahrenheit, value fahrenheit ] []
            ]
        ]
