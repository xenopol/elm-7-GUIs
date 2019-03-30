module Main exposing (main)

import Browser
import Html exposing (Html, button, div, fieldset, input, legend, progress, text)
import Html.Attributes exposing (class, max, min, type_, value)
import Html.Events exposing (onClick, onInput)
import Time


timerMaxValue : Int
timerMaxValue =
    30



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Model


type alias Model =
    { timer : Float
    , range : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 (toFloat timerMaxValue / 2), Cmd.none )



-- Update


type Msg
    = Tick Time.Posix
    | SetTimerRange String
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model
                | timer =
                    (model.timer
                        |> (+) 0.1
                        |> (*) 10
                        |> round
                        |> toFloat
                    )
                        / 10
              }
            , Cmd.none
            )

        SetTimerRange value ->
            ( { model | range = String.toFloat value |> Maybe.withDefault 0 }
            , Cmd.none
            )

        Reset ->
            ( { model | timer = 0 }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions { timer, range } =
    if timer < range then
        Time.every 100 Tick

    else
        Sub.none



-- View


view : Model -> Html Msg
view { timer, range } =
    fieldset [ class "container" ]
        [ legend [] [ text "Flight Booker" ]
        , div [ class "progress" ]
            [ text "Elapsed Time:"
            , progress
                [ max <| String.fromFloat range
                , value <| String.fromFloat timer
                ]
                []
            ]
        , div [ class "timer" ] [ text <| "⏱ " ++ getTimer timer ]
        , div [ class "duration" ]
            [ text "Duration:"
            , input
                [ type_ "range"
                , min "1"
                , max <| String.fromInt timerMaxValue
                , value <| String.fromFloat range
                , onInput SetTimerRange
                ]
                []
            ]
        , button [ onClick Reset ] [ text "Reset" ]
        ]



-- If timer is a whole number: 5, add '.0' at the end,
-- in order to always have the same width/nr. of decimals.
-- Otherwise, the timer will jump from '5 s' to '5.01 s'


getTimer : Float -> String
getTimer number =
    let
        timer =
            String.fromFloat number

        timerLength =
            String.length timer

        roundedTimerLength =
            number
                |> round
                |> String.fromInt
                |> String.length
    in
    if timerLength == roundedTimerLength then
        timer ++ ".0 s"

    else
        timer ++ " s"
