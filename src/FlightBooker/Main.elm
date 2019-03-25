module Main exposing (FlightType(..), Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, button, div, fieldset, input, legend, option, select, text)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onClick, onInput)



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
    { flightType : FlightType
    , departureDay : String
    , returnDay : String
    , isDepartureDayValid : Bool
    , isReturnDayValid : Bool
    , isFlightBooked : Bool
    }


type FlightType
    = OneWay
    | Return


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model OneWay "" "" True True False, Cmd.none )



-- Update


type Msg
    = SelectFlightType String
    | SetDepartureDay String
    | SetReturnDay String
    | BookFlight


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectFlightType flightType ->
            ( { model
                | flightType =
                    if flightType == "one-way" then
                        OneWay

                    else
                        Return
              }
            , Cmd.none
            )

        SetDepartureDay day ->
            ( { model | departureDay = day }, Cmd.none )

        SetReturnDay day ->
            ( { model | returnDay = day }, Cmd.none )

        BookFlight ->
            ( { model | isFlightBooked = True }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    if model.isFlightBooked then
        let
            flightType =
                if model.flightType == OneWay then
                    "one-way"

                else
                    "return"
        in
        div [] [ text <| "You booked a " ++ flightType ++ " flight on " ++ model.departureDay ]

    else
        fieldset [ class "container" ]
            [ legend [] [ text "Flight Booker" ]
            , select [ onInput SelectFlightType ]
                [ option [ value "one-way" ] [ text "one-way flight" ]
                , option [ value "return" ] [ text "return flight" ]
                ]
            , input [ type_ "date", onInput SetDepartureDay ] []
            , input
                [ type_ "date"
                , onInput SetReturnDay
                , disabled <| model.flightType == OneWay
                ]
                []
            , button
                [ onClick BookFlight -- TO-DO add validation
                ]
                [ text "Book" ]
            ]
