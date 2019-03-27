module Main exposing (FlightType(..), Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, button, div, fieldset, input, legend, option, select, text)
import Html.Attributes exposing (class, disabled, min, type_, value)
import Html.Events exposing (onClick, onInput)
import Task
import Time



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


type Model
    = Booking Flight String
    | Booked Flight


type alias Flight =
    { flightType : FlightType
    , departureDay : String
    , returnDay : String
    }


type FlightType
    = OneWay
    | Return


init : () -> ( Model, Cmd Msg )
init _ =
    ( Booking
        (Flight OneWay "" "")
        ""
    , Task.perform GetTimezone Time.here
    )



-- Update


type Msg
    = GetTimezone Time.Zone
    | GetTime Time.Zone Time.Posix
    | SelectFlightType String
    | SetDepartureDay String
    | SetReturnDay String
    | BookFlight


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( _, GetTimezone timezone ) ->
            ( model
            , Task.perform (GetTime timezone) Time.now
            )

        ( Booking flight _, GetTime timezone time ) ->
            let
                year =
                    Time.toYear timezone time

                month =
                    Time.toMonth timezone time
                        |> getMonthNumber

                day =
                    Time.toDay timezone time
                        |> String.fromInt
                        |> String.padLeft 2 '0'

                today =
                    String.fromInt year ++ "-" ++ month ++ "-" ++ day
            in
            ( Booking
                { flight | departureDay = today, returnDay = today }
                today
            , Cmd.none
            )

        ( Booking flight today, SelectFlightType flightType ) ->
            ( Booking
                { flight
                    | flightType =
                        if flightType == "one-way" then
                            OneWay

                        else
                            Return
                }
                today
            , Cmd.none
            )

        ( Booking flight today, SetDepartureDay day ) ->
            ( Booking { flight | departureDay = day } today, Cmd.none )

        ( Booking flight today, SetReturnDay day ) ->
            ( Booking { flight | returnDay = day } today, Cmd.none )

        ( Booking flight _, BookFlight ) ->
            ( Booked flight, Cmd.none )

        ( Booked _, _ ) ->
            ( model, Cmd.none )


getMonthNumber : Time.Month -> String
getMonthNumber month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    case model of
        Booking flight today ->
            fieldset [ class "container" ]
                [ legend [] [ text "Flight Booker" ]
                , select [ onInput SelectFlightType ]
                    [ option [ value "one-way" ] [ text "one-way flight" ]
                    , option [ value "return" ] [ text "return flight" ]
                    ]
                , input
                    [ type_ "date"
                    , value flight.departureDay
                    , min today
                    , onInput SetDepartureDay
                    ]
                    []
                , input
                    [ type_ "date"
                    , value flight.returnDay
                    , min <|
                        if String.isEmpty flight.departureDay then
                            today

                        else
                            flight.departureDay
                    , disabled <| flight.flightType == OneWay
                    , onInput SetReturnDay
                    ]
                    []
                , button
                    [ onClick BookFlight
                    , disabled <| isFormInvalid flight
                    ]
                    [ text "Book" ]
                ]

        Booked flight ->
            let
                flightType =
                    if flight.flightType == OneWay then
                        "one-way"

                    else
                        "return"

                fragment =
                    "You booked a " ++ flightType ++ " flight on " ++ flight.departureDay

                message =
                    if flight.flightType == OneWay then
                        fragment

                    else
                        fragment ++ " to " ++ flight.returnDay
            in
            div [ class "book-text" ]
                [ text message ]


isFormInvalid : Flight -> Bool
isFormInvalid { flightType, departureDay, returnDay } =
    if
        String.isEmpty departureDay
            || (flightType == Return && String.isEmpty returnDay)
            || (flightType == Return && returnDay < departureDay)
    then
        True

    else
        False
