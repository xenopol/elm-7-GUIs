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


type alias Model =
    { timezone : Time.Zone
    , date : String
    , flightType : FlightType
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
    ( Model
        Time.utc
        ""
        OneWay
        ""
        ""
        True
        True
        False
    , Task.perform GetTimezone Time.here
    )



-- Update


type Msg
    = GetTimezone Time.Zone
    | GetTime Time.Posix
    | SelectFlightType String
    | SetDepartureDay String
    | SetReturnDay String
    | BookFlight


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTimezone timezone ->
            ( { model | timezone = timezone }, Task.perform GetTime Time.now )

        GetTime time ->
            let
                year =
                    Time.toYear model.timezone time

                month =
                    Time.toMonth model.timezone time
                        |> getMonthNumber

                day =
                    Time.toDay model.timezone time
                        |> String.fromInt
                        |> String.padLeft 2 '0'

                date =
                    String.fromInt year ++ "-" ++ month ++ "-" ++ day
            in
            ( { model | date = date, departureDay = date, returnDay = date }, Cmd.none )

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
    if model.isFlightBooked then
        let
            flightType =
                if model.flightType == OneWay then
                    "one-way"

                else
                    "return"
        in
        div [ class "book-text" ] [ text <| "You booked a " ++ flightType ++ " flight on " ++ model.departureDay ]

    else
        fieldset [ class "container" ]
            [ legend [] [ text "Flight Booker" ]
            , select [ onInput SelectFlightType ]
                [ option [ value "one-way" ] [ text "one-way flight" ]
                , option [ value "return" ] [ text "return flight" ]
                ]
            , input
                [ type_ "date"
                , value model.departureDay
                , min model.date
                , onInput SetDepartureDay
                ]
                []
            , input
                [ type_ "date"
                , value model.returnDay
                , min <|
                    if String.isEmpty model.departureDay then
                        model.date

                    else
                        model.departureDay
                , disabled <| model.flightType == OneWay
                , onInput SetReturnDay
                ]
                []
            , button
                [ onClick BookFlight
                , disabled <| isFormInvalid model
                ]
                [ text "Book" ]
            ]


isFormInvalid : Model -> Bool
isFormInvalid { flightType, departureDay, returnDay } =
    if
        String.isEmpty departureDay
            || (flightType == Return && String.isEmpty returnDay)
            || (flightType == Return && returnDay < departureDay)
    then
        True

    else
        False
