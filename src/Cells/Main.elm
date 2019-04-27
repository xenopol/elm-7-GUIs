module Main exposing (Model, Msg(..), init, main, update)

import Browser
import Browser.Dom
import Dict exposing (Dict)
import Html exposing (Attribute, Html, fieldset, input, legend, span, td, text, th)
import Html.Attributes exposing (class, classList, id, scope, value)
import Html.Events exposing (on, onClick, onDoubleClick, stopPropagationOn)
import Html.Keyed
import Html.Lazy
import Json.Decode
import Task



-- Main


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Model


type alias Model =
    { cells : Cells
    , activeCellId : Maybe String
    }


type alias Cells =
    Dict String String


rowsData : List String
rowsData =
    List.range -1 99
        |> List.map String.fromInt


columnsData : List String
columnsData =
    List.range 64 90
        |> List.map (Char.fromCode >> String.fromChar)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Dict.empty Nothing, Cmd.none )


getCellValueOrDefault : String -> Cells -> String
getCellValueOrDefault id cells =
    Dict.get id cells |> Maybe.withDefault ""



-- Update


type Msg
    = SelectCell String
    | ClearActiveCell
    | EditCell String String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectCell id ->
            ( { model | activeCellId = Just id }
            , Browser.Dom.focus "active-input" |> Task.attempt (\_ -> NoOp)
            )

        EditCell key value ->
            if String.length key == 1 then
                ( { model | cells = getUpdatedCells (value ++ key) model }
                , Cmd.none
                )

            else if key == "Escape" || key == "Enter" then
                ( { model | activeCellId = Nothing }, Cmd.none )

            else if key == "Backspace" then
                ( { model | cells = getUpdatedCells (String.dropRight 1 value) model }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ClearActiveCell ->
            ( { model | activeCellId = Nothing }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


getUpdatedCells : String -> Model -> Cells
getUpdatedCells newValue { activeCellId, cells } =
    case activeCellId of
        Just id ->
            Dict.insert id newValue cells

        Nothing ->
            cells


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view {activeCellId, cells} =
    fieldset [ class "container", onClick ClearActiveCell ]
        [ legend [] [ text "Cells" ]
        , viewTable activeCellId cells
        ]


viewTable : Maybe String -> Cells -> Html Msg
viewTable activeCellId cells =
    Html.Keyed.node "table" [] <|
        List.indexedMap
            (\i row ->
                ( String.fromInt i
                , Html.Lazy.lazy4 viewTableRow i row activeCellId cells
                )
            )
            rowsData


viewTableRow : Int -> String -> Maybe String -> Cells -> Html Msg
viewTableRow i row activeCellId cells =
    Html.Keyed.node "tr" [] <|
        List.indexedMap
            (\j column ->
                ( String.fromInt j
                , Html.Lazy.lazy6 viewTableData activeCellId cells row i j column
                )
            )
            columnsData


viewTableData : Maybe String -> Cells -> String -> Int -> Int -> String -> Html Msg
viewTableData activeCellId cells row i j column =
    if i == 0 && j == 0 then
        td [] []

    else if i == 0 then
        th [ scope "col" ] [ text column ]

    else if j == 0 then
        th [ scope "row" ] [ text row ]

    else
        let
            cellId =
                column ++ "-" ++ row

            isCellActive =
                Maybe.withDefault "" activeCellId == cellId

            cellValue =
                getCellValueOrDefault cellId cells
        in
        Html.Lazy.lazy3 getTableData isCellActive cellId cellValue


getTableData : Bool -> String -> String -> Html Msg
getTableData isCellActive cellId cellValue =
    td
        [ classList [ ( "active", isCellActive ) ]
        , onDoubleClick <| SelectCell cellId
        ]
    <|
        if isCellActive then
            [ input
                [ id "active-input"
                , onKeyDown EditCell
                , stopPropagationOn "click" <| Json.Decode.succeed ( NoOp, True )
                , value cellValue
                ]
                []
            ]

        else
            [ parseCellValue cellValue ]


onKeyDown : (String -> String -> Msg) -> Attribute Msg
onKeyDown msg =
    on "keydown" <|
        Json.Decode.map2
            msg
            (Json.Decode.field "key" Json.Decode.string)
            (Json.Decode.at [ "target", "value" ] Json.Decode.string)


parseCellValue : String -> Html Msg
parseCellValue value_ =
    let
        value =
            String.trim value_

        isFormula =
            String.startsWith "=" value && String.length value > 1
    in
    if isFormula then
        span [ class "formula" ] [ text <| calculateCellValue value ]

    else
        span [] [ text value ]


calculateCellValue : String -> String
calculateCellValue value =
    case String.split "=" value of
        [] ->
            value

        head :: tail ->
            List.head tail |> Maybe.withDefault ""
