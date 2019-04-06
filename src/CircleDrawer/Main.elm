module Main exposing (Change(..), Changes, Circle, Model, Msg(..), defaultCircleSize, getCircles, init, main, onClickPosition, onClickStopPropagation, update, view)

import Browser
import Html exposing (Attribute, Html, button, div, fieldset, input, legend, text)
import Html.Attributes exposing (class, disabled, max, min, style, type_, value)
import Html.Events exposing (on, onClick, onInput, stopPropagationOn)
import Json.Decode



-- Main


main : Platform.Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- Model


type alias Model =
    { changes : Changes
    , inactiveChanges : Changes
    , selectedCircle : Maybe Circle
    , idCounter : Int
    , isResizing : Bool
    }


type alias Changes =
    List Change


type alias Circle =
    { id : Int
    , size : String
    , xPos : String
    , yPos : String
    }


type Change
    = DrawCircle Circle
    | ResizeCircle Int String


init : Model
init =
    Model [] [] Nothing 0 False


defaultCircleSize : String
defaultCircleSize =
    "50"



-- Update


type Msg
    = Draw ( String, String )
    | Select Int
    | Resize String
    | Undo
    | Redo


update : Msg -> Model -> Model
update msg ({ changes, inactiveChanges, idCounter, isResizing, selectedCircle } as model) =
    case msg of
        Draw ( xPos, yPos ) ->
            let
                hasCircleSelected =
                    case selectedCircle of
                        Just _ ->
                            True

                        _ ->
                            False
            in
            if isResizing then
                { model
                    | changes = addResizeCircle model
                    , isResizing = False
                    , selectedCircle = Nothing
                    , inactiveChanges = []
                }

            else if hasCircleSelected then
                { model | selectedCircle = Nothing }

            else
                { model
                    | changes = addDrawCircle xPos yPos model
                    , idCounter = idCounter + 1
                    , inactiveChanges = []
                }

        Select id ->
            if isResizing then
                { model
                    | changes = addResizeCircle model
                    , selectedCircle = findCircle id changes
                    , isResizing = False
                }

            else
                { model | selectedCircle = findCircle id changes }

        Resize size ->
            { model
                | changes = resizeCircle size selectedCircle changes
                , isResizing = True
            }

        Undo ->
            case changes of
                [] ->
                    { model | changes = [] }

                ((DrawCircle _) as head) :: tail ->
                    { model | changes = tail, inactiveChanges = head :: inactiveChanges }

                (ResizeCircle id size) :: tail ->
                    let
                        lastSize =
                            findCircle id changes
                                |> Maybe.map .size
                                |> Maybe.withDefault defaultCircleSize
                    in
                    { model
                        | changes = resizeCircle size (findCircle id tail) tail
                        , inactiveChanges = ResizeCircle id lastSize :: inactiveChanges
                    }

        Redo ->
            case inactiveChanges of
                [] ->
                    { model | inactiveChanges = [] }

                ((DrawCircle _) as head) :: tail ->
                    { model | changes = head :: changes, inactiveChanges = tail }

                (ResizeCircle id size) :: tail ->
                    let
                        lastSize =
                            findCircle id changes
                                |> Maybe.map .size
                                |> Maybe.withDefault defaultCircleSize
                    in
                    { model
                        | changes =
                            resizeCircle
                                size
                                (findCircle id changes)
                                (ResizeCircle id lastSize :: changes)
                        , inactiveChanges = tail
                    }


addDrawCircle : String -> String -> Model -> Changes
addDrawCircle xPos yPos { idCounter, changes } =
    DrawCircle (Circle idCounter defaultCircleSize xPos yPos) :: changes


addResizeCircle : Model -> Changes
addResizeCircle { changes, selectedCircle } =
    case selectedCircle of
        Just { id, size } ->
            ResizeCircle id size :: changes

        _ ->
            changes


findCircle : Int -> Changes -> Maybe Circle
findCircle id_ changes =
    changes
        |> List.filter
            (\change ->
                case change of
                    DrawCircle { id } ->
                        id == id_

                    _ ->
                        False
            )
        |> List.head
        |> Maybe.andThen
            (\change ->
                case change of
                    DrawCircle circle ->
                        Just circle

                    _ ->
                        Nothing
            )


resizeCircle : String -> Maybe Circle -> Changes -> Changes
resizeCircle size selectedCircle changes =
    List.map
        (\change ->
            case ( change, selectedCircle ) of
                ( DrawCircle { id, xPos, yPos }, Just selectedCircle_ ) ->
                    if id == selectedCircle_.id then
                        DrawCircle <| Circle id size xPos yPos

                    else
                        change

                ( _, _ ) ->
                    change
        )
        changes



-- View


view : Model -> Html Msg
view ({ changes, inactiveChanges } as model) =
    fieldset [ class "container" ]
        [ legend [] [ text "Circle Drawer" ]
        , div [ class "header" ]
            [ button [ onClick Undo, disabled <| List.isEmpty changes ] [ text "Undo" ]
            , button [ onClick Redo, disabled <| List.isEmpty inactiveChanges ] [ text "Redo" ]
            ]
        , div [ class "drawing-area", onClickPosition ] <| getCircles model.changes
        , case model.selectedCircle of
            Just { xPos, yPos } ->
                div [ class "resize-container" ]
                    [ text <| "Adjust diameter of circle at (" ++ xPos ++ ", " ++ yPos ++ ")."
                    , input
                        [ type_ "range"
                        , min "10"
                        , max "200"
                        , value <| getCircleSize model
                        , onInput Resize
                        ]
                        []
                    ]

            Nothing ->
                div [ class "resize-container" ] []
        ]


getCircles : Changes -> List (Html Msg)
getCircles =
    List.map
        (\change ->
            case change of
                DrawCircle { id, xPos, yPos, size } ->
                    div
                        [ class "circle"
                        , style "top" <| yPos ++ "px"
                        , style "left" <| xPos ++ "px"
                        , style "width" <| size ++ "px"
                        , style "height" <| size ++ "px"
                        , onClickStopPropagation id
                        ]
                        []

                ResizeCircle _ _ ->
                    text ""
        )


getCircleSize : Model -> String
getCircleSize { changes, selectedCircle } =
    case selectedCircle of
        Just { id } ->
            findCircle id changes
                |> Maybe.map .size
                |> Maybe.withDefault defaultCircleSize

        _ ->
            defaultCircleSize


onClickPosition : Attribute Msg
onClickPosition =
    on "click" <|
        Json.Decode.map2
            (\xPos yPos -> Draw ( String.fromInt xPos, String.fromInt yPos ))
            (Json.Decode.field "clientX" Json.Decode.int)
            (Json.Decode.field "clientY" Json.Decode.int)


onClickStopPropagation : Int -> Attribute Msg
onClickStopPropagation id =
    stopPropagationOn "click" <|
        Json.Decode.map
            (\msg -> ( msg, True ))
            (Json.Decode.succeed <| Select id)
