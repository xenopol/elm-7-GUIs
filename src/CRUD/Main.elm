module Main exposing (Model, Msg(..), Names, getNames, handleOnClick, init, main, subscriptions, update, view)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, button, div, fieldset, input, legend, text)
import Html.Attributes exposing (attribute, class, classList, disabled, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import Json.Decode



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


type alias Names =
    Dict String ( String, String )


type alias Model =
    { names : Names
    , id : String
    , firstName : String
    , lastName : String
    , search : String
    }


defaultNames : Names
defaultNames =
    Dict.fromList
        [ ( "HansEmil", ( "Hans", "Emil" ) )
        , ( "MaxMustermann", ( "Max", "Mustermann" ) )
        , ( "RomanTisch", ( "Roman", "Tisch" ) )
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model defaultNames "" "" "" "", Cmd.none )



-- Update


type Msg
    = Create
    | Update
    | Delete
    | Search String
    | SetFirstName String
    | SetLastName String
    | Click ( Maybe String, String )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Create ->
            if isStringValid model.firstName && isStringValid model.lastName then
                ( { model | names = addNewName model }, Cmd.none )

            else
                ( model, Cmd.none )

        Update ->
            ( { model | names = updateName model }, Cmd.none )

        Delete ->
            ( { model | names = Dict.remove model.id model.names }, Cmd.none )

        Search value ->
            ( { model | search = value }, Cmd.none )

        SetFirstName name ->
            ( { model | firstName = name }, Cmd.none )

        SetLastName name ->
            ( { model | lastName = name }, Cmd.none )

        Click ( id_, nodeName ) ->
            case id_ of
                Just id ->
                    let
                        ( firstName, lastName ) =
                            Dict.get id model.names
                                |> Maybe.withDefault ( "", "" )
                    in
                    ( { model | firstName = firstName, lastName = lastName, id = id }, Cmd.none )

                Nothing ->
                    if nodeName == "INPUT" then
                        ( model, Cmd.none )

                    else
                        ( { model | firstName = "", lastName = "", id = "" }, Cmd.none )


isStringValid : String -> Bool
isStringValid =
    String.trim >> String.isEmpty >> not


addNewName : Model -> Names
addNewName { names, firstName, lastName } =
    Dict.insert
        (lastName ++ firstName)
        ( firstName, lastName )
        names


updateName : Model -> Names
updateName { id, firstName, lastName, names } =
    Dict.update
        id
        (Maybe.map (\_ -> ( firstName, lastName )))
        names



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onClick handleOnClick


handleOnClick : Json.Decode.Decoder Msg
handleOnClick =
    Json.Decode.map2 (\id nodeName -> Click ( id, nodeName ))
        (Json.Decode.maybe <|
            Json.Decode.at [ "target", "dataset", "id" ] Json.Decode.string
        )
        (Json.Decode.at [ "target", "nodeName" ] Json.Decode.string)



--View


view : Model -> Html Msg
view model =
    fieldset [ class "container" ]
        [ legend [] [ text "Flight Booker" ]
        , div [ class "header" ]
            [ text "Filter prefix:"
            , input [ onInput Search ] []
            ]
        , div [ class "body" ]
            [ Html.Keyed.ul [ class "names" ] <|
                getNames model.id <|
                    filterNames model.search model.names
            , div [ class "actions" ]
                [ div [ class "actions-first" ]
                    [ text "Name:"
                    , input [ onInput SetFirstName, value model.firstName ] []
                    ]
                , div [ class "actions-last" ]
                    [ text "Surname:"
                    , input [ onInput SetLastName, value model.lastName ] []
                    ]
                ]
            ]
        , div [ class "footer" ]
            [ button
                [ onClick Create
                , disabled <| isButtonDisabled model || not (String.isEmpty model.id)
                ]
                [ text "Create" ]
            , button
                [ onClick Update
                , disabled <| String.isEmpty model.id || isButtonDisabled model
                ]
                [ text "Update" ]
            , button
                [ onClick Delete
                , disabled <| String.isEmpty model.id || isButtonDisabled model
                ]
                [ text "Delete" ]
            ]
        ]


isButtonDisabled : Model -> Bool
isButtonDisabled { firstName, lastName } =
    isStringValid firstName && isStringValid lastName |> not


getNames : String -> Names -> List ( String, Html Msg )
getNames id names =
    names
        |> Dict.toList
        |> List.map
            (\( nameId, ( firstName, lastName ) ) ->
                ( firstName ++ lastName
                , div
                    [ classList [ ( "name", True ), ( "active", nameId == id ) ]
                    , attribute "data-id" nameId
                    ]
                    [ text <| lastName ++ ", " ++ firstName ]
                )
            )


filterNames : String -> Names -> Names
filterNames value names =
    if isStringValid value then
        names
            |> Dict.filter
                (\_ ( _, lastName ) ->
                    let
                        searchValue =
                            value
                                |> String.trim
                                |> String.toLower
                    in
                    lastName
                        |> String.toLower
                        |> String.contains searchValue
                )

    else
        names
