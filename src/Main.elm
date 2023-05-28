module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import Json.Decode as Decode


---- MODEL ----


type alias Model =
    { accessToken: String
    , baseUrl: String
    , error: Maybe String
    }

type alias Flags =
    { accessToken: String
    , baseUrl: String
    }

flagsDecoder: Decode.Decoder Flags
flagsDecoder =
    Decode.map2 Flags
        (Decode.field "accessToken" Decode.string)
        (Decode.field "baseUrl" Decode.string)

handleJsonError : Decode.Error -> Maybe String
handleJsonError error = Just (Decode.errorToString error)


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue flagsDecoder flags of
        Ok decodedFlags ->
            ( Model decodedFlags.accessToken decodedFlags.baseUrl Nothing
            , Cmd.none
            )

        Err error ->
            ( Model "" "" (handleJsonError error)
            , Cmd.none
            )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.error of
        Nothing  ->
            div []
                [ img [ src "/logo.svg" ] []
                , h1 [] [ text "Your Elm App is working!" ]
                , div [] [ text model.baseUrl ]
                ]
        Just errorMessage ->
            div [] [ text errorMessage ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


---- PROGRAM ----


main : Program Decode.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
