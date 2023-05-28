module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Keyed as Keyed
import Http
import Json.Decode as Decode



---- MODEL ----


type alias Config =
    { accessToken : String
    , baseUrl : String
    , error : Maybe String
    }


type alias Movie =
    { title : String
    , id : Int
    }


type alias Model =
    { config : Config
    , movies : List Movie
    }


type alias Flags =
    { accessToken : String
    , baseUrl : String
    }


type alias GetMoviesResult =
    { results : List Movie }


type Msg
    = GetPopularMovies
    | GotPopularMovies (Result Http.Error GetMoviesResult)
    | ErrorGettingMovies


movieDecoder : Decode.Decoder Movie
movieDecoder =
    Decode.map2 Movie
        (Decode.field "title" Decode.string)
        (Decode.field "id" Decode.int)


getMoviesResultDecoder : Decode.Decoder GetMoviesResult
getMoviesResultDecoder =
    Decode.map GetMoviesResult
        (Decode.field "results" (Decode.list movieDecoder))


flagsDecoder : Decode.Decoder Flags
flagsDecoder =
    Decode.map2 Flags
        (Decode.field "accessToken" Decode.string)
        (Decode.field "baseUrl" Decode.string)


handleJsonError : Decode.Error -> Maybe String
handleJsonError error =
    Just (Decode.errorToString error)


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue flagsDecoder flags of
        Ok decodedFlags ->
            let
                config =
                    Config decodedFlags.accessToken decodedFlags.baseUrl Nothing
            in
            ( Model config []
            , getPopularMovies config
            )

        Err error ->
            ( Model (Config "" "" (handleJsonError error)) []
            , Cmd.none
            )



---- UPDATE ----


getPopularMovies : Config -> Cmd Msg
getPopularMovies config =
    Http.request
        { method = "GET"
        , url = config.baseUrl ++ "/movie/popular"
        , expect = Http.expectJson GotPopularMovies getMoviesResultDecoder
        , body = Http.emptyBody
        , headers = [ Http.header "Authorization" ("Bearer " ++ config.accessToken) ]
        , timeout = Nothing
        , tracker = Nothing
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPopularMovies ->
            ( model, getPopularMovies model.config )

        GotPopularMovies result ->
            case result of
                Ok data ->
                    ( { model | movies = data.results }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.config.error of
        Nothing ->
            if List.isEmpty model.movies then
                div [] [ text "loading..." ]

            else
                Keyed.node "div" [] (List.map (\movie -> ( String.fromInt movie.id, div [] [ text movie.title ] )) model.movies)

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
