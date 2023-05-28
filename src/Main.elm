module Main exposing (..)

import Browser
import Element as UI
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Keyed as Keyed
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
    , overview : String
    , genre_ids : List Int
    , poster_path : String
    , release_date : String
    , vote_average : Float
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
    Decode.map7 Movie
        (Decode.field "title" Decode.string)
        (Decode.field "id" Decode.int)
        (Decode.field "overview" Decode.string)
        (Decode.field "genre_ids" (Decode.list Decode.int))
        (Decode.field "poster_path" Decode.string)
        (Decode.field "release_date" Decode.string)
        (Decode.field "vote_average" Decode.float)


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


getPosterUrl : String -> String
getPosterUrl path =
    "https://image.tmdb.org/t/p/w300" ++ path


view : Model -> Html Msg
view model =
    UI.layout []
        (case model.config.error of
            Nothing ->
                if List.isEmpty model.movies then
                    UI.el [] (UI.text "loading...")

                else
                    Keyed.column [] (List.map (\movie -> ( String.fromInt movie.id, UI.el [] (UI.text movie.title) )) model.movies)

            Just errorMessage ->
                UI.el [] (UI.text errorMessage)
        )



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
