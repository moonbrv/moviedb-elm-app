module Main exposing (..)

import Browser
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
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


color =
    { blue = E.rgb255 0x72 0x9F 0xCF
    , white = E.rgb255 255 255 255
    }


getPosterUrl : String -> String
getPosterUrl path =
    "https://image.tmdb.org/t/p/w300" ++ path


getMovieTitle : Movie -> String
getMovieTitle movie =
    let
        year =
            String.split "-" movie.release_date |> List.head
    in
    case year of
        Just y ->
            movie.title ++ " (" ++ y ++ ")"

        Nothing ->
            movie.title


viewMovieCard : Movie -> E.Element msg
viewMovieCard movie =
    let
        color1 =
            E.rgba255 17 17 26 0.05

        color2 =
            E.rgba255 17 17 26 0.1

        shadow1 =
            Border.shadow
                { offset = ( 0, 1 )
                , size = 0
                , blur = 0
                , color = color1
                }

        shadow2 =
            Border.shadow
                { offset = ( 0, 0 )
                , size = 0
                , blur = 8
                , color = color2
                }
    in
    E.el [ shadow1, shadow2, Border.rounded 8, E.width E.fill ]
        (E.row []
            [ E.image
                [ Border.roundEach
                    { topLeft = 8
                    , topRight = 0
                    , bottomLeft = 8
                    , bottomRight = 0
                    }
                ]
                { src = getPosterUrl movie.poster_path
                , description = movie.title
                }
            , E.column [ E.width E.fill, E.height E.fill, E.padding 12 ]
                [ E.textColumn [ E.alignTop, E.spacing 12 ]
                    [ E.paragraph [] [ E.el [ Font.semiBold ] (E.text (getMovieTitle movie)) ]
                    , E.paragraph [] [ E.text movie.overview ]
                    ]
                , Input.button [ E.paddingXY 12 8, E.alignRight, E.alignBottom, Font.color color.white, Background.color color.blue, Border.rounded 4 ]
                    { label = E.text "Details"
                    , onPress = Nothing
                    }
                ]
            ]
        )


view : Model -> Html Msg
view model =
    E.layout []
        (case model.config.error of
            Nothing ->
                if List.isEmpty model.movies then
                    E.el [] (E.text "loading...")

                else
                    Keyed.column [ E.spacing 24, E.padding 24 ]
                        (List.map (\movie -> ( String.fromInt movie.id, viewMovieCard movie )) model.movies)

            Just errorMessage ->
                E.el [] (E.text errorMessage)
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
