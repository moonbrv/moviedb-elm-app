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
import Svg.Loaders as Loaders



---- MODEL ----


type alias Config =
    { accessToken : String
    , baseUrl : String
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


initialModel : Config -> MoviesState -> Model
initialModel config state =
    { movies = []
    , state = state
    , page = 1
    , totalPages = 1
    , totalResults = 0
    , config = config
    }


type alias Model =
    { config : Config
    , movies : List Movie
    , state : MoviesState
    , page : Int
    , totalPages : Int
    , totalResults : Int
    }


type alias Flags =
    { accessToken : String
    , baseUrl : String
    }


type alias GetMoviesResult =
    { results : List Movie
    , page : Int
    , total_pages : Int
    , total_results : Int
    }


type Msg
    = GetPopularMovies
    | GotPopularMovies (Result Http.Error GetMoviesResult)


type MoviesState
    = Loading
    | Idle
    | Error String
    | Broken String


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
    Decode.map4 GetMoviesResult
        (Decode.field "results" (Decode.list movieDecoder))
        (Decode.field "page" Decode.int)
        (Decode.field "total_pages" Decode.int)
        (Decode.field "total_results" Decode.int)


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
                    Config decodedFlags.accessToken decodedFlags.baseUrl

                model =
                    initialModel config Loading
            in
            ( model
            , getPopularMovies model
            )

        Err error ->
            let
                errorMessage =
                    case handleJsonError error of
                        Just errMessage ->
                            errMessage

                        Nothing ->
                            "Error during parsing flags (configuration)"
            in
            ( initialModel (Config "" "") <| Broken errorMessage
            , Cmd.none
            )



---- UPDATE ----


getPopularMovies : Model -> Cmd Msg
getPopularMovies model =
    Http.request
        { method = "GET"
        , url = model.config.baseUrl ++ "/movie/popular"
        , expect = Http.expectJson GotPopularMovies getMoviesResultDecoder
        , body = Http.emptyBody
        , headers = [ Http.header "Authorization" ("Bearer " ++ model.config.accessToken) ]
        , timeout = Nothing
        , tracker = Nothing
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPopularMovies ->
            ( { model | state = Loading }
            , getPopularMovies model
            )

        GotPopularMovies result ->
            case result of
                Ok data ->
                    ( { model
                        | movies = data.results
                        , state = Idle
                        , totalPages = data.total_pages
                        , totalResults = data.total_results
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | state = Error "Error during getting movies" }
                    , Cmd.none
                    )



---- VIEW ----


color =
    { blue = E.rgb255 0x72 0x9F 0xCF
    , white = E.rgb255 255 255 255
    , red = E.rgb255 0xAA 0x00 0x00
    }


toCssColor255 : E.Color -> String
toCssColor255 elementColor =
    let
        props =
            E.toRgb elementColor

        vals =
            List.map (\c -> String.fromInt <| round (c * 255)) [ props.red, props.green, props.blue, props.alpha ]
    in
    "rgba(" ++ String.join "," vals ++ ")"


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
        color2 =
            E.rgba255 17 17 26 0.1

        shadow =
            Border.shadow
                { offset = ( 0, 0 )
                , size = 0
                , blur = 8
                , color = color2
                }
    in
    E.el [ shadow, Border.rounded 8, E.width E.fill ]
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
        (case model.state of
            Loading ->
                E.el [ E.centerX, E.centerY ] <| E.html <| Loaders.rings [ Loaders.color (toCssColor255 color.blue), Loaders.size 96 ]

            Idle ->
                Keyed.column [ E.spacing 24, E.padding 24 ]
                    (List.map (\movie -> ( String.fromInt movie.id, viewMovieCard movie )) model.movies)

            Error errorMessage ->
                E.column [ E.centerX, E.centerY ]
                    [ E.el [ Font.color color.red, E.padding 12 ] (E.text errorMessage)
                    , Input.button [ E.paddingXY 12 8, E.centerX, E.centerY, Font.color color.white, Background.color color.blue, Border.rounded 4 ]
                        { label = E.text "Retry"
                        , onPress = Just GetPopularMovies
                        }
                    ]

            Broken errorMessage ->
                E.el [ Font.color color.red, E.centerX, E.centerY ] (E.text errorMessage)
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
