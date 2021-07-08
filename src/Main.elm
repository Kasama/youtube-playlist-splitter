module Main exposing (main)

import Browser
import Dict
import Html exposing (Html, button, div, h1, hr, img, input, li, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as JsonPipeline
import List.Split
import Ports
import Task
import Youtube


type alias Model =
    { apiKey : String
    , userAuth : UserAuthentication
    , playlistId : String
    , playlistItems : List Youtube.Video
    , playlistInfo : Maybe String
    , splitIn : Int
    , videos : List Youtube.Video
    , errors : String
    }


type alias UserInfo =
    { userId : String
    , fullName : String
    , email : String
    , accessToken : String
    }


type UserAuthentication
    = Unknown
    | SignedIn UserInfo
    | SignedOut


type Msg
    = APIKey String
    | PlaylistId String
    | SplitIn String
    | GetPlaylistItems
    | GotPlaylistItems (Result Http.Error (List Youtube.Video))
    | AuthStateChanged (Result Decode.Error UserAuthentication)
    | NativeError Decode.Value
    | SignIn
    | SignOut


init : () -> ( Model, Cmd Msg )
init _ =
    ( { apiKey = "<API_KEY>"

      -- , playlistId = "PLv3TTBr1W_9tppikBxAE_G6qjWdBljBHJ" -- regret
      -- , playlistId = "PLNUEwMFEAhOGZ6AjAsBcR-GOFaBkD0cmV" -- sterbilich
      , playlistId = "PLRD7N-Zrj2DN260UFzdCSp1TRg2LYMdYU" -- critical
      , userAuth = Unknown
      , playlistInfo = Nothing
      , errors = ""
      , playlistItems = []
      , splitIn = 100
      , videos = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        APIKey key ->
            ( { model | apiKey = key }, Cmd.none )

        PlaylistId playlist ->
            ( { model | playlistId = playlist }, Cmd.none )

        GetPlaylistItems ->
            ( { model | errors = "", playlistInfo = Just "Loading..." }
            , Task.attempt GotPlaylistItems
                (Youtube.fetchPlaylistItems
                    { key = model.apiKey
                    , playlistId = model.playlistId
                    , pageToken = ""
                    }
                    []
                    |> Task.andThen
                        (\playlist ->
                            Youtube.fetchVideos
                                { key = model.apiKey
                                , videoIds = List.map (\playlistItem -> playlistItem.contentDetails.videoId) playlist
                                , pageToken = ""
                                }
                        )
                )
            )

        GotPlaylistItems (Ok playlist) ->
            ( { model
                | playlistItems = playlist
                , playlistInfo = Just (Debug.toString (List.map (\item -> item.snippet.title) playlist))
              }
            , Cmd.none
            )

        GotPlaylistItems (Err e) ->
            ( { model | errors = Debug.toString e }, Cmd.none )

        SplitIn string ->
            ( { model | splitIn = string |> String.toInt |> Maybe.withDefault 0 }, Cmd.none )

        AuthStateChanged state ->
            case state of
                Ok authState ->
                    ( { model | userAuth = authState }, Cmd.none )

                Err m ->
                    ( { model | errors = m |> Debug.toString }, Cmd.none )

        NativeError value ->
            ( { model | errors = value |> Debug.toString }, Cmd.none )

        SignIn ->
            ( model, Ports.signIn () )

        SignOut ->
            ( model, Ports.signOut () )


view : Model -> Html Msg
view model =
    div [ style "width" "40vw" ]
        [ div [ style "display" "flex", style "flex-direction" "column" ]
            (let
                signedOutPage =
                    [ h1 [] [ text "biiiii" ]
                    , div []
                        [ button [ onClick SignIn ] [ text "Sign in" ]
                        , button [ onClick SignOut ] [ text "Sign out" ]
                        ]
                    ]
             in
             case model.userAuth of
                SignedOut ->
                    signedOutPage

                Unknown ->
                    signedOutPage

                SignedIn u ->
                    [ h1 [] [ text (String.concat [ "errors: ", model.errors ]) ]
                    , h1 [] [ text (String.concat [ "api key: ", model.apiKey ]) ]
                    , h1 [] [ text (String.concat [ "playlist: ", model.playlistId ]) ]
                    , h1 [] [ text (String.concat [ "user: ", u.email ]) ]
                    , input [ placeholder "API Token", value model.apiKey, onInput APIKey ] []
                    , input [ placeholder "Playlist ID", value model.playlistId, onInput PlaylistId ] []
                    , input [ placeholder "Split in", type_ "number", value (model.splitIn |> String.fromInt), onInput SplitIn ] []
                    , hr [ style "width" "100%" ] []
                    , button [ onClick GetPlaylistItems ] [ text "Get playlist info" ]
                    , div [ style "display" "flex", style "flex-direction" "row", style "overflow" "scroll" ]
                        (model.playlistItems
                            |> List.Split.chunksOfLeft model.splitIn
                            |> List.map
                                (\chunk ->
                                    li []
                                        (chunk
                                            |> List.map
                                                (\video ->
                                                    ul []
                                                        [ div [ style "display" "flex" ]
                                                            [ img
                                                                [ src
                                                                    (video.snippet.thumbnails
                                                                        |> Dict.get "default"
                                                                        |> Maybe.map (\thumb -> thumb.url)
                                                                        |> Maybe.withDefault ""
                                                                    )
                                                                , alt video.snippet.title
                                                                ]
                                                                []
                                                            , div [ style "display" "flex", style "flex-direction" "column" ]
                                                                [ span [] [ text video.snippet.title ]
                                                                , span [] [ text video.snippet.description ]
                                                                ]
                                                            ]
                                                        ]
                                                )
                                        )
                                )
                        )
                    ]
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.authStateChanged
            (AuthStateChanged
                << Decode.decodeValue
                    (Decode.oneOf
                        [ Decode.succeed UserInfo
                            |> JsonPipeline.required "userId" Decode.string
                            |> JsonPipeline.required "fullName" Decode.string
                            |> JsonPipeline.required "email" Decode.string
                            |> JsonPipeline.required "accessToken" Decode.string
                            |> Decode.map SignedIn
                        , Decode.succeed SignedOut
                        ]
                    )
            )
        , Ports.nativeError NativeError
        ]


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }
