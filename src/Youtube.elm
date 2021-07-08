module Youtube exposing (ContentDetails, PageInfo, Playlist, PlaylistItem, Video, fetchPlaylistItems, fetchVideos, playlistDecoder)

import Dict exposing (Dict)
import Http
import Json.Decode exposing (Decoder, dict, int, list, maybe, string)
import Json.Decode.Pipeline exposing (optional, required)
import List.Split
import Task exposing (Task)
import Url.Builder


maxPageChunk : number
maxPageChunk =
    50


type alias PageInfo =
    { totalResults : Int
    , resultsPerPage : Int
    }


pageInfoDecoder : Decoder PageInfo
pageInfoDecoder =
    Json.Decode.succeed PageInfo
        |> required "totalResults" int
        |> required "resultsPerPage" int


type alias ContentDetails =
    { videoId : String
    , videoPublishedAt : Maybe String
    }


contentDetailsDecoder : Decoder ContentDetails
contentDetailsDecoder =
    Json.Decode.succeed ContentDetails
        |> required "videoId" string
        |> optional "videoPublishedAt" (maybe string) Nothing


type alias PlaylistItem =
    { kind : String
    , etag : String
    , id : String
    , contentDetails : ContentDetails
    }


playlistItemDecoder : Decoder PlaylistItem
playlistItemDecoder =
    Json.Decode.succeed PlaylistItem
        |> required "kind" string
        |> required "etag" string
        |> required "id" string
        |> required "contentDetails" contentDetailsDecoder


type alias Playlist =
    { kind : String
    , etag : String
    , pageInfo : PageInfo
    , nextPageToken : Maybe String
    , items : List PlaylistItem
    }


playlistDecoder : Decoder Playlist
playlistDecoder =
    Json.Decode.succeed Playlist
        |> required "kind" string
        |> required "etag" string
        |> required "pageInfo" pageInfoDecoder
        |> optional "nextPageToken" (maybe string) Nothing
        |> required "items" (list playlistItemDecoder)


type alias VideoThumbnail =
    { url : String
    , width : Int
    , height : Int
    }


videoThumbnailDecoder : Decoder VideoThumbnail
videoThumbnailDecoder =
    Json.Decode.succeed VideoThumbnail
        |> required "url" string
        |> required "width" int
        |> required "height" int


type alias VideoSnippet =
    { title : String
    , description : String
    , thumbnails : Dict String VideoThumbnail
    }


videoSnippetDecoder : Decoder VideoSnippet
videoSnippetDecoder =
    Json.Decode.succeed VideoSnippet
        |> required "title" string
        |> required "description" string
        |> required "thumbnails" (dict videoThumbnailDecoder)


type alias Video =
    { kind : String
    , etag : String
    , id : String
    , snippet : VideoSnippet
    }


videoDecoder : Decoder Video
videoDecoder =
    Json.Decode.succeed Video
        |> required "kind" string
        |> required "etag" string
        |> required "id" string
        |> required "snippet" videoSnippetDecoder


type alias VideoListResponse =
    { kind : String
    , etag : String
    , pageInfo : PageInfo
    , nextPageToken : Maybe String
    , items : List Video
    }


videoListResponseDecoder : Decoder VideoListResponse
videoListResponseDecoder =
    Json.Decode.succeed VideoListResponse
        |> required "kind" string
        |> required "etag" string
        |> required "pageInfo" pageInfoDecoder
        |> optional "nextPageToken" (maybe string) Nothing
        |> required "items" (list videoDecoder)


handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Err e ->
                    Err (Http.BadBody (body ++ "\n\n" ++ Debug.toString e))

                Ok result ->
                    Ok result


fetchPlaylistItems : { playlistId : String, key : String, pageToken : String } -> List PlaylistItem -> Task Http.Error (List PlaylistItem)
fetchPlaylistItems =
    fetchPaginatedResults fetchPlaylistTask


fetchPaginatedResults : ({ params | pageToken : String } -> Task Http.Error { paginated | nextPageToken : Maybe String, items : List item }) -> { params | pageToken : String } -> List item -> Task Http.Error (List item)
fetchPaginatedResults f params existingItems =
    f params
        |> Task.andThen
            (\page ->
                let
                    allItemsList =
                        List.concat [ existingItems, page.items ]
                in
                case page.nextPageToken of
                    Just nextPageToken ->
                        fetchPaginatedResults
                            f
                            { params | pageToken = nextPageToken }
                            allItemsList

                    Nothing ->
                        Task.succeed allItemsList
            )


fetchPlaylistTask : { playlistId : String, key : String, pageToken : String } -> Task Http.Error Playlist
fetchPlaylistTask { playlistId, key, pageToken } =
    Http.task
        { method = "GET"
        , headers = []
        , url =
            Url.Builder.crossOrigin "https://youtube.googleapis.com/youtube/v3"
                [ "playlistItems" ]
                [ Url.Builder.string "part" "contentDetails"
                , Url.Builder.string "part" "id"
                , Url.Builder.string "maxResults" (maxPageChunk |> String.fromInt)
                , Url.Builder.string "pageToken" pageToken
                , Url.Builder.string "playlistId" playlistId
                , Url.Builder.string "key" key
                ]
        , body = Http.emptyBody
        , timeout = Nothing
        , resolver = Http.stringResolver <| handleJsonResponse <| playlistDecoder
        }


fetchVideos : { videoIds : List String, key : String, pageToken : String } -> Task Http.Error (List Video)
fetchVideos { videoIds, key, pageToken } =
    let
        fetchChunk =
            \chunk ->
                Http.task
                    { method = "GET"
                    , headers = []
                    , url =
                        Url.Builder.crossOrigin "https://youtube.googleapis.com/youtube/v3"
                            [ "videos" ]
                            [ Url.Builder.string "part" "snippet"
                            , Url.Builder.string "id" (String.join "," chunk)
                            , Url.Builder.string "key" key
                            , Url.Builder.string "pageToken" pageToken
                            ]
                    , body = Http.emptyBody
                    , timeout = Nothing
                    , resolver = Http.stringResolver <| handleJsonResponse <| videoListResponseDecoder
                    }
                    |> Task.map (\response -> response.items)
    in
    List.Split.chunksOfLeft
        maxPageChunk
        videoIds
        |> List.map fetchChunk
        |> Task.sequence
        |> Task.map (List.foldr (++) [])
