port module Main exposing (Model, Msg(..), Video, channelVideos, init, main, update, view, viewChannel, viewChannelColumn, viewVideoPlayer, viewVideoThumbnail)

import Array exposing (get)
import Browser
import Browser.Dom
import Channel exposing (Channel)
import Env exposing (apiKey)
import Html exposing (Html, aside, button, div, form, h1, h2, h3, iframe, img, input, main_, span)
import Html.Attributes exposing (attribute, class, id, name, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Iso8601
import Json.Decode as D
import Json.Encode as E
import Task
import Time
import Url
import Url.Builder
import Url.Parser exposing ((</>))



-- MAIN


main : Program E.Value Model Msg
main =
    Browser.document
        { init = init
        , update = updateWithStorage
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Video =
    { channel : Channel
    , id : String
    , publishedAt : Time.Posix
    , thumbnailUrl : String
    , title : String
    , description : String
    }


type alias Model =
    { videos : List Video
    , currentVideo : Maybe Video
    , newChannelUrl : String
    , errorMsg : Maybe String
    , channels : List Channel
    , seenVideos : List String
    }


decodeFlags : E.Value -> Model
decodeFlags flags =
    case D.decodeValue modelDecoder flags of
        Ok model ->
            model

        Err err ->
            { channels = [], videos = [], newChannelUrl = "", errorMsg = Just (D.errorToString err), currentVideo = Nothing, seenVideos = [] }


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( flags |> decodeFlags
    , Cmd.batch
        (List.map
            getChannelVideos
            (flags |> decodeFlags |> .channels)
        )
    )



-- UPDATE


type Msg
    = AddChannel
    | UpdateNewChannel String
    | RemoveChannel Channel
    | WatchVideo Video
    | ExitVideo
    | FinishVideo Video
    | LoadedChannelInfo (Result Http.Error Channel)
    | LoadedChannelVideos (Result Http.Error (List Video))
    | LoadedChannelId (Result Http.Error Channel.Id)
    | Scroll (Result Browser.Dom.Error ())
    | ClearError Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddChannel ->
            case Channel.parseChannelUrl model.newChannelUrl of
                Nothing ->
                    ( { model | newChannelUrl = "", errorMsg = Just ("'" ++ model.newChannelUrl ++ "' isn't a valid YouTube URL.") }
                    , Cmd.none
                    )

                -- need to send a request to get the channel id
                Just (Channel.DisplayName name) ->
                    ( { model | newChannelUrl = "" }, getChannelId name )

                Just (Channel.ChannelId id) ->
                    ( { model | newChannelUrl = "" }, getChannelInfo (Channel.Id id) )

        UpdateNewChannel newChannelUrl ->
            ( { model | newChannelUrl = newChannelUrl }, Cmd.none )

        RemoveChannel channel ->
            ( { model | channels = List.filter (\c -> c /= channel) model.channels }, Cmd.none )

        WatchVideo video ->
            ( { model | currentVideo = Just video }, scrollToTop )

        ExitVideo ->
            ( { model | currentVideo = Nothing }, Cmd.none )

        FinishVideo video ->
            ( { model
                | seenVideos = video.id :: model.seenVideos
                , currentVideo = Nothing
              }
            , Cmd.none
            )

        LoadedChannelId result ->
            case result of
                Ok id ->
                    ( model, getChannelInfo id )

                Err err ->
                    ( { model | errorMsg = Just (httpErrorToString err) }, Cmd.none )

        LoadedChannelVideos result ->
            case result of
                Ok videos ->
                    ( { model | videos = model.videos ++ videos }, Cmd.none )

                Err err ->
                    ( { model | errorMsg = Just (httpErrorToString err) }, Cmd.none )

        LoadedChannelInfo result ->
            case result of
                Ok channel ->
                    if List.member channel model.channels then
                        ( { model | errorMsg = Just ("Already subscribed to " ++ channel.title ++ ".") }, Cmd.none )

                    else
                        ( { model | channels = List.filter (\c -> c.id /= channel.id) model.channels ++ [ channel ] }, getChannelVideos channel )

                Err err ->
                    ( { model | errorMsg = Just (httpErrorToString err) }, Cmd.none )

        Scroll _ ->
            ( model, Cmd.none )

        ClearError _ ->
            ( { model | errorMsg = Nothing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.errorMsg of
        Nothing ->
            Sub.none

        Just _ ->
            Time.every 2000 ClearError



-- URL


buildSearchUrl : String -> String
buildSearchUrl displayName =
    Url.Builder.crossOrigin
        "https://www.googleapis.com"
        [ "youtube", "v3", "search" ]
        [ Url.Builder.string "part" "snippet"
        , Url.Builder.string "q" displayName
        , Url.Builder.string "type" "channel"
        , Url.Builder.string "key" apiKey
        ]


buildChannelUrl : Channel.Id -> String
buildChannelUrl (Channel.Id id) =
    Url.Builder.crossOrigin
        "https://www.googleapis.com"
        [ "youtube", "v3", "channels" ]
        [ Url.Builder.string "part" "contentDetails,snippet"
        , Url.Builder.string "id" id
        , Url.Builder.string "key" apiKey
        ]


buildPlaylistUrl : String -> String
buildPlaylistUrl playlistId =
    Url.Builder.crossOrigin
        "https://www.googleapis.com"
        [ "youtube", "v3", "playlistItems" ]
        [ Url.Builder.string "part" "snippet"
        , Url.Builder.string "playlistId" playlistId
        , Url.Builder.string "key" apiKey
        , Url.Builder.int "maxResults" 3
        ]



-- HTTP


getChannelVideos : Channel -> Cmd Msg
getChannelVideos channel =
    Http.get
        { url = buildPlaylistUrl channel.uploadPlaylistId
        , expect = Http.expectJson LoadedChannelVideos (videoListDecoder channel)
        }


getChannelInfo : Channel.Id -> Cmd Msg
getChannelInfo id =
    Http.get
        { url = buildChannelUrl id
        , expect = Http.expectJson LoadedChannelInfo (channelDecoder id)
        }


getChannelId : String -> Cmd Msg
getChannelId displayName =
    Http.get
        { url = buildSearchUrl displayName
        , expect = Http.expectJson LoadedChannelId searchDecoder
        }


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl msg ->
            msg

        Http.Timeout ->
            "request timed out"

        Http.NetworkError ->
            "network error"

        Http.BadStatus status ->
            "status code " ++ String.fromInt status

        Http.BadBody msg ->
            msg


publishDecoder : D.Decoder Time.Posix
publishDecoder =
    D.at [ "snippet", "publishedAt" ] Iso8601.decoder


thumbnailDecoder : D.Decoder String
thumbnailDecoder =
    D.at [ "snippet", "thumbnails", "standard", "url" ] D.string


idDecoder : D.Decoder String
idDecoder =
    D.at [ "snippet", "resourceId", "videoId" ] D.string


titleDecoder : D.Decoder String
titleDecoder =
    D.at [ "snippet", "title" ] D.string


descriptionDecoder : D.Decoder String
descriptionDecoder =
    D.at [ "snippet", "description" ] D.string


videoDecoder : Channel -> D.Decoder Video
videoDecoder channel =
    D.map5 (Video channel)
        idDecoder
        publishDecoder
        thumbnailDecoder
        titleDecoder
        descriptionDecoder


videoListDecoder : Channel -> D.Decoder (List Video)
videoListDecoder channel =
    D.field "items" (D.list (videoDecoder channel))


channelTitleDecoder : D.Decoder String
channelTitleDecoder =
    D.field "items" <| D.index 0 <| D.at [ "snippet", "title" ] D.string


uploadPlaylistIdDecoder : D.Decoder String
uploadPlaylistIdDecoder =
    D.field "items" <| D.index 0 <| D.at [ "contentDetails", "relatedPlaylists", "uploads" ] D.string


channelDecoder : Channel.Id -> D.Decoder Channel
channelDecoder id =
    D.map2 (Channel id)
        channelTitleDecoder
        uploadPlaylistIdDecoder


searchDecoder : D.Decoder Channel.Id
searchDecoder =
    D.field "items" <| D.map Channel.Id <| D.index 0 <| D.at [ "id", "channelId" ] D.string



-- PORTS


port setStorage : E.Value -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg prevModel =
    let
        ( newModel, cmds ) =
            update msg prevModel
    in
    ( newModel
    , Cmd.batch [ setStorage (encodeModel newModel), cmds ]
    )



-- JSON ENCODE/DECODE


encodeChannel : Channel -> E.Value
encodeChannel channel =
    let
        (Channel.Id id) =
            channel.id
    in
    E.object
        [ ( "id", E.string id )
        , ( "title", E.string channel.title )
        , ( "uploadPlaylistId", E.string channel.uploadPlaylistId )
        ]


encodeVideo : Video -> E.Value
encodeVideo video =
    E.string video.id


encodeModel : Model -> E.Value
encodeModel model =
    E.object
        [ ( "channels", E.list encodeChannel model.channels )
        , ( "seen", E.list encodeVideo (List.filter (isVideoSeen model.seenVideos) model.videos) )
        ]


channelStorageDecoder : D.Decoder Channel
channelStorageDecoder =
    D.map3 Channel
        (D.map Channel.Id <| D.field "id" D.string)
        (D.field "title" D.string)
        (D.field "uploadPlaylistId" D.string)


modelDecoder : D.Decoder Model
modelDecoder =
    D.map2 (Model [] Nothing "" Nothing)
        (D.field "channels" (D.list channelStorageDecoder))
        (D.field "seen" (D.list D.string))



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Slow YouTube - Elm"
        [ div
            []
            [ aside
                [ class "sidebar" ]
                [ h1 [] [ Html.text "Channels" ]
                , div
                    [ id "channel-list" ]
                    (model.channels
                        |> List.sortBy (.title >> String.toLower)
                        |> List.map viewChannel
                    )
                , form [ onSubmit AddChannel ]
                    [ input [ type_ "text", name "url", placeholder "Channel URL", onInput UpdateNewChannel, value model.newChannelUrl ] []
                    , input [ type_ "submit", value "Add Channel" ] []
                    ]
                ]
            , main_
                [ class
                    (case model.currentVideo of
                        Just _ ->
                            "content blurred"

                        Nothing ->
                            "content"
                    )
                ]
                [ h1 [] [ Html.text "Videos" ]
                , viewErrorMsg model.errorMsg
                , div [ id "video-list" ]
                    (List.map
                        (viewChannelColumn model.videos model.seenVideos)
                        (List.sortBy (.title >> String.toLower) model.channels)
                    )
                ]
            ]
        , viewVideoPlayer model.currentVideo
        ]


videoSrc : Video -> String
videoSrc video =
    -- https://www.youtube-nocookie.com/embed/ZPM_5xedVus
    "https://www.youtube-nocookie.com/embed/" ++ video.id


viewErrorMsg : Maybe String -> Html Msg
viewErrorMsg maybeMsg =
    case maybeMsg of
        Nothing ->
            Html.text ""

        Just msg ->
            Html.p [ class "error-msg" ] [ Html.text msg ]


viewVideoPlayer : Maybe Video -> Html Msg
viewVideoPlayer maybeVideo =
    case maybeVideo of
        Nothing ->
            Html.text ""

        Just video ->
            div [ id "video-player" ]
                [ div [ class "aspect-ratio" ]
                    [ iframe
                        [ src (videoSrc video)
                        , attribute "frameborder" "0"
                        , attribute "allow" "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"
                        , attribute "allowfullscreen" "true"
                        ]
                        []
                    ]
                , button [ onClick (FinishVideo video) ] [ Html.text "Finished" ]
                , button [ onClick ExitVideo ] [ Html.text "Exit" ]
                ]


toEnglishMonth : Time.Month -> String
toEnglishMonth month =
    case month of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"


toDateString : Time.Posix -> String
toDateString date =
    toEnglishMonth (Time.toMonth Time.utc date) ++ " " ++ String.fromInt (Time.toDay Time.utc date) ++ ", " ++ String.fromInt (Time.toYear Time.utc date)


viewVideoThumbnail : Video -> Bool -> Html Msg
viewVideoThumbnail video seen =
    div
        [ id ("video-" ++ video.id)
        , class
            (if seen then
                "video-thumbnail seen"

             else
                "video-thumbnail"
            )
        , onClick (WatchVideo video)
        ]
        [ img [ src video.thumbnailUrl ] []
        , h3 [] [ Html.text video.title ]
        , span [] [ Html.text (toDateString video.publishedAt) ]
        ]


viewChannel : Channel -> Html Msg
viewChannel channel =
    -- renders a channel
    div [ class "channel-name" ]
        [ span [] [ Html.text channel.title ]
        , button [ onClick (RemoveChannel channel) ] [ Html.text "Remove" ]
        ]


channelVideos : List Video -> Channel -> List Video
channelVideos allVideos channel =
    allVideos
        |> List.filter (\v -> v.channel == channel)
        |> List.sortBy (.publishedAt >> Time.posixToMillis)
        |> List.reverse


isVideoSeen : List String -> Video -> Bool
isVideoSeen seenVideoIds video =
    List.member video.id seenVideoIds


viewChannelColumn : List Video -> List String -> Channel -> Html Msg
viewChannelColumn allVideos seenVideoIds channel =
    let
        videos =
            channelVideos allVideos channel
    in
    div [ class ("channel-" ++ channel.title) ]
        (h2 [] [ Html.text channel.title ]
            :: List.map2 viewVideoThumbnail
                videos
                (List.map (isVideoSeen seenVideoIds) videos)
        )



--
-- DOM


scrollToTop : Cmd Msg
scrollToTop =
    Task.attempt Scroll (Browser.Dom.setViewport 0 0)
