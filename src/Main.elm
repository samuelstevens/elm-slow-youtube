port module Main exposing (..)

import Browser
import Browser.Dom
import Dict exposing (Dict)
import Html exposing (Html, aside, button, div, form, h1, h2, h3, iframe, img, input, main_, span)
import Html.Attributes exposing (attribute, class, id, name, placeholder, src, type_, value)
import Html.Events
import Http
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)
import Task
import Time
import Url
import Url.Builder
import Url.Parser exposing ((</>))
import YouTube



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


type Problem
    = StorageError String
    | UrlParseError String
    | HttpError String
    | AlreadySubscribedError String
    | ApiKeyError
    | UnknownError


type alias App =
    { apiKey : String
    , channels : List YouTube.Channel
    , activity : YouTube.Activity

    -- UI
    , channelUrl : String
    }


type alias StoredModel =
    { apiKey : String
    , channels : List YouTube.Channel -- List not for order, but because Channel.Id is not comparable
    , seen : List YouTube.VideoDefinition
    }


storedModelFromApp : App -> StoredModel
storedModelFromApp { channels, apiKey } =
    let
        seenVideos =
            List.map YouTube.makeDefinition (flatten (List.map (\chan -> List.filter .seen chan.videos) channels))
    in
    { channels = unique channels, apiKey = apiKey, seen = seenVideos }


type Model
    = Watching App YouTube.Video
    | Overview App (Maybe Problem)
    | Irrecoverable Problem


decodeLocalStorage : E.Value -> Model
decodeLocalStorage args =
    case D.decodeValue storedModelDecoder args of
        Ok model ->
            case model.apiKey of
                "" ->
                    Irrecoverable ApiKeyError

                _ ->
                    Overview { channels = model.channels, apiKey = model.apiKey, activity = YouTube.activityFromVideoDefinitions model.seen, channelUrl = "" } Nothing

        Err err ->
            Irrecoverable (StorageError (D.errorToString err))



-- ( { channels = [], apiKey = "", activity = YouTube.newActivity, channelUrl = "" }, Just (StorageError (D.errorToString err)) )


init : E.Value -> ( Model, Cmd Msg )
init args =
    case decodeLocalStorage args of
        Overview app (Just (StorageError msg)) ->
            ( Overview app (Just (StorageError msg)), saveApp app )

        Overview app maybeProblem ->
            ( Overview app maybeProblem
            , Cmd.batch
                (saveApp app
                    :: List.map
                        (getChannelVideos app.apiKey)
                        app.channels
                )
            )

        Irrecoverable problem ->
            ( Irrecoverable problem, Cmd.none )

        _ ->
            ( Irrecoverable UnknownError, Cmd.none )



-- UPDATE


type Msg
    = UpdateChannelUrl String
    | AddChannel
    | RemoveChannel YouTube.Channel
    | WatchVideo YouTube.Video
    | ExitVideo
    | FinishVideo
    | LoadedChannelInfo (Result Http.Error YouTube.Channel)
    | LoadedChannelVideos (Result Http.Error YouTube.Channel)
    | LoadedChannelId (Result Http.Error YouTube.Id)
    | Scroll (Result Browser.Dom.Error ())
    | ClearError Time.Posix



-- https://m.youtube.com/channel/UCZHmQk67mSJgfCCTn7xBfew


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UpdateChannelUrl url, Overview app problem ) ->
            ( Overview { app | channelUrl = url } problem, Cmd.none )

        ( AddChannel, Overview app problem ) ->
            case YouTube.parseChannelUrl app.channelUrl of
                Nothing ->
                    ( Overview app (Just (UrlParseError (app.channelUrl ++ " isn't a valid YouTube channel URL."))), Cmd.none )

                -- need to send a request to get the channel id
                Just (YouTube.DisplayName name) ->
                    ( model, getChannelId app.apiKey name )

                Just urlId ->
                    case YouTube.idFromUrl urlId of
                        Ok id ->
                            ( model, getChannelInfo app.apiKey id )

                        Err err ->
                            ( Overview app (Just (UrlParseError err)), Cmd.none )

        ( RemoveChannel channel, Overview app problem ) ->
            ( Overview { app | channels = List.filter (\c -> c /= channel) app.channels } problem, Cmd.none )

        ( WatchVideo video, Overview app problem ) ->
            ( Watching app video, scrollToTop )

        ( ExitVideo, Watching app video ) ->
            ( Overview app Nothing, Cmd.none )

        ( FinishVideo, Watching app video ) ->
            ( Overview { app | channels = List.map (YouTube.markVideoAsSeen video.id) app.channels } Nothing, Cmd.none )

        ( LoadedChannelId result, Watching app video ) ->
            case result of
                Ok id ->
                    ( model, getChannelInfo app.apiKey id )

                Err err ->
                    ( model, Cmd.none )

        ( LoadedChannelId result, Overview app problem ) ->
            case result of
                Ok id ->
                    ( model, getChannelInfo app.apiKey id )

                Err err ->
                    ( Overview app (Just (HttpError (httpErrorToString err))), Cmd.none )

        ( LoadedChannelVideos result, Watching app video ) ->
            case result of
                Ok channel ->
                    ( Watching { app | channels = List.map (updateChannel channel) app.channels } video, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        ( LoadedChannelVideos result, Overview app problem ) ->
            case result of
                Ok channel ->
                    ( Overview { app | channels = List.map (updateChannel (YouTube.updateVideosWithActivity app.activity channel)) app.channels } problem, Cmd.none )

                Err err ->
                    ( Overview app (Just (HttpError (httpErrorToString err))), Cmd.none )

        ( LoadedChannelInfo result, Watching app video ) ->
            case result of
                Ok channel ->
                    if List.member channel app.channels then
                        ( model, Cmd.none )

                    else
                        ( Watching { app | channels = channel :: app.channels } video, getChannelVideos app.apiKey channel )

                Err err ->
                    ( model, Cmd.none )

        ( LoadedChannelInfo result, Overview app problem ) ->
            case result of
                Ok channel ->
                    if List.member channel app.channels then
                        ( Overview app (Just (AlreadySubscribedError ("Already subscribed to " ++ channel.title ++ "."))), Cmd.none )

                    else
                        ( Overview { app | channels = channel :: app.channels } problem, getChannelVideos app.apiKey channel )

                Err err ->
                    ( Overview app (Just (HttpError (httpErrorToString err))), Cmd.none )

        ( Scroll _, _ ) ->
            ( model, Cmd.none )

        ( ClearError _, Overview app problem ) ->
            ( Overview app Nothing, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


updateChannel : YouTube.Channel -> YouTube.Channel -> YouTube.Channel
updateChannel new old =
    if old.id == new.id then
        new

    else
        old



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Overview app (Just _) ->
            Time.every 5000 ClearError

        _ ->
            Sub.none



-- HTTP


{-| Given a channel, get all the videos for the channel
-}
getChannelVideos : String -> YouTube.Channel -> Cmd Msg
getChannelVideos apiKey channel =
    Http.get
        { url = YouTube.buildPlaylistUrl apiKey channel.uploadPlaylistId
        , expect =
            Http.expectJson LoadedChannelVideos (YouTube.uploadedVideosApiDecoder channel)
        }


{-| Given a channel id, get the playlist
-}
getChannelInfo : String -> YouTube.Id -> Cmd Msg
getChannelInfo apiKey id =
    Http.get
        { url = YouTube.buildChannelUrl apiKey id
        , expect = Http.expectJson LoadedChannelInfo YouTube.channelApiDecoder
        }


getChannelId : String -> String -> Cmd Msg
getChannelId apiKey displayName =
    Http.get
        { url = YouTube.buildSearchUrl apiKey displayName
        , expect = Http.expectJson LoadedChannelId YouTube.channelIdApiDecoder
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



-- PORTS


port setStorage : E.Value -> Cmd msg


saveApp : App -> Cmd Msg
saveApp app =
    setStorage (encodeModel (storedModelFromApp app))


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg prev =
    let
        ( new, cmd ) =
            update msg prev
    in
    case new of
        Overview app problem ->
            ( new, Cmd.batch [ saveApp app, cmd ] )

        _ ->
            ( new, cmd )



-- JSON ENCODE/DECODE


encodeModel : StoredModel -> E.Value
encodeModel model =
    E.object
        [ ( "channels", E.list YouTube.encodeChannel model.channels )
        , ( "seen", E.list YouTube.encodeVideoDefinition model.seen )
        , ( "apiKey", E.string model.apiKey )
        ]


storedModelDecoder : D.Decoder StoredModel
storedModelDecoder =
    D.map3 StoredModel
        (D.field "apiKey" D.string)
        (D.oneOf [ D.field "channels" (D.list YouTube.channelStorageDecoder), D.succeed [] ])
        (D.oneOf [ D.field "seen" (D.list YouTube.videoDefinitionDecoder), D.succeed [] ])


view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Slow YouTube - Elm"
        [ div
            []
            [ main_
                [ class
                    (case model of
                        Watching _ _ ->
                            "content blurred"

                        _ ->
                            "content"
                    )
                ]
                [ viewErrorMsg
                    (case model of
                        Overview _ problem ->
                            problem

                        Irrecoverable problem ->
                            Just problem

                        _ ->
                            Nothing
                    )
                , div [ id "video-list" ]
                    (case model of
                        Watching { channels } _ ->
                            viewVideoList channels

                        Overview { channels } _ ->
                            viewVideoList channels

                        Irrecoverable err ->
                            []
                    )
                ]
            , viewVideoPlayer
                (case model of
                    Watching app video ->
                        Just video

                    _ ->
                        Nothing
                )
            ]
        , aside
            [ class "sidebar" ]
            [ h1 [] [ Html.text "Channels" ]
            , div
                [ id "channel-list" ]
                (case model of
                    Watching { channels } _ ->
                        viewChannelList channels

                    Overview { channels } _ ->
                        viewChannelList channels

                    Irrecoverable err ->
                        []
                )
            , form
                [ Html.Events.onSubmit AddChannel ]
                [ input [ type_ "text", name "url", placeholder "Channel URL", Html.Events.onInput UpdateChannelUrl ] []
                , input [ type_ "submit", value "Add Channel" ] []
                ]
            ]
        ]


viewChannelList : List YouTube.Channel -> List (Html Msg)
viewChannelList channels =
    channels
        |> List.sortBy (.title >> String.toLower)
        |> List.map viewChannel


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry fn ( a, b ) =
    fn a b


videoList : YouTube.Channel -> List ( YouTube.Channel, YouTube.Video )
videoList channel =
    List.map (\v -> ( channel, v )) channel.videos


viewVideoList : List YouTube.Channel -> List (Html Msg)
viewVideoList channels =
    List.map videoList channels
        |> flatten
        |> List.sortBy (\( c, v ) -> Time.posixToMillis v.publishedAt)
        |> List.reverse
        |> List.map (uncurry viewVideoThumbnail)


viewErrorMsg : Maybe Problem -> Html Msg
viewErrorMsg maybeProblem =
    case maybeProblem of
        Just (StorageError msg) ->
            Html.p [] [ Html.text ("Error in your local storage: " ++ msg) ]

        Just (UrlParseError msg) ->
            Html.p [] [ Html.text msg ]

        Just (HttpError msg) ->
            Html.p [] [ Html.text msg ]

        Just (AlreadySubscribedError msg) ->
            Html.p [] [ Html.text msg ]

        Just ApiKeyError ->
            Html.p [] [ Html.text "No API key. Please refresh the page." ]

        Just UnknownError ->
            Html.p [] [ Html.text "Unknown error. Please contact the developers." ]

        Nothing ->
            Html.text ""


viewVideoPlayer : Maybe YouTube.Video -> Html Msg
viewVideoPlayer maybeVideo =
    case maybeVideo of
        Nothing ->
            Html.text ""

        Just video ->
            div [ id "video-player" ]
                [ div [ class "aspect-ratio" ]
                    [ iframe
                        [ src (YouTube.src video)
                        , attribute "frameborder" "0"
                        , attribute "allow" "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"
                        , attribute "allowfullscreen" "true"
                        ]
                        []
                    ]
                , button [ Html.Events.onClick FinishVideo ] [ Html.text "Finished" ]
                , button [ Html.Events.onClick ExitVideo ] [ Html.text "Exit" ]
                ]


viewVideoThumbnail : YouTube.Channel -> YouTube.Video -> Html Msg
viewVideoThumbnail channel video =
    div
        [ class
            (if video.seen then
                "video-thumbnail seen"

             else
                "video-thumbnail"
            )
        , Html.Events.onClick (WatchVideo video)
        ]
        [ img [ src video.thumbnail ] []
        , div [ class "video-caption" ]
            [ span [ class "video-title" ] [ Html.text video.title ]
            , span [ class "video-channel" ] [ Html.text (channel.title ++ " (" ++ YouTube.toDateString video.publishedAt ++ ")") ]
            ]
        ]


viewChannel : YouTube.Channel -> Html Msg
viewChannel channel =
    -- renders a channel
    div [ class "channel-name" ]
        [ span [] [ Html.text channel.title ]
        , button [ Html.Events.onClick (RemoveChannel channel) ] [ Html.text "Remove" ]
        ]



--
-- DOM


scrollToTop : Cmd Msg
scrollToTop =
    Task.attempt Scroll (Browser.Dom.setViewport 0 0)



-- UTIL


flatten : List (List a) -> List a
flatten list =
    list |> List.foldr (++) []


{-| Removes all unique values from a list according to a equality function. Slow.
-}
unique : List a -> List a
unique list =
    case list of
        [] ->
            []

        x :: xs ->
            if List.member x xs then
                unique xs

            else
                x :: unique xs
