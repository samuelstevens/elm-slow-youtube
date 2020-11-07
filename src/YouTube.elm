module YouTube exposing (Activity, Channel, Id, Url(..), Video, VideoDefinition, activityFromVideoDefinitions, buildChannelUrl, buildPlaylistUrl, channelApiDecoder, channelIdApiDecoder, channelStorageDecoder, encodeChannel, encodeId, encodeVideoDefinition, idDecoder, idFromUrl, makeDefinition, markVideoAsSeen, newActivity, parseChannelUrl, src, toDateString, updateVideosWithActivity, uploadedVideosApiDecoder, videoApiDecoder, videoDefinitionDecoder, videoSeen)

import Dict exposing (Dict)
import Iso8601
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)
import Time
import Url
import Url.Builder
import Url.Parser exposing ((</>))


type Id
    = Id String


type alias Video =
    { id : Id
    , publishedAt : Time.Posix
    , thumbnail : String
    , title : String
    , description : String
    , seen : Bool
    }


type alias VideoDefinition =
    { id : Id, seen : Bool }


type alias Channel =
    { id : Id -- READONLY
    , title : String -- READONLY
    , uploadPlaylistId : Id -- READONLY
    , videos : List Video
    }


src : Video -> String
src video =
    let
        (Id i) =
            video.id
    in
    "https://www.youtube-nocookie.com/embed/" ++ i


makeDefinition : Video -> VideoDefinition
makeDefinition video =
    { id = video.id, seen = video.seen }



-- ACTIVITY


type Activity
    = Activity
        { seen : Dict String Bool
        }


newActivity : Activity
newActivity =
    Activity { seen = Dict.empty }


activityFromVideoDefinitions : List VideoDefinition -> Activity
activityFromVideoDefinitions defs =
    Activity
        { seen =
            Dict.fromList
                (List.map
                    (\{ seen, id } ->
                        case id of
                            Id i ->
                                ( i, seen )
                    )
                    defs
                )
        }


videoSeen : Activity -> Id -> Bool
videoSeen (Activity activity) (Id id) =
    case Dict.get id activity.seen of
        Just res ->
            res

        _ ->
            False


{-| -}
updateVideosWithActivity : Activity -> Channel -> Channel
updateVideosWithActivity (Activity { seen }) channel =
    let
        seenIds =
            List.map (\( id, s ) -> Id id) <| List.filter (\( id, s ) -> s) (Dict.toList seen)
    in
    markRestOfVideosAsSeen seenIds channel


markRestOfVideosAsSeen : List Id -> Channel -> Channel
markRestOfVideosAsSeen seenIds channel =
    case seenIds of
        [] ->
            channel

        video :: rest ->
            markRestOfVideosAsSeen rest (markVideoAsSeen video channel)


markVideoAsSeen : Id -> Channel -> Channel
markVideoAsSeen id channel =
    { channel
        | videos =
            List.map
                (\v ->
                    if v.id == id then
                        { v | seen = True }

                    else
                        v
                )
                channel.videos
    }


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



-- channelVideos : List Video -> Channel.Channel -> List Video
-- channelVideos videos channel =
--     videos
--         |> List.filter (\(Video v) -> v.channel == channel)
--         |> List.sortBy (\(Video v) -> v.publishedAt |> Time.posixToMillis)
--         |> List.reverse


encodeId : Id -> E.Value
encodeId (Id i) =
    E.string i


encodeChannel : Channel -> E.Value
encodeChannel channel =
    E.object [ ( "id", encodeId channel.id ), ( "title", E.string channel.title ), ( "uploadPlaylistId", encodeId channel.uploadPlaylistId ) ]


encodeVideoDefinition : VideoDefinition -> E.Value
encodeVideoDefinition def =
    E.object [ ( "id", encodeId def.id ), ( "seen", E.bool def.seen ) ]


publishDecoder : D.Decoder Time.Posix
publishDecoder =
    D.at [ "snippet", "publishedAt" ] Iso8601.decoder


thumbnailDecoder : D.Decoder String
thumbnailDecoder =
    D.at [ "snippet", "thumbnails", "standard", "url" ] D.string


videoIdDecoder : D.Decoder Id
videoIdDecoder =
    D.map Id <| D.at [ "snippet", "resourceId", "videoId" ] D.string


titleDecoder : D.Decoder String
titleDecoder =
    D.at [ "snippet", "title" ] D.string


descriptionDecoder : D.Decoder String
descriptionDecoder =
    D.at [ "snippet", "description" ] D.string


videoApiDecoder : Channel -> D.Decoder Video
videoApiDecoder channel =
    D.map6 Video
        videoIdDecoder
        publishDecoder
        thumbnailDecoder
        titleDecoder
        descriptionDecoder
        (videoIdDecoder |> D.andThen (\id -> D.succeed False))


uploadedVideosApiDecoder : Channel -> D.Decoder Channel
uploadedVideosApiDecoder channel =
    D.map (Channel channel.id channel.title channel.uploadPlaylistId)
        (D.field "items" <| D.list (videoApiDecoder channel))


videoDefinitionDecoder : D.Decoder VideoDefinition
videoDefinitionDecoder =
    D.map2 VideoDefinition (D.field "id" idDecoder) (D.field "seen" D.bool)



-- videoDecoder : Channel -> (String -> Bool) -> D.Decoder LoadedVideo
-- videoDecoder channel isSeen =
--


type Url
    = UrlId String
    | DisplayName String


idFromUrl : Url -> Result String Id
idFromUrl url =
    case url of
        UrlId i ->
            Ok (Id i)

        DisplayName name ->
            Err (name ++ " is not a YouTube ID")


idDecoder : D.Decoder Id
idDecoder =
    D.map Id <| D.string


{-| Decodes a channel from local storage (personal JSON format)

The Channel has no videos; it must be populated to be useful

-}
channelStorageDecoder : D.Decoder Channel
channelStorageDecoder =
    D.map4 Channel
        (D.field "id" idDecoder)
        (D.field "title" D.string)
        (D.field "uploadPlaylistId" idDecoder)
        (D.succeed [])


{-| Decodes the HTTP request for getting a channel id from the display name.

The Channel has no videos; it must be populated to be useful

-}
channelIdApiDecoder : D.Decoder Id
channelIdApiDecoder =
    D.field "items" <| D.map Id <| D.index 0 <| D.at [ "id", "channelId" ] D.string


uploadPlaylistIdDecoder : D.Decoder Id
uploadPlaylistIdDecoder =
    D.at [ "contentDetails", "relatedPlaylists", "uploads" ] (D.map Id D.string)


{-| Decodes the HTTP request for getting channel videos from a channel uploadPlaylistId
-}
channelApiDecoder : D.Decoder Channel
channelApiDecoder =
    D.map4 Channel
        (D.field "items" <| D.index 0 <| D.field "id" idDecoder)
        (D.field "items" <| D.index 0 <| titleDecoder)
        (D.field "items" <| D.index 0 <| uploadPlaylistIdDecoder)
        -- TODO
        (D.succeed [])


channelParser : Url.Parser.Parser (Url -> a) a
channelParser =
    Url.Parser.oneOf
        [ Url.Parser.map UrlId (Url.Parser.s "channel" </> Url.Parser.string)
        , Url.Parser.map DisplayName (Url.Parser.s "c" </> Url.Parser.string) -- https://www.youtube.com/c/dogdogtwitch
        ]


parseChannelUrl : String -> Maybe Url
parseChannelUrl string =
    Url.fromString string
        |> Maybe.andThen (Url.Parser.parse channelParser)



-- URL


buildChannelUrl : String -> Id -> String
buildChannelUrl apiKey (Id i) =
    Url.Builder.crossOrigin
        "https://www.googleapis.com"
        [ "youtube", "v3", "channels" ]
        [ Url.Builder.string "part" "contentDetails,snippet"
        , Url.Builder.string "id" i
        , Url.Builder.string "key" apiKey
        ]


buildPlaylistUrl : String -> Id -> String
buildPlaylistUrl apiKey (Id playlistId) =
    Url.Builder.crossOrigin
        "https://www.googleapis.com"
        [ "youtube", "v3", "playlistItems" ]
        [ Url.Builder.string "part" "snippet"
        , Url.Builder.string "playlistId" playlistId
        , Url.Builder.string "key" apiKey
        , Url.Builder.int "maxResults" 3
        ]
