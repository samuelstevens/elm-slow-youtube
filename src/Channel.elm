module Channel exposing (..)

import Url exposing (fromString)
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string)


type Url
    = DisplayName String
    | ChannelId String


type Id
    = Id String


type alias Channel =
    { id : Id
    , title : String
    , uploadPlaylistId : String
    }


channelParser : Parser (Url -> a) a
channelParser =
    oneOf
        [ map ChannelId (s "channel" </> string)
        , map DisplayName (s "c" </> string) -- https://www.youtube.com/c/dogdogtwitch
        ]


parseChannelUrl : String -> Maybe Url
parseChannelUrl string =
    fromString string
        |> Maybe.andThen (Url.Parser.parse channelParser)
