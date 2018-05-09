module DataUrl.Parser
    exposing
        ( dataUrl
        , isBase64
        )

import DataUrl.Data exposing (Data(..))
import DataUrl.Data.Parser as Data
import DataUrl.Internal exposing (DataUrl(..))
import DataUrl.MediaType.Parser as MediaType
import Parser exposing (..)


{-|

    Result.mapError .col <| run dataUrl "data:text/html"
    --> Err 15

    Result.mapError .col <| run dataUrl "data:text/html;base64"
    --> Err 22

    Result.mapError .col <| run dataUrl "data:text/html;base64xxx"
    --> Err 22
-}
dataUrl : Parser DataUrl
dataUrl =
    succeed
        (\mediaType isB64 data ->
            DataUrl
                { mediaType = mediaType
                , data =
                    (if isB64 then
                        Base64
                     else
                        Textual
                    )
                        data
                }
        )
        |. keyword "data:"
        |= maybe MediaType.mediaType
        |= isBase64
        |. symbol ","
        |= Data.data_
        |. end


maybe : Parser a -> Parser (Maybe a)
maybe delayedParser =
    oneOf
        [ map Just delayedParser
        , succeed Nothing
        ]


isBase64 : Parser Bool
isBase64 =
    oneOf
        [ map (\_ -> True) <| keyword ";base64"
        , succeed False
        ]
