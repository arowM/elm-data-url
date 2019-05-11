module DataUrl.Parser exposing
    ( dataUrl
    , dataUrlFuzzy
    , isBase64
    , maybe
    )

import DataUrl.Data exposing (Data(..))
import DataUrl.Data.Parser as Data
import DataUrl.Internal exposing (DataUrl(..))
import DataUrl.MediaType.Parser as MediaType
import Parser exposing (..)


{-|

    Result.mapError (\_ -> ()) <| run dataUrl "data:text/html"
    --> Err ()

    Result.mapError (\_ -> ()) <| run dataUrl "data:text/html;base64"
    --> Err ()

    fesult.mapError (\_ -> ()) <| run dataUrl "data:text/html;base64xxx"
    --> Err ()

-}
dataUrl : Parser DataUrl
dataUrl =
    succeed
        (\mediaType isB64 data ->
            DataUrl
                { mediaType = mediaType
                , data =
                    if isB64 then
                        Base64 data

                    else
                        Textual data
                }
        )
        |. token "data:"
        |= maybe MediaType.mediaType
        |= isBase64
        |. symbol ","
        |= Data.data_
        |. end


{-|

    Result.mapError (\_ -> ()) <| run dataUrlFuzzy "data:text/html"
    --> Err ()

    Result.mapError (\_ -> ()) <| run dataUrlFuzzy "data:text/html;base64"
    --> Err ()

    fesult.mapError (\_ -> ()) <| run dataUrlFuzzy "data:text/html;base64xxx"
    --> Err ()

-}
dataUrlFuzzy : Parser DataUrl
dataUrlFuzzy =
    succeed
        (\mediaType isB64 data ->
            DataUrl
                { mediaType = mediaType
                , data =
                    if isB64 then
                        Base64 data

                    else
                        Textual data
                }
        )
        |. token "data:"
        |= maybe MediaType.mediaType
        |= isBase64
        |. symbol ","
        |= Data.fuzzy
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
        [ map (\_ -> True) <| token ";base64"
        , succeed False
        ]
