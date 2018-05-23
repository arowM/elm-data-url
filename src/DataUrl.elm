module DataUrl
    exposing
        ( DataUrl
        , Data
        , MediaType
        , fromString
        , toString
        , mediaType
        , data
        , parser
        )

{-| A module to handle [data URLs](https://developer.mozilla.org/docs/Web/HTTP/Basics_of_HTTP/Data_URIs) ([IETF RFC 2397](https://tools.ietf.org/html/rfc2397)) in type safe manner.


# Types

@docs DataUrl
@docs Data
@docs MediaType


# Constructors

@docs fromString


# Getters

@docs mediaType
@docs data


# Convert functions

@docs toString


# Lower level functions

@docs parser

-}

import DataUrl.Data as Data
import DataUrl.Internal exposing (DataUrl(..))
import DataUrl.MediaType as MediaType exposing (MediaType)
import DataUrl.Parser
import Parser exposing (Parser)


-- Types


{-| An opaque type representing data url
-}
type alias DataUrl =
    DataUrl.Internal.DataUrl


{-| Reexport `Data` type from `DataUrl.Data` for convenience.
-}
type alias Data =
    Data.Data


{-| Reexport `MediaType` type from `DataUrl.MediaType` for convenience.
-}
type alias MediaType =
    MediaType.MediaType



-- Constructors


{-| The only way to construct `DataUrl` value.
It takes a `String` representation of data URL and parses it as a `DataUrl` value.
If the provided `String` argument is illigal as a data URL, it returns `Nothing`.
Note that it does not accept raw HTML string, though [data URLs page of MDN](https://developer.mozilla.org/docs/data_URIs) has such an example.

    fromString "data:text/html,<script>alert('hi');</script>"
    --> Nothing

-}
fromString : String -> Maybe DataUrl
fromString =
    Result.toMaybe << Parser.run DataUrl.Parser.dataUrl



-- Getters


{-| Take `MediaType` value from `DataUrl` value.
As [data URLs page of MDN](https://developer.mozilla.org/docs/Web/HTTP/Basics_of_HTTP/Data_URIs) says,
it should be assumed that `text/plain;charset=US-ASCII` is set when the value is `Nothing`.
-}
mediaType : DataUrl -> Maybe MediaType
mediaType (DataUrl { mediaType }) =
    mediaType


{-| Take data part from `DataUrl`.
It is guaranteed that the data URL has `base64` token when the value is `Base64 someString`, but the `someString` could be illegal base64 string as follows.

    import DataUrl.Data exposing(Data(..))

    Maybe.map data <| fromString "data:text/plain;base64,This-is?illegal%base64_string"
    --> Just <| Base64 "This-is?illegal%base64_string"

The `someString` is only guaranteed to meet `uric` of [RFC 2396](https://tools.ietf.org/html/rfc2396).

-}
data : DataUrl -> Data
data (DataUrl { data }) =
    data



-- Convert functions


{-| Convert `DataUrl` values to `String`.

    Maybe.map DataUrl.toString <|
        fromString "data:text/plain;base64,SGVsbG8sIFdvcmxkIQ%3D%3D"
    --> Just "data:text/plain;base64,SGVsbG8sIFdvcmxkIQ%3D%3D"

-}
toString : DataUrl -> String
toString (DataUrl o) =
    String.concat <|
        List.filterMap identity <|
            [ Just "data:"
            , Maybe.map MediaType.toString o.mediaType
            , if Data.isBase64 o.data then
                Just ";base64"
              else
                Nothing
            , Just ","
            , Just <| Data.toString o.data
            ]



-- Lower level functions


{-| A parser for `DataUrl`.
Use `fromString` for simple usage.
-}
parser : Parser DataUrl
parser =
    DataUrl.Parser.dataUrl
