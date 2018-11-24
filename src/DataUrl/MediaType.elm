module DataUrl.MediaType exposing
    ( MediaType
    , toString
    , type_
    , parameters
    )

{-| A module to handle media types of [data URLs](https://developer.mozilla.org/docs/Web/HTTP/Basics_of_HTTP/Data_URIs) ([IETF RFC 2397](https://tools.ietf.org/html/rfc2397)) in type safe manner.

This module only provides a opaque type and getters, so there are no way to construct new `MediaType`.
The only way to get `MediaType` is using `DataUrl.mediaType`, which picks `MediaType` value from `DataUrl` value.


# Types

@docs MediaType


# Convert functions

@docs toString


# Getters

@docs type_
@docs parameters

-}

import DataUrl.MediaType.Internal exposing (MediaType(..))


{-| An opaque type representing the media type part of data urls.
-}
type alias MediaType =
    DataUrl.MediaType.Internal.MediaType


{-| Take pair of type and subtype from `MediaType` value.
The type and subtype is guaranteed to meet `type-name` and `subtype-name` in [IETF RFC 6838](https://tools.ietf.org/html/rfc6838) respectively, which is not as strict as [IETF RFC 2397](https://tools.ietf.org/html/rfc2397) requires, when created by `DataUrl.fromString`.

    import DataUrl.MediaType.Internal exposing (sampleMediaType)

    type_ sampleMediaType
    --> ( "text", "plain" )

-}
type_ : MediaType -> ( String, String )
type_ (MediaType o) =
    o.type_


{-| Take list of key-value pair of parameters from `MediaType` value.
The parameter key and value is guaranteed to meet `attribute` and `value` in [IETF RFC 2045](https://tools.ietf.org/html/rfc2045) respectively, as [IETF RFC 2397](https://tools.ietf.org/html/rfc2397) requires, when created by `DataUrl.fromString`.

    import DataUrl.MediaType.Internal exposing (sampleMediaType)

    parameters sampleMediaType
    --> [ ( "charset", "iso-8859-7" ) ]

-}
parameters : MediaType -> List ( String, String )
parameters (MediaType o) =
    o.parameters


{-| Convert `MediaType` value to string representation.

    import DataUrl.MediaType.Internal exposing (sampleMediaType)

    DataUrl.MediaType.toString sampleMediaType
    --> "text/plain;charset=iso-8859-7"

-}
toString : MediaType -> String
toString (MediaType o) =
    String.concat
        (typeToString o.type_ :: List.map parameterToString o.parameters)


typeToString : ( String, String ) -> String
typeToString ( mainType, subtype ) =
    String.concat
        [ mainType
        , "/"
        , subtype
        ]


parameterToString : ( String, String ) -> String
parameterToString ( key, val ) =
    String.concat
        [ ";"
        , key
        , "="
        , val
        ]
