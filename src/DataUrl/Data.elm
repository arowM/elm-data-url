module DataUrl.Data
    exposing
        ( Data(..)
        , toString
        , isBase64
        )

{-| A module for `data` part of [data URLs](https://developer.mozilla.org/docs/Web/HTTP/Basics_of_HTTP/Data_URIs).


# Types

@docs Data


# Convert functions

@docs toString


# Helper functions

@docs isBase64

-}


{-| A type representing the `data` part of data urls.
-}
type Data
    = Textual String
    | Base64 String


{-| Convert `Data` value to `String`.
DataUrl.Data.toString <| Textual "foobarbaz"
--> "foobarbaz"

    DataUrl.Data.toString <| Base64 "foobarbaz"
    --> "foobarbaz"

-}
toString : Data -> String
toString data =
    case data of
        Textual str ->
            str

        Base64 str ->
            str



-- Helper functions


{-|

    isBase64 (Base64 "any string")
    --> True

    isBase64 (Textual "any string")
    --> False
-}
isBase64 : Data -> Bool
isBase64 data =
    case data of
        Base64 _ ->
            True

        _ ->
            False
