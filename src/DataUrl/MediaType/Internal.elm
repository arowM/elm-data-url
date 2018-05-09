module DataUrl.MediaType.Internal
    exposing
        ( MediaType(..)
        , init
        )


type MediaType
    = MediaType
        { type_ : ( String, String )
        , parameters : List ( String, String )
        }


init : ( String, String ) -> List ( String, String ) -> MediaType
init type_ parameters =
    MediaType
        { type_ = type_
        , parameters = parameters
        }
