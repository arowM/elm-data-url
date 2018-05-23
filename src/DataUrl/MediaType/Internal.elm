module DataUrl.MediaType.Internal
    exposing
        ( MediaType(..)
        , init
        , sampleMediaType
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


sampleMediaType : MediaType
sampleMediaType =
    init
        ( "text", "plain" )
        [ ( "charset", "iso-8859-7" ) ]
