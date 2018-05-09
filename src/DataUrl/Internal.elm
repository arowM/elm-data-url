module DataUrl.Internal
    exposing
        ( DataUrl(..)
        , init
        )

import DataUrl.Data exposing (Data)
import DataUrl.MediaType exposing (MediaType)


type DataUrl
    = DataUrl
        { mediaType : Maybe MediaType
        , data : Data
        }


init : Maybe MediaType -> Data -> DataUrl
init mediaType data =
    DataUrl
        { mediaType = mediaType
        , data = data
        }
