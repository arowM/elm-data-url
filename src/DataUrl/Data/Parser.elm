module DataUrl.Data.Parser exposing
    ( data_
    , escaped
    , fuzzy
    , reserved
    , unreserved
    )

import Char
import Parser exposing (..)


fuzzy : Parser String
fuzzy =
    getChompedString <| chompWhile (always True)


data_ : Parser String
data_ =
    getChompedString <| loop () dataP


dataP : () -> Parser (Step () ())
dataP _ =
    oneOf
        -- [ map Loop <| backtrackable uric
        [ map Loop uric
        , succeed (Done ())
        ]


uric : Parser ()
uric =
    oneOf
        [ reserved
        , unreserved
        , escaped
        ]


reserved : Parser ()
reserved =
    succeed ()
        |. chompIf (\c -> List.member c [ ';', '/', '?', ':', '@', '&', '=', '+', '$', ',' ])


unreserved : Parser ()
unreserved =
    succeed ()
        |. chompIf isUnreservedChar


isUnreservedChar : Char -> Bool
isUnreservedChar c =
    List.any (\f -> f c)
        [ Char.isUpper
        , Char.isLower
        , Char.isDigit
        , \_ ->
            List.member c
                [ '-', '_', '.', '!', '~', '*', '\'', '(', ')' ]
        ]


escaped : Parser ()
escaped =
    succeed ()
        |. backtrackable (chompIf (\c -> c == '%'))
        |. backtrackable (chompIf Char.isHexDigit)
        |. chompIf Char.isHexDigit
