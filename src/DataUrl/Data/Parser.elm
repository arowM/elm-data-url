module DataUrl.Data.Parser exposing
    ( data_
    , escaped
    , reserved
    , unreserved
    )

import Char
import DataUrl.Helper exposing (zeroOrMore)
import Parser exposing (..)


data_ : Parser String
data_ =
    zeroOrMore uric


uric : Parser String
uric =
    oneOf
        [ reserved
        , unreserved
        , escaped
        ]


reserved : Parser String
reserved =
    getChompedString <|
        succeed ()
            |. chompIf (\c -> List.member c [ ';', '/', '?', ':', '@', '&', '=', '+', '$', ',' ])


unreserved : Parser String
unreserved =
    getChompedString <|
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


escaped : Parser String
escaped =
    getChompedString <|
        succeed ()
            |. backtrackable (chompIf (\c -> c == '%'))
            |. backtrackable (chompIf Char.isHexDigit)
            |. chompIf Char.isHexDigit
