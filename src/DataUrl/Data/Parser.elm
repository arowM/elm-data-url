module DataUrl.Data.Parser
    exposing
        ( data_
        , reserved
        , unreserved
        , escaped
        )

import Char
import Parser exposing (..)


data_ : Parser String
data_ =
    map String.concat <|
        repeat zeroOrMore uric


uric : Parser String
uric =
    oneOf
        [ reserved
        , unreserved
        , escaped
        ]


reserved : Parser String
reserved =
    keep (Exactly 1) <|
        \c ->
            List.member c
                [ ';', '/', '?', ':', '@', '&', '=', '+', '$', ',' ]


unreserved : Parser String
unreserved =
    keep (Exactly 1) isUnreservedChar


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
    delayedCommitMap
        (\_ hex -> "%" ++ hex)
        (symbol "%")
    <|
        delayedCommitMap (++)
            (keep (Exactly 1) Char.isHexDigit)
            (keep (Exactly 1) Char.isHexDigit)
