module DataUrl.MediaType.Parser exposing
    ( core
    , mediaType
    , parameter
    , parameters
    , restrictedName
    )

import Char
import DataUrl.Helper exposing (oneOrMore, zeroOrMore)
import DataUrl.MediaType.Internal as MediaType exposing (MediaType(..))
import Parser exposing (..)


mediaType : Parser MediaType
mediaType =
    succeed MediaType.init
        |= backtrackable core
        |= parameters


core : Parser ( String, String )
core =
    succeed Tuple.pair
        |= backtrackable coreType
        |. backtrackable (symbol "/")
        |= coreSubtype


coreType : Parser String
coreType =
    restrictedName


coreSubtype : Parser String
coreSubtype =
    restrictedName


restrictedName : Parser String
restrictedName =
    getChompedString <|
        succeed ()
            |. backtrackable (chompIf isRestrictedNameFirst)
            |. chompWhile isRestrictedNameChars


isRestrictedNameFirst : Char -> Bool
isRestrictedNameFirst c =
    List.any (\f -> f c)
        [ Char.isUpper
        , Char.isLower
        , Char.isDigit
        ]


isRestrictedNameChars : Char -> Bool
isRestrictedNameChars c =
    List.any (\f -> f c)
        [ Char.isUpper
        , Char.isLower
        , Char.isDigit
        , \x -> List.member x
            [ '!', '#', '$', '&', '-', '^', '_', '.', '+' ]
        ]


parameters : Parser (List ( String, String ))
parameters =
    oneOf
        [ succeed (::)
            |. backtrackable (symbol ";")
            |= parameter
            |= lazy (\_ -> parameters)
        , succeed []
        ]


parameter : Parser ( String, String )
parameter =
    succeed Tuple.pair
        |= backtrackable parameterAttribute
        |. backtrackable (symbol "=")
        |= parameterValue


parameterAttribute : Parser String
parameterAttribute =
    token


parameterValue : Parser String
parameterValue =
    oneOf
        [ quotedString
        , token
        ]


quotedString : Parser String
quotedString =
    succeed (\str -> "\"" ++ str ++ "\"")
        |. backtrackable (symbol "\"")
        |= backtrackable quotedStringBody
        |. symbol "\""


quotedStringBody : Parser String
quotedStringBody =
    oneOf
        [ zeroOrMore <|
            oneOf
                [ qtext
                , linearWhiteSpace
                , quotedPair
                ]
        , succeed ""
        ]


qtext : Parser String
qtext =
    getChompedString <|
        succeed ()
            |. chompIf
                (\c ->
                    List.member (Char.toCode c) <|
                        List.range 0 12
                            ++ List.range 14 33
                            ++ List.range 35 91
                            ++ List.range 93 127
                )


linearWhiteSpace : Parser String
linearWhiteSpace =
    oneOrMore <|
        succeed (++)
            |= backtrackable
                (oneOf
                    [ map (\_ -> "\u{000D}\n") <| Parser.token "\u{000D}\n"
                    , succeed ""
                    ]
                )
            |= lwspChar


lwspChar : Parser String
lwspChar =
    getChompedString <|
        succeed ()
            |. chompIf (Char.toCode >> (\n -> n == 32 || n == 9))


quotedPair : Parser String
quotedPair =
    getChompedString <|
        succeed ()
            |. backtrackable (chompIf (\c -> c == '\\'))
            |. chompIf isChar


isChar : Char -> Bool
isChar =
    Char.toCode >> \n -> 0 <= n && n <= 127


token : Parser String
token =
    getChompedString <|
        succeed ()
            |. backtrackable (chompIf isTokenChar)
            |. chompWhile isTokenChar


isTokenChar : Char -> Bool
isTokenChar c =
    List.any (\f -> f c)
        [ Char.isUpper
        , Char.isLower
        , Char.isDigit
        , \_ ->
            List.member c
                [ '!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '^', '_', '`', '{', '|', '}', '~' ]
        ]
