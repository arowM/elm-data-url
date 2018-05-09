module DataUrl.MediaType.Parser
    exposing
        ( mediaType
        , core
        , parameters
        , parameter
        , restrictedName
        )

import Char
import DataUrl.MediaType.Internal as MediaType
import DataUrl.MediaType.Internal exposing (MediaType(..))
import Parser exposing (..)
import Regex


mediaType : Parser MediaType
mediaType =
    delayedCommitMap MediaType.init
        core
        parameters


core : Parser ( String, String )
core =
    delayedCommitMap (,)
        coreType
        (delayedCommit (symbol "/") coreSubtype)


coreType : Parser String
coreType =
    restrictedName


coreSubtype : Parser String
coreSubtype =
    restrictedName


restrictedName : Parser String
restrictedName =
    delayedCommitMap (++)
        (keep (Exactly 1) isRestrictedNameFirst)
        (keep zeroOrMore isRestrictedNameChars
            |> andThen
                (\str ->
                    if String.length str > 126 then
                        fail "Too long restricted name of MIME type."
                    else
                        succeed str
                )
        )


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
        , flip List.member
            [ '!', '#', '$', '&', '-', '^', '_', '.', '+' ]
        ]


parameters : Parser (List ( String, String ))
parameters =
    repeat zeroOrMore
        (delayedCommit (symbol ";") parameter)


parameter : Parser ( String, String )
parameter =
    delayedCommitMap (,)
        parameterAttribute
        (delayedCommit (symbol "=") parameterValue)


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
    delayedCommit (succeed ()) <|
        succeed (\strs -> "\"" ++ String.concat strs ++ "\"")
            |. symbol "\""
            |= repeat zeroOrMore
                (oneOf
                    [ qtext
                    , linearWhiteSpace
                    , quotedPair
                    ]
                )
            |. symbol "\""


qtext : Parser String
qtext =
    keep (Exactly 1)
        (Regex.contains
            (Regex.regex "[\\0-\\14\\16-\\41\\43-\\133\\135-\\177]")
            << String.fromChar
        )


linearWhiteSpace : Parser String
linearWhiteSpace =
    map String.concat <|
        repeat oneOrMore
            (delayedCommitMap (++)
                (oneOf
                    [ keyword_ "\x0D\n"
                    , succeed ""
                    ]
                )
                lwspChar
            )


lwspChar : Parser String
lwspChar =
    keep (Exactly 1)
        (Regex.contains
            (Regex.regex "(\\40|\\11)")
            << String.fromChar
        )


quotedPair : Parser String
quotedPair =
    delayedCommitMap (++)
        (symbol_ "\\")
        (keep (Exactly 1) isChar)


isChar : Char -> Bool
isChar =
    Regex.contains
        (Regex.regex "[\\0-\\177]")
        << String.fromChar


token : Parser String
token =
    keep oneOrMore isTokenChar


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


keyword_ : String -> Parser String
keyword_ str =
    map (\_ -> str) <| keyword str


symbol_ : String -> Parser String
symbol_ str =
    map (\_ -> str) <| symbol str
