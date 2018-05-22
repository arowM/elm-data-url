module DataUrlSuite exposing (..)

import Char
import DataUrl
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)


suite : Test
suite =
    describe "DataUrl"
        [ describe "`toString` after `fromString`"
            [ fuzz dataUrl "is equals" <|
                \str ->
                    DataUrl.fromString str
                        |> Maybe.map DataUrl.toString
                        |> Expect.equal (Just str)
            ]
        ]


dataUrl : Fuzzer String
dataUrl =
    Fuzz.map3
        (\isBase64 mmediaType data ->
            String.concat
                [ "data:"
                , Maybe.withDefault "" mmediaType
                , if isBase64 then
                    ";base64"
                  else
                    ""
                , ","
                , data
                ]
        )
        Fuzz.bool
        (Fuzz.maybe mediaType)
        data


mediaType : Fuzzer String
mediaType =
    Fuzz.map3
        (\type_ subtype params ->
            String.concat <|
                [ type_
                , "/"
                , subtype
                ]
                    ++ List.map
                        (\( attr, val ) ->
                            String.concat
                                [ ";"
                                , attr
                                , "="
                                , val
                                ]
                        )
                        params
        )
        restrictedName
        restrictedName
        parameters


restrictedName : Fuzzer String
restrictedName =
    Fuzz.map2 (++)
        restrictedNameFirst
        restrictedNameTail


restrictedNameFirst : Fuzzer String
restrictedNameFirst =
    Fuzz.map String.fromChar <|
        Fuzz.oneOf
            [ upper
            , lower
            , digit
            ]


restrictedNameTail : Fuzzer String
restrictedNameTail =
    Fuzz.map (String.fromList << List.take 126) <|
        Fuzz.list <|
            Fuzz.oneOf
                [ upper
                , lower
                , digit
                , restrictedMark
                ]


restrictedMark : Fuzzer Char
restrictedMark =
    Fuzz.oneOf <| List.map Fuzz.constant [ '!', '#', '$', '&', '-', '^', '_', '.', '+' ]


parameters : Fuzzer (List ( String, String ))
parameters =
    Fuzz.list <|
        Fuzz.map2 (,) attribute value


attribute : Fuzzer String
attribute =
    token


value : Fuzzer String
value =
    Fuzz.oneOf
        [ quotedString
        , token
        ]


quotedString : Fuzzer String
quotedString =
    Fuzz.map (\strs -> "\"" ++ String.concat strs ++ "\"") <|
        Fuzz.list <|
            Fuzz.oneOf
                [ qtext
                , linearWhiteSpace
                , quotedPair
                ]


qtext : Fuzzer String
qtext =
    Fuzz.map (String.fromChar << Char.fromCode) <|
        Fuzz.oneOf <|
            List.map Fuzz.constant <|
                List.concat
                    [ List.range 0 12
                    , List.range 14 33
                    , List.range 35 91
                    , List.range 93 127
                    ]


linearWhiteSpace : Fuzzer String
linearWhiteSpace =
    Fuzz.map String.concat <|
        Fuzz.list <|
            Fuzz.map2 (++)
                (Fuzz.oneOf <| List.map Fuzz.constant [ "\x0D\n", "" ])
                (Fuzz.map (String.fromChar << Char.fromCode) <| Fuzz.oneOf <| List.map Fuzz.constant [ 32, 9 ])


quotedPair : Fuzzer String
quotedPair =
    Fuzz.map (\i -> String.fromList [ '\\', Char.fromCode i ]) <|
        Fuzz.oneOf <|
            List.map Fuzz.constant <|
                List.range 0 127


token : Fuzzer String
token =
    Fuzz.map String.fromList <|
        nonEmptyList <|
            Fuzz.oneOf
                [ upper
                , lower
                , digit
                , tokenMark
                ]


tokenMark : Fuzzer Char
tokenMark =
    Fuzz.oneOf <| List.map Fuzz.constant [ '!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '^', '_', '`', '{', '|', '}', '~' ]


data : Fuzzer String
data =
    Fuzz.map (String.concat) <| Fuzz.list uric


uric : Fuzzer String
uric =
    Fuzz.oneOf
        [ reserved
        , unreserved
        , escaped
        ]


reserved : Fuzzer String
reserved =
    Fuzz.map String.fromChar <|
        Fuzz.oneOf <|
            List.map Fuzz.constant [ ';', '/', '?', ':', '@', '&', '=', '+', '$', ',' ]


unreserved : Fuzzer String
unreserved =
    Fuzz.map String.fromChar <|
        Fuzz.oneOf
            [ upper
            , lower
            , digit
            , unreservedMark
            ]


unreservedMark : Fuzzer Char
unreservedMark =
    Fuzz.oneOf <| List.map Fuzz.constant [ '-', '_', '.', '!', '~', '*', '\'', '(', ')' ]


escaped : Fuzzer String
escaped =
    Fuzz.map2 (\hex1 hex2 -> String.fromList [ '%', hex1, hex2 ])
        hexDigit
        hexDigit


hexDigit : Fuzzer Char
hexDigit =
    Fuzz.oneOf <|
        List.map (Fuzz.constant << Char.fromCode) <|
            List.concat
                [ List.range 48 57
                , List.range 65 70
                , List.range 97 102
                ]


upper : Fuzzer Char
upper =
    Fuzz.oneOf <|
        List.map (Fuzz.constant << Char.fromCode) <|
            List.range 65 90


lower : Fuzzer Char
lower =
    Fuzz.oneOf <|
        List.map (Fuzz.constant << Char.fromCode) <|
            List.range 97 122


digit : Fuzzer Char
digit =
    Fuzz.oneOf <|
        List.map (Fuzz.constant << Char.fromCode) <|
            List.range 48 57


nonEmptyList : Fuzzer a -> Fuzzer (List a)
nonEmptyList f =
    Fuzz.map2 (::) f <| Fuzz.list f
