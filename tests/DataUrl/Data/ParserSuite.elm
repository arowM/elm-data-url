module DataUrl.Data.ParserSuite exposing (suite)

import DataUrl.Data.Parser exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, string)
import Parser exposing (Parser, getChompedString)
import Test exposing (..)
import Utils exposing (shouldParse)


suite : Test
suite =
    describe "DataUrl.MediaType.Parser"
        [ describe "reserved"
            [ test "cannot accept empty string" <|
                \_ ->
                    shouldChomp reserved "" <|
                        Err ()
            , test "cannot contain `!`" <|
                \_ ->
                    shouldChomp reserved "!" <|
                        Err ()
            , test "can contain `?`" <|
                \_ ->
                    shouldChomp reserved "??1a!" <|
                        Ok "?"
            ]
        , describe "unreserved"
            [ test "cannot accept empty string" <|
                \_ ->
                    shouldChomp unreserved "" <|
                        Err ()
            , test "cannot contain '?'" <|
                \_ ->
                    shouldChomp unreserved "?" <|
                        Err ()
            , test "can contain `!`" <|
                \_ ->
                    shouldChomp unreserved "!!1a?" <|
                        Ok "!"
            ]
        , describe "escaped"
            [ test "have to contain two hexes" <|
                \_ ->
                    shouldChomp escaped "%3G" <|
                        Err ()
            , test "have to begin with '%'" <|
                \_ ->
                    shouldChomp escaped "X1a" <|
                        Err ()
            , test "can use lower alphabet" <|
                \_ ->
                    shouldChomp escaped "%af" <|
                        Ok "%af"
            , test "can use upper alphabet" <|
                \_ ->
                    shouldChomp escaped "%AF" <|
                        Ok "%AF"
            ]
        , describe "data_"
            [ test "can be empty string" <|
                \_ ->
                    shouldParse data_ "" <|
                        Ok ""
            , test "can be sequencial `reserved`s" <|
                \_ ->
                    shouldParse data_ "?;/:=,+%foo" <|
                        Ok "?;/:=,+"
            , test "can be sequencial `unreserved`s" <|
                \_ ->
                    shouldParse data_ "!-_.'bar()%foo" <|
                        Ok "!-_.'bar()"
            , test "can be sequencial `excaped`s" <|
                \_ ->
                    shouldParse data_ "%1f%aB%B0%foo" <|
                        Ok "%1f%aB%B0"
            , test "can be mix of `reserved`s, `unreserved`s and `excaped`s" <|
                \_ ->
                    shouldParse data_ "%1f%aB!-_%B0?;/:x%0xxx" <|
                        Ok "%1f%aB!-_%B0?;/:x"
            ]
        ]


shouldChomp : Parser () -> String -> Result () String -> Expectation
shouldChomp p =
    shouldParse (getChompedString p)
