module DataUrl.Data.ParserSuite exposing (..)

import DataUrl.Data.Parser exposing (..)
import Fuzz exposing (Fuzzer, string)
import Test exposing (..)
import Utils exposing (isDelayed, shouldParse)


suite : Test
suite =
    describe "DataUrl.MediaType.Parser"
        [ describe "reserved"
            [ fuzz string "is delayed" <|
                isDelayed reserved
            , test "cannot accept empty string" <|
                \_ ->
                    shouldParse reserved "" <|
                        Err 1
            , test "cannot contain `!`" <|
                \_ ->
                    shouldParse reserved "!" <|
                        Err 1
            , test "can contain `?`" <|
                \_ ->
                    shouldParse reserved "??1a!" <|
                        Ok "?"
            ]
        , describe "unreserved"
            [ fuzz string "is delayed" <|
                isDelayed unreserved
            , test "cannot accept empty string" <|
                \_ ->
                    shouldParse unreserved "" <|
                        Err 1
            , test "cannot contain '?'" <|
                \_ ->
                    shouldParse unreserved "?" <|
                        Err 1
            , test "can contain `!`" <|
                \_ ->
                    shouldParse unreserved "!!1a?" <|
                        Ok "!"
            ]
        , describe "escaped"
            [ fuzz string "is delayed" <|
                isDelayed escaped
            , test "have to contain two hexes" <|
                \_ ->
                    shouldParse escaped "%3G" <|
                        Err 1
            , test "have to begin with '%'" <|
                \_ ->
                    shouldParse escaped "X1a" <|
                        Err 1
            , test "can use lower alphabet" <|
                \_ ->
                    shouldParse escaped "%af" <|
                        Ok "%af"
            , test "can use upper alphabet" <|
                \_ ->
                    shouldParse escaped "%AF" <|
                        Ok "%AF"
            ]
        , describe "data_"
            [ fuzz string "is delayed" <|
                isDelayed data_
            , test "can be empty string" <|
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
