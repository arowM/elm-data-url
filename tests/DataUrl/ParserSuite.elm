module DataUrl.ParserSuite exposing (..)

import DataUrl.Data exposing (Data(..))
import DataUrl.Internal as DataUrl
import DataUrl.MediaType.Internal as MediaType
import DataUrl.Parser exposing (..)
import Fuzz exposing (Fuzzer, string)
import Test exposing (..)
import Utils exposing (cannotBeErr, shouldParse)


suite : Test
suite =
    describe "DataUrl.Parser"
        [ describe "isBase64"
            [ fuzz string "cannot be `Err`" <|
                cannotBeErr isBase64
            , test "is `True` if it exactly matches" <|
                \_ ->
                    shouldParse isBase64 ";base64" <|
                        Ok True
            , test "is `True` even if it has trailing charactors" <|
                \_ ->
                    shouldParse isBase64 ";base64xxx" <|
                        Ok True
            , test "is `False` if it has leading charactors" <|
                \_ ->
                    shouldParse isBase64 "x;base64" <|
                        Ok False
            , test "is `False` if it lacks leading ';'" <|
                \_ ->
                    shouldParse isBase64 "base64" <|
                        Ok False
            , test "is `False` if it lacks trailing '4'" <|
                \_ ->
                    shouldParse isBase64 ";base6" <|
                        Ok False
            ]
        , describe "dataUrl"
            [ test "can omit media type on textual data" <|
                \_ ->
                    shouldParse dataUrl "data:,Hello%2C%20World!" <|
                        Ok <|
                            DataUrl.init
                                Nothing
                                (Textual "Hello%2C%20World!")
            , test "can omit media type on base64 encoded data" <|
                \_ ->
                    shouldParse dataUrl "data:;base64,Hello%2C%20World!" <|
                        Ok <|
                            DataUrl.init
                                Nothing
                                (Base64 "Hello%2C%20World!")
            , test "can parse textual data with media type" <|
                \_ ->
                    shouldParse dataUrl "data:text/html,%3Ch1%3EHello%2C%20World!%3C%2Fh1%3E" <|
                        Ok <|
                            DataUrl.init
                                (Just (MediaType.init ( "text", "html" ) []))
                                (Textual "%3Ch1%3EHello%2C%20World!%3C%2Fh1%3E")
            , test "can parse textual data with media type and its parameters" <|
                \_ ->
                    shouldParse dataUrl "data:text/plain;charset=US-ASCII,Hello%2C%20World!" <|
                        Ok <|
                            DataUrl.init
                                (Just (MediaType.init ( "text", "plain" ) [ ( "charset", "US-ASCII" ) ]))
                                (Textual "Hello%2C%20World!")
            , test "can parse base64 data with media type" <|
                \_ ->
                    shouldParse dataUrl "data:text/plain;base64,SGVsbG8sIFdvcmxkIQ%3D%3D" <|
                        Ok <|
                            DataUrl.init
                                (Just (MediaType.init ( "text", "plain" ) []))
                                (Base64 "SGVsbG8sIFdvcmxkIQ%3D%3D")
            , test "can parse base64 data with media type and its parameters" <|
                \_ ->
                    shouldParse dataUrl "data:text/plain;charset=utf-8;base64,SGVsbG8sIFdvcmxkIQ%3D%3D" <|
                        Ok <|
                            DataUrl.init
                                (Just (MediaType.init ( "text", "plain" ) [ ( "charset", "utf-8" ) ]))
                                (Base64 "SGVsbG8sIFdvcmxkIQ%3D%3D")
            , test "cannot contain raw html" <|
                \_ ->
                    shouldParse dataUrl "data:text/html,<script>alert('hi');</script>" <|
                        Err ()
            , test "can contain \"base64\" in media type parameters of base64 encoded data" <|
                \_ ->
                    shouldParse dataUrl "data:text/html;base64=foo;base64,SGVsbG8sIFdvcmxkIQ%3D%3D" <|
                        Ok <|
                            DataUrl.init
                                (Just (MediaType.init ( "text", "html" ) [ ( "base64", "foo" ) ]))
                                (Base64 "SGVsbG8sIFdvcmxkIQ%3D%3D")
            , test "can contain \"base64\" in media type parameters of textual data" <|
                \_ ->
                    shouldParse dataUrl "data:text/html;base64=foo,SGVsbG8sIFdvcmxkIQ%3D%3D" <|
                        Ok <|
                            DataUrl.init
                                (Just (MediaType.init ( "text", "html" ) [ ( "base64", "foo" ) ]))
                                (Textual "SGVsbG8sIFdvcmxkIQ%3D%3D")
            , test "can be no `data` part" <|
                \_ ->
                    shouldParse dataUrl "data:text/html," <|
                        Ok <|
                            DataUrl.init
                                (Just (MediaType.init ( "text", "html" ) []))
                                (Textual "")
            , test "cannot ommit trailing ',' after media type" <|
                \_ ->
                    shouldParse dataUrl "data:text/html" <|
                        Err ()
            , test "cannot ommit trailing ',' after \";base64\"" <|
                \_ ->
                    shouldParse dataUrl "data:text/html;base64" <|
                        Err ()
            , test "cannot contain invlid string in `;base64` part" <|
                \_ ->
                    shouldParse dataUrl "data:text/html;base64x," <|
                        Err ()
            ]
        ]
