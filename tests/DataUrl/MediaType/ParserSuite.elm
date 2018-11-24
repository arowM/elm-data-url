module DataUrl.MediaType.ParserSuite exposing (suite)

import DataUrl.MediaType.Internal exposing (MediaType(..))
import DataUrl.MediaType.Parser exposing (..)
import Fuzz exposing (Fuzzer, string)
import Test exposing (..)
import Utils exposing (shouldParse)


suite : Test
suite =
    describe "DataUrl.MediaType.Parser"
        [ describe "restrictedName"
            [ test "cannot accept empty string" <|
                \_ ->
                    shouldParse restrictedName "" <|
                        Err ()
            , test "cannot contain `!`" <|
                \_ ->
                    shouldParse restrictedName "!" <|
                        Err ()
            , test "cannot contain `?`" <|
                \_ ->
                    shouldParse restrictedName "1abc3-4?foobar" <|
                        Ok "1abc3-4"
            ]
        , describe "parameter"
            [ test "have to contain '='" <|
                \_ ->
                    shouldParse parameter "foobar" <|
                        Err ()
            , test "cannot contain '@' in key" <|
                \_ ->
                    shouldParse parameter "f@oo=bar" <|
                        Err ()
            , test "should be parsed" <|
                \_ ->
                    shouldParse parameter "foo=bar" <|
                        Ok ( "foo", "bar" )
            , test "can use quoted string in value" <|
                \_ ->
                    shouldParse parameter "foo=\"bar\"" <|
                        Ok ( "foo", "\"bar\"" )
            , test "cannot use quoted string in key" <|
                \_ ->
                    shouldParse parameter "\"foo\"=bar" <|
                        Err ()
            , test "can use quoted pair in quoted value" <|
                \_ ->
                    shouldParse parameter "foo=\"bar\\t\"" <|
                        Ok ( "foo", "\"bar\\t\"" )
            , test "can use quoted pair in the end of quoted value" <|
                \_ ->
                    shouldParse parameter "foo=\"\\tbar\"" <|
                        Ok ( "foo", "\"\\tbar\"" )
            , test "cannot use quoted pair in unquoted value" <|
                \_ ->
                    shouldParse parameter "foo=bar\\t" <|
                        Ok ( "foo", "bar" )
            ]
        , describe "core"
            [ test "have to contain '/'" <|
                \_ ->
                    shouldParse core "foobar" <|
                        Err ()
            , test "cannot contain '/' at the first" <|
                \_ ->
                    shouldParse core "/foobar" <|
                        Err ()
            , test "cannot contain '/' at the end" <|
                \_ ->
                    shouldParse core "foobar/" <|
                        Err ()
            , test "should be parsed" <|
                \_ ->
                    shouldParse core "foo/bar" <|
                        Ok ( "foo", "bar" )
            ]
        , describe "mediaType"
            [ test "should be parsed" <|
                \_ ->
                    shouldParse mediaType "foo/bar;key=\"val\"extra" <|
                        Ok <|
                            MediaType
                                { type_ =
                                    ( "foo", "bar" )
                                , parameters =
                                    [ ( "key", "\"val\"" )
                                    ]
                                }
            , test "should be parsed without parameters" <|
                \_ ->
                    shouldParse mediaType "foo/bar" <|
                        Ok <|
                            MediaType
                                { type_ =
                                    ( "foo", "bar" )
                                , parameters =
                                    []
                                }
            , test "can have multiple parameters" <|
                \_ ->
                    shouldParse mediaType "foo/bar;key1=val1;key2=\"val2\"extra" <|
                        Ok <|
                            MediaType
                                { type_ =
                                    ( "foo", "bar" )
                                , parameters =
                                    [ ( "key1"
                                      , "val1"
                                      )
                                    , ( "key2"
                                      , "\"val2\""
                                      )
                                    ]
                                }
            , test "cannot be parsed without type and subtype" <|
                \_ ->
                    shouldParse mediaType ";key1=val1" <|
                        Err ()
            , test "cannot be parsed without type" <|
                \_ ->
                    shouldParse mediaType "/subtype;key1=val1" <|
                        Err ()
            , test "cannot be parsed without subtype" <|
                \_ ->
                    shouldParse mediaType "type/;key1=val1" <|
                        Err ()
            , test "cannot be parsed without '/'" <|
                \_ ->
                    shouldParse mediaType "typesubtype;key1=val1" <|
                        Err ()
            , test "should be parsed till \";data\"" <|
                \_ ->
                    shouldParse mediaType "foo/bar;data" <|
                        Ok <|
                            MediaType
                                { type_ =
                                    ( "foo", "bar" )
                                , parameters =
                                    []
                                }
            , test "should be parsed till \";data\" even if it has trailing '='" <|
                \_ ->
                    shouldParse mediaType "foo/bar;data=" <|
                        Ok <|
                            MediaType
                                { type_ =
                                    ( "foo", "bar" )
                                , parameters =
                                    []
                                }
            , test "can contain parameter key named \"data\"" <|
                \_ ->
                    shouldParse mediaType "foo/bar;key1=val1;data=val" <|
                        Ok <|
                            MediaType
                                { type_ =
                                    ( "foo", "bar" )
                                , parameters =
                                    [ ( "key1", "val1" )
                                    , ( "data", "val" )
                                    ]
                                }
            , test "can contain only parameter key named \"data\"" <|
                \_ ->
                    shouldParse mediaType "foo/bar;data=val" <|
                        Ok <|
                            MediaType
                                { type_ =
                                    ( "foo", "bar" )
                                , parameters =
                                    [ ( "data", "val" )
                                    ]
                                }
            ]
        ]
