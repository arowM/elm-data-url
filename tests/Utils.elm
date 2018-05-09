module Utils exposing (..)

import Expect exposing (Expectation)
import Parser exposing (..)


isDelayed : Parser a -> String -> Expectation
isDelayed p str =
    let
        parser : Parser Bool
        parser =
            succeed
                (\f rest -> f rest)
                |= oneOf
                    [ map (\_ -> always True) p
                    , succeed <| \rest -> rest == str
                    ]
                |= keep zeroOrMore (\_ -> True)
                |. end
    in
        case run parser str of
            Ok True ->
                Expect.pass

            _ ->
                Expect.fail "This parser is not delayed."


cannotBeErr : Parser a -> String -> Expectation
cannotBeErr parser str =
    case run parser str of
        Ok _ ->
            Expect.pass

        _ ->
            Expect.fail "This parser can be `Err`."


shouldParse : Parser a -> String -> Result Int a -> Expectation
shouldParse p str res =
    run p str
        |> Result.mapError .col
        |> Expect.equal res
