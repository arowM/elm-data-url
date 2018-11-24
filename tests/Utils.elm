module Utils exposing (cannotBeErr, shouldParse)

import Expect exposing (Expectation)
import Parser exposing (..)


cannotBeErr : Parser a -> String -> Expectation
cannotBeErr parser str =
    case run parser str of
        Ok _ ->
            Expect.pass

        _ ->
            Expect.fail "This parser can be `Err`."


shouldParse : Parser a -> String -> Result () a -> Expectation
shouldParse p str res =
    run p str
        |> Result.mapError (\_ -> ())
        |> Expect.equal res
