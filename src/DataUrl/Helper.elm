module DataUrl.Helper exposing
    ( oneOrMore
    , zeroOrMore
    )

import Parser exposing (..)


oneOrMore : Parser String -> Parser String
oneOrMore p =
    succeed (++)
        |= backtrackable p
        |= zeroOrMore p


zeroOrMore : Parser String -> Parser String
zeroOrMore p =
    oneOf
        [ succeed (++)
            |= backtrackable p
            |= lazy (\_ -> zeroOrMore p)
        , succeed ""
        ]
