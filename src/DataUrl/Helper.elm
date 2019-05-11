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
    loop [] (zeroOrMoreP p)


zeroOrMoreP : Parser String -> List String -> Parser (Step (List String) String)
zeroOrMoreP p revStr =
    oneOf
        [ map
            (\str ->
                Loop (str :: revStr)
            )
            (backtrackable p)
        , succeed ()
            |> map (\_ -> Done (String.concat <| List.reverse revStr))
        ]
