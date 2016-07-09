module Tests exposing (..)

import ElmTest exposing (..)
import SweetPollTests


all : Test
all =
    suite
        "NoRedInk/elm-sweet-poll"
        [ SweetPollTests.all
        ]
