module Main exposing (..)

import BeautifulExample
import Color
import Html
import Json.Decode as Decode
import SweetPoll


type alias Model =
    { data : Maybe String
    , error : Maybe String
    , sweetPoll : SweetPoll.Model String
    }


main : Program Never Model (SweetPoll.Msg String)
main =
    let
        config =
            SweetPoll.defaultConfig
                (Decode.field "fulldate" Decode.string)
                -- From http://tiny.cc/currenttime
                "https://script.google.com/macros/s/AKfycbyd5AcbAnWi2Yn0xhFRbyzS4qMq1VucMVgVvhul5XqS9HkAyJY/exec"

        sweetPollInit =
            SweetPoll.init config
    in
        Html.program
            { init =
                ( { data = Nothing
                  , error = Nothing
                  , sweetPoll = sweetPollInit |> Tuple.first
                  }
                , sweetPollInit |> Tuple.second
                )
            , update =
                \action model ->
                    case Debug.log "update" <| SweetPoll.update action model.sweetPoll of
                        { sweetPollModel, newData, error, cmd } ->
                            ( { sweetPoll = sweetPollModel
                              , data = newData
                              , error = error |> Maybe.map toString
                              }
                            , cmd
                            )
            , view =
                \model ->
                    Html.div []
                        [ Html.text ("new data: " ++ toString model.data)
                        , Html.hr [] []
                        , Html.text ("error: " ++ toString model.error)
                        ]
                        |> BeautifulExample.view
                            { title = "elm-sweet-poll"
                            , details = Just """HTTP polling with smart retry backoff."""
                            , color = Just Color.red
                            , maxWidth = 400
                            , githubUrl = Just "https://github.com/NoRedInk/elm-sweet-poll"
                            , documentationUrl = Just "http://package.elm-lang.org/packages/NoRedInk/elm-sweet-poll/latest/SweetPoll"
                            }
            , subscriptions = \_ -> Sub.none
            }
