module Main exposing (Model, main)

import Browser
import Html
import Json.Decode as Decode
import SweetPoll


type alias Model =
    { data : Maybe String
    , error : Maybe String
    , sweetPoll : SweetPoll.Model String
    }


main : Program () Model (SweetPoll.Msg String)
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
    Browser.element
        { init =
            \_ ->
                ( { data = Nothing
                  , error = Nothing
                  , sweetPoll = sweetPollInit |> Tuple.first
                  }
                , sweetPollInit |> Tuple.second
                )
        , update =
            \action model ->
                case SweetPoll.update action model.sweetPoll of
                    { sweetPollModel, newData, error, cmd } ->
                        ( { sweetPoll = sweetPollModel
                          , data = newData
                          , error = error |> Maybe.map Debug.toString
                          }
                        , cmd
                        )
        , view =
            \model ->
                Html.div []
                    [ Html.text ("new data: " ++ Debug.toString model.data)
                    , Html.hr [] []
                    , Html.text ("error: " ++ Debug.toString model.error)
                    ]
        , subscriptions = \_ -> Sub.none
        }
