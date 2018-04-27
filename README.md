# Elm Sweet Poll

A library for HTTP polling, which automatically backs off when the server
response doesn't change.

Follows the Elm architecture.

## Example

```
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
                    case SweetPoll.update action model.sweetPoll of
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
            , subscriptions = \_ -> Sub.none
            }
```

This example is also included in the examples folder in the source code. Build with:

```
> cd elm-sweet-poll
> ./build_all.sh
> open examples/index.html

```
