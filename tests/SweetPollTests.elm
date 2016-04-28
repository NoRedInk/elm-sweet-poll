module SweetPollTests (..) where

import ElmTest exposing (..)
import Json.Decode as Json
import SweetPoll exposing (SweetPoll)
import Time
import Testable.Http as Http
import Http as RealHttp
import Testable.TestContext exposing (..)
import Time


type MyData
  = MyData String


myDataDecoder : Json.Decoder MyData
myDataDecoder =
  Json.string |> Json.map MyData


subject : SweetPoll MyData
subject =
  SweetPoll.create
    <| SweetPoll.defaultConfig myDataDecoder "https://example.com/"


container :
  Component
    (SweetPoll.Action MyData)
    { sweetPoll : SweetPoll.Model
    , lastAction : Maybe (SweetPoll.Action MyData)
    }
container =
  { init =
      case subject.init of
        ( model, effects ) ->
          ( { sweetPoll = model, lastAction = Nothing }
          , effects
          )
  , update =
      \action model ->
        case subject.update action model.sweetPoll of
          ( newModel, effects ) ->
            ( { sweetPoll = newModel
              , lastAction = Just action
              }
            , effects
            )
  }


all : Test
all =
  suite
    "SweetPoll"
    [ container
        |> startForTest
        |> advanceTime (7 * Time.second)
        |> assertHttpRequest
            (Http.getRequest "https://example.com/")
        |> test "makes an initial HTTP request after the delay"
    , let
        successfulNetworkRequest =
          container
            |> startForTest
            |> advanceTime (7 * Time.second)
            |> resolveHttpRequest
                (Http.getRequest "https://example.com/")
                (Http.ok "\"data-1\"")
      in
        suite
          "when initial HTTP request succeeds"
          [ successfulNetworkRequest
              |> currentModel
              |> Result.map .lastAction
              |> assertEqual (Ok <| Just <| SweetPoll.PollSuccess <| MyData "data-1")
              |> test "sends data to the parent"
          , successfulNetworkRequest
              |> advanceTime (7 * Time.second)
              |> assertHttpRequest
                  (Http.getRequest "https://example.com/")
              |> test "makes a new HTTP request after the delay"
          ]
    , let
        failedNetworkRequest =
          container
            |> startForTest
            |> advanceTime (7 * Time.second)
            |> resolveHttpRequest
                (Http.getRequest "https://example.com/")
                (Err RealHttp.RawTimeout)
      in
        suite
          "when initial HTTP request fails"
          [ failedNetworkRequest
              |> currentModel
              |> Result.map .lastAction
              |> assertEqual (Ok <| Just <| SweetPoll.PollFailure <| RealHttp.Timeout)
              |> test "sends data to the parent"
          , failedNetworkRequest
              |> advanceTime (7 * Time.second)
              |> assertHttpRequest
                  (Http.getRequest "https://example.com/")
              |> test "makes a new HTTP request after the delay"
          ]
    ]
