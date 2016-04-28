module SweetPoll (Config, defaultConfig, Action(..), Model, SweetPoll, create) where

{-|

@docs Config, defaultConfig, SweetPoll, create, Action, Model
-}

import Testable.Effects as Effects exposing (Effects)
import Testable.Http as Http
import Testable.Task as Task exposing (Task)
import Json.Decode as Json
import Time exposing (Time)


{-| -}
type alias Config data =
  { url : String
  , decoder : Json.Decoder data
  , delay : Time
  , samesBeforeDelay : Int
  , delayMultiplier : Float
  , maxDelay : Time
  }


{-| Default configuration for SweetPoll
-}
defaultConfig : Json.Decoder data -> String -> Config data
defaultConfig decoder url =
  { decoder = decoder
  , url = url
  , delay = 7 * Time.second
  , samesBeforeDelay = 3
  , delayMultiplier = 1.2
  , maxDelay = 3 * Time.minute
  }


{-| -}
type Action data
  = PollSuccess data
  | PollFailure Http.Error


fromResult : Result Http.Error data -> Action data
fromResult result =
  case result of
    Ok data ->
      PollSuccess data

    Err error ->
      PollFailure error


{-| Private state of the SweetPoll component
-}
type Model
  = Model ()


{-| -}
type alias SweetPoll data =
  { init : ( Model, Effects (Action data) )
  , update : Action data -> Model -> ( Model, Effects (Action data) )
  }


{-| Create a SweetPoll component.

    SweetPoll.create
      { decoder = myDataDecoder
      , url = "https://myserver.example.com/json"
      }

-}
create : Config data -> SweetPoll data
create config =
  let
    pollEffect =
      Task.sleep config.delay
        |> Task.andThen (\_ -> Http.get config.decoder config.url)
        |> Task.toResult
        |> Task.map fromResult
        |> Effects.task
  in
    { init =
        ( Model ()
        , pollEffect
        )
    , update =
        \action model ->
          ( model, pollEffect )
    }
