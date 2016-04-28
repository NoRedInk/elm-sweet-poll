module SweetPoll (Config, defaultConfig, Action(..), Model, init, update) where

{-|

@docs Config, defaultConfig, init, update, Action, Model
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
type Model data
  = Model
      { delayMultiplier : Float
      , sameCount : Int
      , lastData : Maybe data
      , config : Config data
      }


{-| -}
init : Config data -> ( Model data, Effects (Action data) )
init config =
  Model
    { delayMultiplier = 1.0
    , sameCount = 1
    , lastData = Nothing
    , config = config
    }
    |> runPoll


{-| -}
update : Action data -> Model data -> ( Model data, Effects (Action data) )
update action (Model model) =
  let
    newDelayMultiplier =
      model.delayMultiplier * model.config.delayMultiplier
  in
    case action of
      PollSuccess newData ->
        let
          ( newDelayMultiplier, newSameCount ) =
            if (Just newData /= model.lastData) then
              -- If we got a different response, reset everything.
              ( 1.0, 1 )
            else if model.sameCount + 1 >= model.config.samesBeforeDelay then
              -- If we got the same response too many times in a row, up the delay.
              ( model.delayMultiplier * 1.2, model.sameCount + 1 )
            else
              -- Otherwise, leave everything the same.
              ( model.delayMultiplier, model.sameCount + 1 )
        in
          Model
            { model
              | lastData = Just newData
              , delayMultiplier = newDelayMultiplier
              , sameCount = newSameCount
            }
            |> runPoll

      PollFailure _ ->
        -- If there was an error, increase the delay and try again.
        -- Once we hit maxDelay, give up. (Something's probably irreparably broken.)
        if model.config.delay * newDelayMultiplier <= model.config.maxDelay then
          Model { model | delayMultiplier = newDelayMultiplier }
            |> runPoll
        else
          ( Model model, Effects.none )


runPoll : Model data -> ( Model data, Effects (Action data) )
runPoll (Model model) =
  ( Model model
  , Task.sleep (model.config.delay * model.delayMultiplier)
      |> Task.andThen (\_ -> Http.get model.config.decoder model.config.url)
      |> Task.toResult
      |> Task.map fromResult
      |> Effects.task
  )
