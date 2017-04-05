module SweetPoll exposing (Config, defaultConfig, Model, Msg, init, UpdateResult, update)

{-|

# Configuration
@docs Config, defaultConfig

# Elm Artchitecture
@docs Model, Msg, init, UpdateResult, update
-}

import Http
import Json.Decode as Json
import Process
import Task
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
type Msg data
    = PollResult (Result Http.Error data)


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
init : Config data -> ( Model data, Cmd (Msg data) )
init config =
    Model
        { delayMultiplier = 1.0
        , sameCount = 1
        , lastData = Nothing
        , config = config
        }
        |> runPoll
        |> -- lastData and error will always be Nothing here,
           -- so strip it out to keep init's type signature nicer
           (\result -> ( result.sweetPollModel, result.cmd ))


{-|

 - sweetPollModel: the new state of the SweetPoll
 - currentData: the most recent data received by the SweetPoll
 - error: any new error occurring in the current update cycle
 - cmd: a Cmd to keep the SweetPoll running
-}
type alias UpdateResult data =
    { sweetPollModel : Model data
    , currentData : Maybe data
    , error : Maybe Http.Error
    , cmd : Cmd (Msg data)
    }


{-| The SweetPoll StartApp-style update function
-}
update : Msg data -> Model data -> UpdateResult data
update action (Model model) =
    let
        newDelayMultiplier =
            model.delayMultiplier * model.config.delayMultiplier
    in
        case action of
            PollResult (Ok newData) ->
                let
                    dataChanged =
                        Just newData /= model.lastData

                    ( newDelayMultiplier, newSameCount ) =
                        if dataChanged then
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

            PollResult (Err error) ->
                -- If there was an error, increase the delay and try again.
                -- Once we hit maxDelay, give up. (Something's probably irreparably broken.)
                if model.config.delay * newDelayMultiplier <= model.config.maxDelay then
                    Model { model | delayMultiplier = newDelayMultiplier }
                        |> runPoll
                        |> (\result -> { result | error = Just error })
                else
                    { sweetPollModel = Model model
                    , currentData = model.lastData
                    , error = Just error
                    , cmd = Cmd.none
                    }


runPoll : Model data -> UpdateResult data
runPoll (Model model) =
    { sweetPollModel = Model model
    , currentData = model.lastData
    , error = Nothing
    , cmd =
        Process.sleep (model.config.delay * model.delayMultiplier)
            |> Task.andThen (\_ -> Http.toTask <| Http.get model.config.url model.config.decoder)
            |> Task.attempt PollResult
    }
