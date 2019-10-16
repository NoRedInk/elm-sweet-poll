module SweetPoll exposing
    ( init, defaultConfig, Config, PollingState
    , Msg, update, UpdateResult
    )

{-|


# Configuration & Set up

@docs init, defaultConfig, Config, PollingState


# Working with responses

@docs Msg, update, UpdateResult

-}

import Http
import Json.Decode as Json exposing (Decoder)
import Process
import Task exposing (Task)


{-|

    - url: Route to poll against
    - decoder: How to handle the data you expect back
    - delay: How long to wait between poll attempts
    - samesBeforeDelay: How many identical responses before increasing the delay
    - delayMultiplier: How much to multiply the delay by when getting identical responses
    - maxDelay: How long the delay should be before we give up on polling

-}
type alias Config data =
    { url : String
    , decoder : Json.Decoder data
    , delay : Float
    , samesBeforeDelay : Int
    , delayMultiplier : Float
    , maxDelay : Float
    }


{-| Default configuration for SweetPoll.
-}
defaultConfig : Json.Decoder data -> String -> Config data
defaultConfig decoder url =
    { decoder = decoder
    , url = url
    , delay = 1000 * 7
    , samesBeforeDelay = 3
    , delayMultiplier = 1.2
    , maxDelay = 1000 * 60 * 3
    }


{-| -}
type Msg data
    = PollResult (Result Http.Error data)


{-| Private state of the SweetPoll component.

    import SweetPoll

    type alias PollingState =
        { sweetPoll : SweetPoll.PollingState ServerData
        , data : ServerData
        , error : Maybe Http.Error
        }

    type alias ServerData =
        {}

-}
type PollingState data
    = PollingState
        { delayMultiplier : Float
        , sameCount : Int
        , lastData : Maybe data
        }


{-| Initialize the SweetPoll behavior.

    import SweetPoll
    import Json.Decode as Decode

    init : (Model, Cmd Msg)
    init =
        let
            (sweetPollModel, sweetPollCommands) =
                "www.example-url.com/get-some-data"
                    |> SweetPoll.defaultConfig (Decode.succeed {})
                    |> SweetPoll.init
                    |> Tuple.mapSecond SweetPollMsg
        in
            ...

-}
init : Config data -> ( PollingState data, Cmd (Msg data) )
init config =
    let
        model =
            PollingState
                { delayMultiplier = 1.0
                , sameCount = 1
                , lastData = Nothing
                }
    in
    ( model, runPoll config model )


{-|

  - newState: the new state of the SweetPoll
  - newData: any new data received by the SweetPoll
  - error: any new Http error occurring in the current update cycle
  - cmd: a Cmd to keep the SweetPoll running

-}
type alias UpdateResult data =
    { newState : PollingState data
    , newData : Maybe data
    , error : Maybe Http.Error
    , cmd : Cmd (Msg data)
    }


{-| Takes the SweetPoll Msg and Model and produces a non-opaque result that you
can work with.

    import SweetPoll

    type Msg
        = SweetPollMsg (SweetPoll.Msg ServerData)

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            SweetPollMsg sweetPollMsg ->
                case SweetPoll.update SweetPoll.defaultConfig sweetPollMsg model.sweetPollModel of
                    { sweetPollModel, newData, error, cmd } ->
                        ( { sweetPollModel = sweetPollModel
                          , data = newData
                          , error = error
                          }
                        , cmd
                        )

-}
update : Config data -> Msg data -> PollingState data -> UpdateResult data
update config action (PollingState model) =
    case action of
        PollResult (Ok newData) ->
            let
                dataChanged =
                    Just newData /= model.lastData

                ( newDelayMultiplier, newSameCount ) =
                    if dataChanged then
                        -- If we got a different response, reset everything.
                        ( 1.0, 1 )

                    else if model.sameCount + 1 >= config.samesBeforeDelay then
                        -- If we got the same response too many times in a row, up the delay.
                        ( model.delayMultiplier * 1.2, model.sameCount + 1 )

                    else
                        -- Otherwise, leave everything the same.
                        ( model.delayMultiplier, model.sameCount + 1 )

                newState =
                    PollingState
                        { model
                            | lastData = Just newData
                            , delayMultiplier = newDelayMultiplier
                            , sameCount = newSameCount
                        }
            in
            { newState = newState
            , newData = Just newData
            , error = Nothing
            , cmd = runPoll config newState
            }

        PollResult (Err error) ->
            let
                newDelayMultiplier =
                    model.delayMultiplier * config.delayMultiplier
            in
            -- If there was an error, increase the delay and try again.
            -- Once we hit maxDelay, give up. (Something's probably irreparably broken.)
            if config.delay * newDelayMultiplier <= config.maxDelay then
                let
                    newState =
                        PollingState { model | delayMultiplier = newDelayMultiplier }
                in
                { newState = newState
                , newData = Nothing
                , error = Just error
                , cmd = runPoll config newState
                }

            else
                { newState = PollingState model
                , newData = Nothing
                , error = Just error
                , cmd = Cmd.none
                }


runPoll : Config data -> PollingState data -> Cmd (Msg data)
runPoll config (PollingState model) =
    Process.sleep (config.delay * model.delayMultiplier)
        |> Task.andThen (\_ -> toTask config.url config.decoder)
        |> Task.attempt PollResult


toTask : String -> Decoder a -> Task Http.Error a
toTask url decoder =
    Http.task
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = url
        , body = Http.emptyBody
        , resolver = resolveJsonResponse decoder
        , timeout = Nothing
        }


{-| `Http.task` requires you to specify your own "resolver". We don't
actually want to do anything special here, so this just attempts to
re-implement the expected behavior from a normal request.

See `elm/http` docs [here.](https://package.elm-lang.org/packages/elm/http/latest/Http#expectStringResponse)

-}
resolveJsonResponse : Decoder a -> Http.Resolver Http.Error a
resolveJsonResponse decoder =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ badUrl ->
                    Err (Http.BadUrl badUrl)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    body
                        |> Json.decodeString decoder
                        |> Result.mapError (Json.errorToString >> Http.BadBody)
