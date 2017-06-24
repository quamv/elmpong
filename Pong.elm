
module Pong exposing (main)

import Html exposing (..)
import Time exposing (..)
import Keyboard exposing (..)
import Random exposing (..)
import AnimationFrame exposing (diffs)
import Char exposing (toCode, fromCode)
import String exposing (fromChar)
import Set exposing (..)

import Model exposing (..)
import View exposing (..)
import CollisionHelpers exposing (..)
import Helpers exposing (..)
import PaddleHelpers exposing (..)
import Line exposing (..)
import ModelHelpers exposing (..)
import AILogic exposing (..)


main =
    Html.program {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions
        }



{-
Creates the initial model and random ball direction
-}
init : ( Model, Cmd Msg )
init = ({
        yesno = False
        , nextballDirection = R2L -- is always 'the next' direction
        , gameState = Playing
        , score = (Score 1 2)
        , ball = defaultBall
        , lpaddle = Paddle 500 Left
        , rpaddle = Paddle 500 Right
        , keys = defaultSet
        , reflectionMode = PaddleCenterRelative
        , showSettings = False
        , player1type = CPU
        , player2type = CPU
        , collided = False
        , intercepts = getPaddleIntercepts defaultBall
        , pauseAfterGoal = True
        , colorsSet = { paddleFill="black", ballFill="white", bgFill="grey"}
    }
    , Cmd.none) --,Random.generate NewBallConstantVX (int -8 8))


{-
Subscriptions
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        Paused ->
            -- when paused, listen only for key presses
            Sub.batch
                [ Keyboard.downs KeyDownMsg
                , Keyboard.ups KeyUpMsg
                ]
        Playing ->
            -- when playing, listen for animation frames, ai update message, and key presses
            if model.player1type == CPU || model.player2type == CPU then
                -- cpu players require ai ticks
                Sub.batch
                    [
                    AnimationFrame.diffs Frame
                    , Time.every (100 * millisecond) AITick
                    , Keyboard.downs KeyDownMsg
                    , Keyboard.ups KeyUpMsg
                    ]
            else
                -- no cpu players, no need for ai ticks
                Sub.batch
                    [ AnimationFrame.diffs Frame
                    , Keyboard.downs KeyDownMsg
                    , Keyboard.ups KeyUpMsg
                    ]

{-
Update
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        TogglePaused ->
            -- toggle game state, Paused/Playing
            ({ model | gameState = togglePaused model.gameState }, Cmd.none)

        RequestNewBall ->
            -- generic 'new ball' request
            ( model, randomizeBallConstantVX)

        Frame timeDiff ->
            -- generate a new animation frame
            stepGameForward model timeDiff

        KeyDownMsg k ->
            -- Process a key down
            keyDown k model

        KeyUpMsg k ->
            -- Only thing we do here is remove a key from our set of current keys.
            ( { model | keys = (removeKey (fromCode k) model.keys) }, Cmd.none )

        NewBallConstantVX vy ->
            -- create a new ball with a constant horizontal speed (VX) and 'vy' vertical speed
            -- toggle the direction left to right via model.direction flag
            -- NewBallConstantVX vy -> newBallConstantVX model vy
            newBallConstantVX model vy

        SetReflectionMode reflectionMode ->
            ({ model | reflectionMode = reflectionMode}, Cmd.none)

        ToggleShowSettings ->
            ({ model | showSettings = not model.showSettings}, Cmd.none)

        TogglePauseAfterGoal ->
            ({ model | pauseAfterGoal = not model.pauseAfterGoal}, Cmd.none)

        AITick _ ->
            ({ model |
                keys = (updateAIPlayers model)
                , collided = False
            }, Cmd.none)

        ToggleActivePlayer side ->
            case side of
                Player1 ->
                    ({ model |
                        player1type = togglePlayerType model.player1type}
                     , Cmd.none)
                Player2 ->
                    ({ model |
                        player2type = togglePlayerType model.player2type}
                     , Cmd.none)

        UpdateSetting setting val ->
            updateSetting model setting val


updateSetting : Model -> ConfigurableString -> String -> (Model, Cmd Msg)
updateSetting model setting val =
    let
        prev = model.colorsSet
    in
    case setting of
        BgFill ->  ({ model | colorsSet = { prev | bgFill = val} } , Cmd.none)
        BallFill -> ({ model | colorsSet = { prev | ballFill = val} } , Cmd.none)
        PaddleFill -> ({ model | colorsSet = { prev | paddleFill = val} }, Cmd.none)



{-
Process a KeyDown message
If the key is a special command, run it
Otherwise store the key in our set of currently pressed keys
-}
keyDown : Keyboard.KeyCode -> Model -> (Model, Cmd Msg)
keyDown k model =
    if k == (toCode 'P') then
        update TogglePaused model
    else if k == (toCode 'N') then
        update RequestNewBall model
    else
        if isCpuPlayerKey k model.player1type model.player2type then
            -- ignore keypresses for CPU players
            (model, Cmd.none)
        else
            ({ model | keys = (addKey (fromCode k) model.keys) }, Cmd.none)


{-
Generates the next frame, checks for goal scored
Returns the updated model and a Cmd Msg (or Cmd.none)
-}
stepGameForward : Model -> Float -> ( Model, Cmd Msg )
stepGameForward model timeDiff =
    let
        -- Normalize the time diff based on normal FPS vs current FPS
        diff =
            timeDiff / 50

        -- get the ball's assumed next straight-line xy coord
        stepball =
            stepBall diff model.ball

        -- check for/respond to collisions, determine ball's final xy coord
        collisionResults =
            doCollisions model stepball

        -- grab the Ball
        ball =
            collisionResults.ball

        -- update intercepts
        newIntercepts =
            if collisionResults.collided then
                -- re-evaluate intercepts
                getPaddleIntercepts ball
            else
                model.intercepts

        -- update the paddle locations based on user input
        (nextlpaddle,nextrpaddle) =
            stepPaddles model diff

        -- generate new model
        newModel = ({ model |
            ball = ball,
            intercepts = newIntercepts,
            lpaddle = nextlpaddle,
            rpaddle = nextrpaddle
            })

    in
        -- if a goal has apparently been scored, update score, reset ball
        case checkGoalScored ball of
            Just player ->
                ({ newModel |
                        score = addPoint player model.score
                        ,gameState = postGoalState model.pauseAfterGoal
                }, randomizeBallConstantVX)

            Nothing ->
                -- no goal scoresd
                (newModel, Cmd.none)


postGoalState : Bool -> GameState
postGoalState pauseAfterGoal =
    case pauseAfterGoal of
        True -> Paused
        False -> Playing


{-
newBallConstantVX 'resets' the ball and sets the initial velocity based
on model.direction being either L2R or R2L
in this case we are using a constant x-velocity which seems appropriate
to make sure the game play stays consistent
TODO: could we play around with the "diff" parameter for speed manipulation?
-}
newBallConstantVX : Model -> Int -> (Model, Cmd Msg)
newBallConstantVX model vy =
    let
        vx = case model.nextballDirection of
            L2R -> ballVX
            R2L -> 0 - ballVX

        newball = Ball centerPt (Velocity2d vx (toFloat vy))
    in
        ({ model |
            nextballDirection = flipLR model.nextballDirection
            , intercepts = getPaddleIntercepts newball
            , ball = newball}
        , Cmd.none)



{-
Takes a Player and current score
Adds 1 point to specified player's score.
Returns updated score
-}
addPoint : PlayerSide -> Score -> Score
addPoint player (Score l r) =
    case player of
        Player1 -> Score (l + 1) r
        Player2 -> Score l (r + 1)


{-
Generates a random ball velocity between -ballVX and ballVX
-}
randomizeBallConstantVX =
    Random.generate NewBallConstantVX (int -ballVX ballVX)


{-
Takes a Ball
x-position determines if a goal is scored
Returns True if a goal was scored, False otherwise
-}
checkGoalScored : Ball -> Maybe PlayerSide
checkGoalScored ball =
    if ball.pos.x < 2 then
        Just Player1
    else if ball.pos.x > 998 then
        Just Player2
    else
        Nothing


{-
Toggle between Paused/Playing state
-}
togglePaused : GameState -> GameState
togglePaused currentState =
    case currentState of
        Paused -> Playing
        Playing -> Paused






