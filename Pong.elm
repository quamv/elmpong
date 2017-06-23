
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

        AITick _ ->
            ({ model | keys = (updateAIPlayers model)}, Cmd.none)


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
        ball =
            doCollisions model stepball

        -- update the paddle locations based on user input
        (nextlpaddle,nextrpaddle) =
            stepPaddles model diff

        -- generate new model
        newModel = ({ model |
            ball = ball,
            lpaddle = nextlpaddle,
            rpaddle = nextrpaddle
            })
    in
        -- if a goal has apparently been scored, update score, reset ball
        case checkGoalScored ball of
            Just Player1 ->
                -- player1 scored
                ({ newModel |
                        score = addPoint Player1 model.score
                        ,gameState = Paused
                }, randomizeBallConstantVX)

            Just Player2 ->
                -- player2 scored
                ({ newModel |
                        score = addPoint Player2 model.score
                        ,gameState = Paused
                }, randomizeBallConstantVX)

            Nothing ->
                -- no goal scoresd
                (newModel, Cmd.none)


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
    in
        ({ model |
            nextballDirection = flipLR model.nextballDirection
            , ball = Ball centerPt (Velocity2d vx (toFloat vy))}
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





