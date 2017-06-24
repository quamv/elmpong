{-
AI Player logic

When the ball is heading towards the AI player, it will seek
the intersection point between the ball and the paddle's vertical
intersection line (this will change if the ball hits a side wall)

When the ball is heading away, the AI player will seek the vertical center

The AI player expresses its desire by 'pressing' the same keys as
a human user would. The actual movement is handled in the animation
frame generation process (update's Frame handler)
-}

module AILogic exposing (
    updateAIPlayers
    )

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
import Helpers exposing (..)
import Line exposing (..)
import ModelHelpers exposing (..)



{-
Drive a round of AI play for all cpu players
this means to return a set of AI pressed keys based on the
cpu players and the current game state
-}
updateAIPlayers : Model -> Set Char
updateAIPlayers model =
    let
        keys2 = case model.player1type of
            Human ->
                model.keys

            CPU ->
                updateAIPlayer model.lpaddle model.keys (getAITargetLeft model)
    in
        case model.player2type of
            Human ->
                keys2

            CPU ->
                updateAIPlayer model.rpaddle keys2 (getAITargetRight model)


{-
Execute a round of AI play for a single player
Takes a Paddle, the set of currently pressed keys, and the
target y-coordinate to seek
-}
updateAIPlayer : Paddle -> Set Char -> Float -> Set Char
updateAIPlayer paddle keys targety =
    let
        -- determine the keys for this player
        (upkey,downkey) = case paddle.side of
                Left -> (playerCmdKeys.p1Up, playerCmdKeys.p1Down)
                Right -> (playerCmdKeys.p2Up, playerCmdKeys.p2Down)

        -- clear any previous key presses for this player
        resetKeys =
            Set.filter (\k -> k /= upkey && k /= downkey) keys

        -- determine the key to press based on relative ball location
        newKeys =
            if targety > paddle.y + paddleHalfHeight then
                addKey downkey resetKeys
            else if targety < paddle.y - paddleHalfHeight then
                addKey upkey resetKeys
            else
                resetKeys
    in
        newKeys


{-
Determine the current target y-coordinate for the left paddle (Player1)
If the ball is heading in the other direction seek the vertical center
-}
getAITargetLeft : Model -> Float
getAITargetLeft model =
    if model.ball.velocity.vx > 0 then
        -- ball is heading in the other direction seek the vertical center
        fieldHeight / 2
    else
        -- ball heading in our direction. seek collision pt with
        -- the line at the left paddle's x-coordinate
        -- getAITarget model lpX
        fst model.intercepts


{-
Determine the current target y-coordinate for the right paddle (Player2)
If the ball is heading in the other direction seek the vertical center
-}
getAITargetRight : Model -> Float
getAITargetRight model =
    if model.ball.velocity.vx < 0 then
        -- ball is heading in the other direction seek the vertical center
        fieldHeight / 2
    else
        -- ball heading in our direction. seek collision pt with
        -- the line at the right paddle's x-coordinate
        -- getAITarget model rpX
        snd model.intercepts



