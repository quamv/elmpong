{-
PaddleHelpers

Paddle-related helper functions.

Data for a paddle on the field
Paddle y-pos (on the surface of the paddle in the center)

  ||
  ||
  |* (y-pos)
  ||
  ||

-}

module PaddleHelpers exposing (
    getPaddleFrontSeg
    , getPaddleSideSeg
    , stepPaddles
    )

import Line exposing (..)
import Model exposing (..)
import Helpers exposing (..)
import ModelHelpers exposing (..)
import Set exposing (..)


{-
kludgy paddle helpers.
well, given that they are no longer in the public namespace and are
module-local, it's no longer terrible.
py is at the center of the paddle, so the intersection points for collision
detection are +/- (paddleHalfHeight + ballRadius) (ie. +/= paddleSegHalfLength)
-}
paddleSegHalfLength = paddleHalfHeight + ballRadius
paddleSegTopY py = py - paddleSegHalfLength
paddleSegBotY py = py + paddleSegHalfLength
paddleIntersectTopBot py = (paddleSegTopY py, paddleSegBotY py)
lpSegX1 = lpX - paddleWidth - ballRadius
lpSegX2 = lpX + ballRadius
rpSegX1 = rpX - ballRadius
rpSegX2 = rpX + paddleWidth + ballRadius


{-
Returns the vertical "front" paddle segment for collision detection
-}
getPaddleFrontSeg : Paddle -> Segment
getPaddleFrontSeg paddle =
    let
        -- y is at the vertical center of the paddle, leaving half the
        -- paddle height above and below the point.
        -- also the intersection segment is extended to include the ball
        -- radius on both top and bottom, allowing for the fact that the
        -- ball's x,y is at its center point
        (yTop, yBot) = paddleIntersectTopBot paddle.y

        x = case paddle.side of
            Left -> lpSegX2
            Right -> rpSegX1
    in
        verLineSegment yTop yBot x


{-
Takes a paddle and a side (Top or Bottom)
Returns the segment representing a given side (Top/Bottom) of a paddle
for collision detection with sides
-}
getPaddleSideSeg : Paddle -> TopOrBottom -> Segment
getPaddleSideSeg paddle toporbottom =
    case paddle.side of
        Left -> case toporbottom of
                Top -> horLineSegment lpSegX1 lpSegX2 (paddleSegTopY paddle.y)
                Bottom -> horLineSegment lpSegX1 lpSegX2 (paddleSegBotY paddle.y)

        Right -> case toporbottom of
                Top -> horLineSegment rpSegX1 rpSegX2 (paddleSegTopY paddle.y)
                Bottom -> horLineSegment rpSegX1 rpSegX2 (paddleSegBotY paddle.y)


{-
Takes a Paddle
Returns a Tuple of Chars representing command keys for
that paddle (eg ('J', 'I') for the right paddle)
This tells the paddle update routines how to interpret current user input
The mapping is centralized in playerCmdKeys
-}
getPaddleKeys : Paddle -> (Char,Char)
getPaddleKeys paddle =
    case paddle.side of
        Left ->
            (playerCmdKeys.p1Up, playerCmdKeys.p1Down)

        Right ->
            (playerCmdKeys.p2Up, playerCmdKeys.p2Down)



{-
Takes a Model and a Float (time diff)
Updates the paddle coordinates based on user input
Returns a tuple of updated Paddles
-}
stepPaddles : Model -> Float -> (Paddle, Paddle)
stepPaddles model diff =
    let
        -- paddle movement timing adjustment factor
        paddletimediff =
            getDiffForPaddleCalcs diff

        -- update the paddle coords
        nextlpaddle = getNextPaddle
            model.lpaddle
            paddletimediff
            model.keys

        nextrpaddle = getNextPaddle
            model.rpaddle
            paddletimediff
            model.keys
    in
        (nextlpaddle, nextrpaddle)

{-
Takes a paddle and a set of keys being pressed
Generates the new paddle coords based on user input
Returns an updated paddle
-}
getNextPaddle : Paddle -> Float -> Set Char -> Paddle
getNextPaddle paddle diff keys =
    let
        -- get the key mapping for this paddle
        (upkey,dnkey) =
            getPaddleKeys paddle
    in
        if isKeyPressed upkey keys then
            stepPaddle diff Up paddle
        else if isKeyPressed dnkey keys then
            stepPaddle diff Down paddle
        else
            paddle


{-
Takes a time delta, a VerticalDir and a Paddle and
Returns a Paddle at a new xy
-}
stepPaddle : Float -> VerticalDir -> Paddle -> Paddle
stepPaddle delta_t vdir paddle =
    stepPaddle2 delta_t (vdirToInt vdir) paddle


{-
Takes a time delta, an integer direction and a Paddle and
Returns a Paddle in that direction.
-}
stepPaddle2 : Float -> Int -> Paddle -> Paddle
stepPaddle2 delta_t dir paddle =
    let
        newY = paddle.y - (toFloat dir) * delta_t * 100
        clampedY = (clamp paddleLowerBound paddleUpperBound newY)
    in
        Paddle clampedY paddle.side


{-
Takes a timediff appropriate for ball movement
Returns a timediff appropriate for paddle movement
Total guesswork here but looks fine
-}
getDiffForPaddleCalcs : Float -> Float
getDiffForPaddleCalcs diff =
    diff / 5

