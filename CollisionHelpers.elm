{-
CollisionHelpers

test for collisions between ball and field walls and ball and paddles
-}
module CollisionHelpers exposing (
    doCollisions
    )

import Model exposing (..)
import Helpers exposing (..)
import Line exposing (..)
import ModelHelpers exposing (..)
import PaddleHelpers exposing (..)
import Reflectors exposing (..)

----------------
-- CONTROLLER --
----------------


{-
hack used to enforce parameter order to intersectSS
intersectSS takes two segments and expects the first segment
to be the ball segment so we use this type and a wrapper function
forcing the user to be clear about which he is passing as the ball
-}
type BallSeg = BallSeg Segment

{-
}
Takes a Model and a Stepball
Calls a set of collision functions between ball, paddles and wall
Returns the next Ball object after collision detection/response
-}
doCollisions : Model -> StepBall -> CollisionResults
doCollisions model stepball =
    let
        collisionDetails = CollisionDetectionSpecs
            model.lpaddle
            model.rpaddle
            model.reflectionMode
    in
        collide False stepball [
            collidePaddles collisionDetails,
            collideFieldWalls,
            collidePaddles collisionDetails
            ]



{-
Checks a directed segment (aka stepball/vector in xy) against a list of collision functions.
It calls each function until the vector doesn't collide with that
function anymore. The assumption is that each function that detects a
collision reflects the vector appropriately (eventually) off the collision point.

Returns a Ball with the new coordinates and velocity (if no collisions
were detected, this will simply be the stepball parameter's 2nd pt and velocity
ie. an object in motion..)
-}
collide : Bool -> StepBall -> List (StepBall -> Maybe StepBall) -> CollisionResults
collide collided stepball collisionFunctionList =
    case collisionFunctionList of

        []   ->
            -- no collision functions left to test. just use the second point of the
            -- stepball as the new x,y position. keep the velocity.
            CollisionResults
                (Ball stepball.p2 stepball.velocity)
                collided

        collisionFunction::collisionFunctionListTail ->
            -- found 1 or more collision functions to test.
            -- try the first one
            case collisionFunction stepball of

                Just reflectedStepball ->
                    -- collided, retry with the reflected ball (which should no longer collide, I guess?
                    -- it seems like this is a way to make an elm program fail at runtime. check. it is.)
                    collide True reflectedStepball collisionFunctionList

                Nothing  ->
                    -- no collision, recurse with the rest of the functions in the list
                    collide collided stepball collisionFunctionListTail


{-
Takes an x-coordinate of a Ball
Returns a Bool
True -> collisions are possible
False -> no collision possible
-}
paddleCollisionPossible : Float -> Bool
paddleCollisionPossible x =
    (x - ballRadius <= lpX) ||
    (x + ballRadius >= rpX)

{-
Checks if a paddle collision is viable
If so, calls collidePaddle to test
Otherwise returns Nothing
-}
collidePaddles : CollisionDetectionSpecs -> StepBall -> Maybe StepBall
collidePaddles specs stepball =
    if stepball.p2.x <= lpX + ballRadius then
        collidePaddle specs.reflectionMode stepball specs.lpaddle CHRight2Left
    else if stepball.p2.x >= rpX - ballRadius then
        collidePaddle specs.reflectionMode stepball specs.rpaddle CHLeft2Right
    else
        Nothing


{-
Checks if the vector (defined by points 1 and 2) crosses the paddle
 and if so returns a vector from the collision point (cp)
 to the reflection point (rp).

(2) *      * (rp)                             (rp) *     * (2)
     \    /                                         \   /
      || /                                           \ ||
      ||* (cp)                                   (cp) *||
      || \                                           / ||
      ||  * (1)                                 (1) *  ||
      ||                                               ||

-}
collidePaddle : ReflectionMode -> StepBall -> Paddle -> CutHor -> Maybe StepBall
collidePaddle reflectionMode stepball paddle expectedHCut =
    let
        ballSeg =
            segFromStepBall stepball

        paddleFrontSeg =
            getPaddleFrontSeg paddle
    in
        -- check for segment intersection (collision) between paddleSeg and ballSeg
        -- NOTE: added BallSeg parameter because the ball segment MUST be the first
        -- segment parameter in order to get the cut directions right
        case collideBallPaddleFront expectedHCut (BallSeg ballSeg) paddleFrontSeg of

            Just collisionPt ->
                -- collision found, reflect
                Just <| flexPaddleReflect collisionPt stepball paddle reflectionMode

            Nothing ->
                -- no intersection found with 'front' of paddle.
                -- see if we need to check the paddle sides
                if stepball.p2.y /= stepball.p1.y then
                    -- check possible collision with paddle sides
                    collidePaddleSide paddle ballSeg stepball.velocity
                else
                    -- horizontal direction can not intersect Top/Bottom of paddle
                    Nothing



{-

Hitting the side instead of the front of the paddle should bounce
 the ball off to the side. Ineffective for gameplay, but a lot better
 looking than having the ball go through the side of the paddle.

 (rp) *   * (1)
       \ /
        * (cp)
       /||
  (2) * ||
        |+ (px,py)
        ||
        ||

-}
collidePaddleSide : Paddle -> Segment -> Velocity2d -> Maybe StepBall
collidePaddleSide paddle ballSeg velocity =
    -- paddle side (Top/Bottom) collisions only possible for non-horizontal movement
    -- we are using the stepball displacement vector to determine the movement rather than
    -- the velocity vector at the moment. velocity seems reasonable but the displacement
    -- vector (specifically (y1 - y2)) seems a sure thing.
    -- determine if a side collision is possible and if so, get the segment to check
    case getViablePaddleSideDetails ballSeg paddle of

        Just (paddleSideSeg, expectedVCut) ->
            -- a collision is possible. check.
            let
                ballSegHack = BallSeg <| ballSeg
            in
            case collideBallPaddleSide expectedVCut ballSegHack paddleSideSeg of

                Just collisionPt ->
                    -- there was a collision between the ball and paddle side
                    -- reflect vertically based on the original destination point
                    Just <| reflectVer collisionPt ballSeg.p2 velocity


                Nothing ->
                    -- no intersection found. no collision
                    Nothing

        Nothing ->
            -- presumably the ball is traveling on a horizontal line
            -- which cannot intersect the sides (Top/Bottom) of the paddles
            Nothing


{-
Determines the viable field wall to collide with and checks for collision
If collision detected, returns a reflected stepball
Otherwise, returns Nothing
-}
collideFieldWalls : StepBall -> Maybe StepBall
collideFieldWalls stepball =
    if stepball.p2.y <= ballRadius then
        collideFieldWall TopWall stepball
    else if stepball.p2.y >= fieldHeight - ballRadius then
        collideFieldWall BottomWall stepball
    else
        Nothing


{-
Checks if the vector (defined by points 1 and 2) crosses
 a field wall and if so returns a vector
 from the collision point (cp) to the reflection point (rp).

      (2) *
         /
 ___________
  (cp) *
      / \
 (1) *   \
     (rp) *

-}
collideFieldWall : WallSide -> StepBall -> Maybe StepBall
collideFieldWall wall stepball =
    let
        -- determine y-coordinate of the wall and expected cut type
        (yCoord, expectedCut) = case wall of
                TopWall -> (ballRadius, CVBottom2Top)
                BottomWall -> (fieldHeight - ballRadius, CVTop2Bottom)

        -- a wall is a horizontal line
        wallLine = horLine yCoord

        -- intersection logic works on line segments
        -- get displacement segment from the stepball for intersection test with wall
        ballSeg = segFromStepBall stepball

    in
        -- check for a collision between wall and stepball displacement vector
        -- only look for 'expectedCut' types (e.g. Bottom2Top for the top wall)
        case intersectLSByCutVer expectedCut wallLine ballSeg of

            Just cp ->
                -- collision, reflect off collision point
                Just <| reflectVer cp stepball.p2 stepball.velocity

            Nothing ->
                -- no intersection found. no collision
                Nothing


{-
If a side collision is viable, returns the segment and expected cut
Otherwise, returns Nothing
-}
getViablePaddleSideDetails : Segment -> Paddle -> Maybe (Segment, CutVer)
getViablePaddleSideDetails seg paddle =
    case getViablePaddleSideCollision (seg.p2.y - seg.p1.y) of

        Just (side2Check, expectedVCut) ->
            -- there is a viable side collision. get the relevant segment
            let
                paddleSideSeg = getPaddleSideSeg paddle side2Check
            in
                Just (paddleSideSeg, expectedVCut)

        Nothing -> Nothing


{-
Paddle side collisions can only occur if there is a vertical velocity component
If vy > 0, we can only possibly hit the top side of a paddle
If vy < 0, we can only possibly hit the bottom side of a paddle
If vy !=0 Return a tuple with the viable side (Top/Bottom) and expected cut
direction (CVTop2Bottom,CVBottom2Top)
If vy == 0 return Nothing
-}
getViablePaddleSideCollision : Float -> Maybe (TopOrBottom, CutVer)
getViablePaddleSideCollision dy =
    if dy > 0 then
        -- dy > 0 means moving downwards, could hit top side
        Just <| (Top, CVTop2Bottom)
    else if dy < 0 then
        -- dy < 0 means moving upwards, could hit bottom side
        Just <| (Bottom, CVBottom2Top)
    else -- presumably horizontal line
        Nothing



--{-
--Field wall collisions can only occur if there is a vertical velocity component
--If vy > 0, we can only possibly hit the bottom wall
--If vy < 0, we can only possibly hit the top wall
--So if vy /=0 Return a tuple with the viable side (Top/Bottom) and expected cut
--direction (CVTop2Bottom,CVBottom2Top)
--If vy == 0 return Nothing (travel along horizontal line)
---}
--getViableFieldWallCollision : Float -> Maybe (WallSide, CutVer)
--getViableFieldWallCollision vy =
--    if vy > 0 then
--        Just <| (BottomWall, CVTop2Bottom)
--    else if vy < 0 then
--        Just <| (TopWall, CVBottom2Top)
--    else -- presumably horizontal line
--        Nothing
--
--


{-
TODO: review this problem regarding ballseg/paddleseg parameter order problem
The function "(Line.)intersectSS : Segment -> Segment..." takes two segment parameters
The parameters  MUST BE IN THE CORRECT ORDER with the "ballseg" coming first.
The ballsegment MUST be the first parameter because the routines that determine
Cut directions (e.g. CHLeft2Right, CVNeutral) assume the first parameter's POV
ie. the assumption is that the first segment is a displacement vector while the
second segment is stationary object.
intersectBPByCutHor (below) is a wrapper function to (essentially) ensure that
the order is correct by forcing the caller to specify the "BallSeg"
Could change to "MovingSeg" and "StationarySeg" perhaps.
Or "DisplacementSeg" and "ExtentSeg"
-}
collideBallPaddleFront : CutHor -> BallSeg -> Segment -> Maybe Point2d
collideBallPaddleFront cutHor (BallSeg ballSeg) paddleSeg =
    intersectSSByCutHor cutHor ballSeg paddleSeg

collideBallPaddleSide : CutVer -> BallSeg -> Segment -> Maybe Point2d
collideBallPaddleSide cutVer (BallSeg ballSeg) paddleSeg =
    intersectSSByCutVer cutVer ballSeg paddleSeg




