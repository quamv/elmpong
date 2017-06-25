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
        case intersectPaddleFront expectedHCut (BallSeg ballSeg) paddleFrontSeg of

            Just collisionPt ->
                -- collision found, reflect
                Just <| flexPaddleReflect collisionPt stepball paddle reflectionMode

            Nothing ->
                -- no intersection found with 'front' of paddle.
                -- see if we need to check the paddle sides
                if stepball.velocity.vy == 0 then
                    -- paddle side (Top/Bottom) collisions only possible for non-horizontal movement
                    Nothing
                else
                    collidePaddleSide paddle ballSeg stepball.velocity



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

TODO: fix this. it rarely works. responding to a side collision as a result of paddle
TODO: movement could be hairy (ie. ball already in spot, paddle moves up and hits)
TODO: because collision tests have already taken place. prob. need to recheck again.
-}
collidePaddleSide : Paddle -> Segment -> Velocity2d -> Maybe StepBall
collidePaddleSide paddle ballSeg velocity =
    -- paddle side (Top/Bottom) collisions only possible for non-horizontal movement
    -- this is a private function and we know the only caller context,
    -- the caller checks velocity.vy /= 0 before calling us so we will not here
    let
        -- determine the possible side collision based on velocity y-component
        (paddleSideSeg, expectedVCut) =
            if velocity.vy < 0 then
                -- vy < 0 means upward movement so bottom side is viable
                (getPaddleSideSeg paddle Bottom, CVBottom2Top)
            else
                -- vy > 0 means downward movement so top side is viable
                (getPaddleSideSeg paddle Top, CVTop2Bottom)

        -- get the segment for intersection test
        ballSegHack = BallSeg <| ballSeg
    in
        case intersectPaddleSide expectedVCut ballSegHack paddleSideSeg of

            Just collisionPt ->
                -- there was a collision between the ball and paddle side
                -- reflect vertically based on the original destination point
                Just <| reflectVer collisionPt ballSeg.p2 velocity

            Nothing ->
                -- no intersection found. no collision
                Nothing



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
intersectPaddleFront : CutHor -> BallSeg -> Segment -> Maybe Point2d
intersectPaddleFront cutHor (BallSeg ballSeg) paddleSeg =
    intersectSSByCutHor cutHor ballSeg paddleSeg

intersectPaddleSide : CutVer -> BallSeg -> Segment -> Maybe Point2d
intersectPaddleSide cutVer (BallSeg ballSeg) paddleSeg =
    intersectSSByCutVer cutVer ballSeg paddleSeg



{-
Determines the viable field wall to collide with and checks for collision
If collision detected, returns a reflected stepball
Otherwise, returns Nothing
TODO: occasionally balls near corners fly off the screen.probably after rapid paddle/wall collisions
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



