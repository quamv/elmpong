{-
ModelHelpers

helper functions with a dependency on Model
-}
module ModelHelpers exposing (..)

import Line exposing (..)
import Model exposing (..)
import Helpers exposing (..)
import Set exposing (..)


{-
Takes 2 floats representing x-coordinates of a displacement
Returns the direction (L2R/R2L) of movement between x1 and x2
-}
segDirByXVals : Float -> Float -> LRDirection
segDirByXVals x1 x2 = if x1 < x2 then L2R else R2L

{-
Wrapper for segDirByXVals with Segment objects
Takes a Segment
Returns the horizontal direction (L2R/R2L) of the segment
Note that this does not handle vertical lines...
-}
segDir : Segment -> LRDirection
segDir seg = segDirByXVals seg.p1.x seg.p2.x



{-
Takes a VerticalDir
Returns a corresponding Int (1/-1 currently) for calculations
(ie maps an abstract VerticalDir to an Int for calculations)
-}
vdirToInt : VerticalDir -> Int
vdirToInt vdir =
    case vdir of
        Up -> 1
        Down -> -1


{-
Takes a time delta and a Ball
Returns a StepBall (a vector defined by two points)
from the current position to the next position using the time
difference and the velocity of the Ball.
-}
stepBall : Float -> Ball -> StepBall
stepBall delta_t ball =
    let
        pos = ball.pos
        v = ball.velocity
        dx = delta_t*v.vx
        dy = delta_t*v.vy
        nextPt = (Point2d (pos.x+dx) (pos.y+dy))
    in
        StepBall ball.pos nextPt ball.velocity


{-
Takes a StepBall
Returns a Ball at stepball.p2 with velocity stepball.velocity
ie. the translated ball at the new position
-}
ballFromStepBallPt2 : StepBall -> Ball
ballFromStepBallPt2 stepball =
    Ball stepball.p2 stepball.velocity


{-
Takes a StepBall
Returns the segment contained therein
-}
segFromStepBall : StepBall -> Segment
segFromStepBall stepball =
    Segment stepball.p1 stepball.p2

{-
Construct Ball from Tuple
-}
getBall (x, y) (vx, vy) =
    Ball (Point2d x y) (Velocity2d vx vy)


-- flip Left/Right
flipLR : LRDirection -> LRDirection
flipLR direction = case direction of
    L2R -> R2L
    R2L -> L2R

lineFromBall : Ball -> Line
lineFromBall ball =
    lineFromSegment
        <| segFromStepBall
        <| stepBall 1 ball
