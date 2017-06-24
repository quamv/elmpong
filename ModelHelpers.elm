{-
ModelHelpers

helper functions with a dependency on Model
-}
module ModelHelpers exposing (..)

import Line exposing (..)
import Model exposing (..)
import Helpers exposing (..)
import Set exposing (..)
import Keyboard exposing (..)
import Char exposing (fromCode)

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


{-
flip Left/Right
-}
flipLR : LRDirection -> LRDirection
flipLR direction = case direction of
    L2R -> R2L
    R2L -> L2R


{-
Extrapolate the ball's position and velocity into a line
Takes a Ball
Steps the Ball to get a StepBall
Extracts a line Segment from the StepBall
Creates a Line from the Segment
Returns a Line
-}
lineFromBall : Ball -> Line
lineFromBall ball =
    lineFromSegment         -- create a line from
        <| segFromStepBall  -- a segment created from
        <| stepBall 1 ball  -- a stepball


{-
Determine the ball's intersection with both paddles
-}
getPaddleIntercepts : Ball -> (Float, Float)
getPaddleIntercepts ball =
    let
        leftIntersectionPt =
            getPaddleIntercept ball <| lpX + ballRadius

        rightIntersectionPt =
            getPaddleIntercept ball <| rpX - ballRadius
    in
        (leftIntersectionPt, rightIntersectionPt)

{-
Determine the target y-coordinate for a specific paddle (Player)
This involves intersecting the ball's travel path with the vertical
line at the specified x coordinate (targetPaddleX)
-}
getPaddleIntercept : Ball -> Float -> Float
getPaddleIntercept ball targetPaddleX =
    let
        -- extrapolate the ball's travel line
        ballLine =
            lineFromBall ball

        -- get a vertical line at x=targetPaddleX
        paddleLine =
            verLine targetPaddleX
    in
        -- intersect the ball's extrapolated travel line with the vertical line
        -- at the paddle's intersection x-coord (e.g. lpX + ballRadius)
        case intersectLL ballLine paddleLine of
            Just pt ->
                pt.y

            Nothing ->
                -- no intersection? shouldn't really happen
                -- default to mid-field
                snd halfField


{-
toggle player type between CPU and Human
-}
togglePlayerType : PlayerType -> PlayerType
togglePlayerType playerType =
    case playerType of
        Human -> CPU
        CPU -> Human


{-
Takes a PlayerSide
Returns a List of Chars representing the keys for that side
-}
getPlayerKeys : PlayerSide -> List Char
getPlayerKeys playerside =
    case playerside of
        Player1 -> [playerCmdKeys.p1Up, playerCmdKeys.p1Down]
        Player2 -> [playerCmdKeys.p2Up, playerCmdKeys.p2Down]

{-
Takes a KeyCode
If the key is a player cmd key, returns a Maybe PlayerSide
If the key is not a specific player's cmd key, returns Nothing
-}
getPlayerFromKey : Keyboard.KeyCode -> Maybe PlayerSide
getPlayerFromKey key1 =
    let key = fromCode key1 in
    if List.member key (getPlayerKeys Player1) then
        Just Player1
    else if List.member key (getPlayerKeys Player2) then
        Just Player2
    else
        Nothing


{-
Takes a KeyCode and the current player types (CPU/Human)
If the key is a cmd key for a CPU user, returns True
Otherwise returns False
(This routine is used to filter out keypresses for a CPU user.
The ability to process them is actually useful, as it allows for
a user to affect the operation of CPU players, so this restriction
may or may not be ultimately desired but it's cleaner for now)
-}
isCpuPlayerKey : Keyboard.KeyCode -> PlayerType -> PlayerType -> Bool
isCpuPlayerKey key player1type player2type =
    case getPlayerFromKey key of
        Just player ->
            case player of
                Player1 -> player1type == CPU
                Player2 -> player2type == CPU

        Nothing ->
            False



