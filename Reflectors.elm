module Reflectors exposing (
    flexPaddleReflect
    ,reflectVer
    ,reflectHor
    )

import Line exposing (..)
import Model exposing (..)
import ModelHelpers exposing (..)


{-
-1 * vx with linear reflection
-}
simplePaddleReflector : Point2d -> StepBall -> Paddle -> StepBall
simplePaddleReflector collidePt stepball paddle =
    reflectHor collidePt stepball.p2 stepball.velocity


{-
Call reflection function based on reflectionMode
-}
flexPaddleReflect : Point2d -> StepBall -> Paddle -> ReflectionMode -> StepBall
flexPaddleReflect collidePt stepball paddle reflectionMode =
    case reflectionMode of
        Simple ->
            simplePaddleReflector collidePt stepball paddle

        PaddleCenterRelative ->
            reflectPaddleCenterRelative collidePt stepball paddle

{-
reflection angle is based only on collision pt's proximity to paddle center
above reflects increasingly upwards and vice versa. max angle off of
horizontal is pi/4
-}
reflectPaddleCenterRelative : Point2d -> StepBall -> Paddle ->  StepBall
reflectPaddleCenterRelative collidePoint stepball paddle =
    let
        distFromPaddleCtr =
            collidePoint.y - paddle.y

        pctOffCenter =
            distFromPaddleCtr / paddleHalfHeight

        reflectionAngle =
            if stepball.velocity.vx > 0 then
                -- heading L2R, reflect @ pi (+/- pi/4)
                -- if cp above paddley pctoff will be neg
                -- so -pctoff will be positive, so pi + pos
                -- should be in 3rd quadrant, but it's not
                -- perhaps this is because of graphics x,y coords (y increasing down)?
                -- TODO: understand reflection angle better
                -- could probably swap the order in the original subtraction
                pi + (-pctOffCenter * pi / 4)
            else
                -- heading R2L, reflect @ 0 (+/- pi/4)
                0 + pctOffCenter * pi / 4

        reflectedVelocity =
            redirectVelocity stepball.velocity reflectionAngle

        reflectedPt =
            reflectDisplacement collidePoint stepball.p2 reflectionAngle
    in
        StepBall collidePoint reflectedPt reflectedVelocity


{-
Takes a collision point, a target pt, and an angle
Returns a point representing the reflection off the collision point
of the original displacement at the specified angle
-}
reflectDisplacement : Point2d -> Point2d -> Float -> Point2d
reflectDisplacement collidePoint p2 angle =
    let
        displacementMagnitude =
            segLengthByPts collidePoint p2

        (dx, dy) =
            vecToXY angle displacementMagnitude
    in
        Point2d (collidePoint.x + dx) (collidePoint.y + dy)

{-
Reflects the y-component of a displacement and velocity vector off a collision point
Returns a StepBall from collision point to reflection point
-}
reflectVer :  Point2d -> Point2d -> Velocity2d -> StepBall
reflectVer collisionPt p2 v =
    let
        reflectedPt = Point2d p2.x (collisionPt.y - (p2.y-collisionPt.y))
        reflectedVelocity = Velocity2d v.vx (0-v.vy)
    in
        StepBall collisionPt reflectedPt reflectedVelocity


{-
Reflects the x-component of a displacement and velocity vector off a collision point
Returns a StepBall from collision point to reflected point
-}
reflectHor : Point2d -> Point2d -> Velocity2d -> StepBall
reflectHor collisionPt p2 v =
    let
        reflectedPt = Point2d (collisionPt.x - (p2.x-collisionPt.x)) p2.y
        reflectedVelocity = Velocity2d (0-v.vx) v.vy
    in
        StepBall collisionPt reflectedPt reflectedVelocity


{-
Takes a velocity and direction (an angle)
Returns a velocity in that direction with the original magnitude
-}
redirectVelocity : Velocity2d -> Float -> Velocity2d
redirectVelocity v angle =
    let
        magnitude = sqrt <| v.vx^2 + v.vy^2
        (dx, dy) = vecToXY angle magnitude
    in
        Velocity2d dx dy




