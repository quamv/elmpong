
module Model exposing (..)


import Time exposing (..)
import Random exposing (..)
import Keyboard exposing (..)
import Set exposing (Set)
import Dict exposing (..)
import Helpers exposing (..)
import Line exposing (Point2d)


-----------
-- MODEL --
-----------


type VerticalDir = Up | Down
type LeftOrRight = Left | Right
type TopOrBottom = Top | Bottom
type WallSide = TopWall | BottomWall
type Score = Score Int Int
type GameState = Paused | Playing
type PlayerType = CPU | Human
type PlayerSide = Player1 | Player2

type alias Player = {
    playertype: PlayerType
    , side: PlayerSide
    }

type alias Velocity2d = {
    vx: Float
    , vy: Float
    }

type alias Paddle = {
    y: Float
    , side: LeftOrRight
    }

type alias Ball = {
    pos: Point2d
    , velocity: Velocity2d
    }

type alias StepBall = {
    p1: Point2d
    , p2: Point2d
    , velocity: Velocity2d
    }

type Msg =
    RequestNewBall
    | TogglePaused
    | NewBallConstantVX Int
    | Frame Time
    | KeyDownMsg Keyboard.KeyCode
    | KeyUpMsg Keyboard.KeyCode
    | SetReflectionMode ReflectionMode
    | ToggleShowSettings
    | AITick Time

type LRDirection =
    L2R
    | R2L


type ReflectionMode =
    Simple -- -1 * vx
    | PaddleCenterRelative -- ignore incoming trajectory. only use y coord of collision pt


type alias Model = {
    yesno: Bool
    , nextballDirection : LRDirection
    , gameState: GameState
    , score: Score
    , ball: Ball
    , lpaddle: Paddle
    , rpaddle: Paddle
    , keys: Set Char
    , reflectionMode: ReflectionMode
    , showSettings: Bool
    , player1type: PlayerType
    , player2type: PlayerType

--    , pauseAfterGoal: Boolean
    }

type alias CollisionDetectionSpecs = {
    lpaddle: Paddle
    , rpaddle: Paddle
    , reflectionMode: ReflectionMode
    }



-- Dimensions of the playing field
fieldWidth = 1000
fieldHeight = 1000

-- Half the field dimensions
-- (width, height) :: (Float, Float)
halfField =
    (fieldWidth / 2, fieldHeight / 2)


-- center pt of the playing surface
--centerPt : (Float, Float)
centerPt = {
    x=fst halfField,
    y=snd halfField
    }

-- TODO: check if this delta variable can/should be used
--delta = 50

-- ball speed in x direction
ballVX = 20
ballRadius = 10

-- Dimensions of the paddle
paddleWidth = 28
paddleHeight = 150
paddleHalfHeight = paddleHeight / 2
paddleHalfWidth = paddleWidth / 2
-- distance between paddle and playing field edge
paddleDist = 10

-- paddle x-coordinates
lpX = 0 + paddleDist + paddleWidth
rpX = fieldWidth - paddleDist - paddleWidth


defaultSet =
    Set.empty

defaultBall =
    Ball (Point2d 700 (centerPt.y - paddleHalfHeight)) (Velocity2d ballVX -10) -- above right paddle
    --   Ball (Point2d 900 (centerPt.y - paddleHalfHeight)) (Velocity2d ballVX 0) -- hit top of right paddle
    --    Ball (Point2d 900 210) (Velocity2d ballVX 8) -- hit top of right paddle

