
module Line exposing (..)

import Helpers exposing (isJust)


type DirectionCoefficient =
    -- Standard case, coefficient between -Inf and Inf exclusive
    DC Float
    -- (coefficient infinite->) Vertical + x of a point it goes through
    | DCVer Float


--  Line DC + y-intercept
type alias Line = {
    dc : DirectionCoefficient,
    yintercept : Float
    }

type alias Point2d = {
    x: Float,
    y: Float
    }

type alias Segment = {
    p1 : Point2d,
    p2 : Point2d
    }

-- direction of cut
type CutHor = CHLeft2Right | CHRight2Left | CHNeutral
type CutVer = CVBottom2Top | CVTop2Bottom | CVNeutral

type alias Cut = {
    ch : CutHor,
    cv : CutVer
    }

type alias CutPt = {
    cut: Cut,
    pt: Point2d
    }


{-

------------------------------------
-- Intersection testers           --
------------------------------------

L - Line
S - Segment
P - Point

e.g.
'intersectLS' intersects a Line and a Segment
-}


{-
Find intersection between two lines
If lines intersect, returns a Point2d
Otherwise returns Nothing
-}
intersectLL : Line -> Line -> Maybe Point2d
intersectLL line1 line2 =
    case (line1.dc, line2.dc) of
        -- vertical lines can't intersect
        (DCVer _, DCVer _) ->
            Nothing

        -- non-vertical with vertical
        (DC c1, DCVer x) ->
            Just <| Point2d x (c1 * x + line1.yintercept)

        -- vertical with non-vertical
        (DCVer x, DC c2) ->
            Just <| Point2d x (c2 * x + line2.yintercept)

        -- two non-vertical lines
        (DC c1, DC c2) ->
            if c1 == c2 then
                -- the slopes are equal, so they can't intersect. return Nothing
                Nothing
            else
                let
                    -- calculate x,y of intersection
                    ipX = (line1.yintercept-line2.yintercept)/(c2-c1)
                    ipY = c1 * ipX + line1.yintercept
                in
                    Just <| Point2d ipX ipY




{-
Find intersection between line and segment
If line1 and segment intersect, returns a CutPt
Otherwise returns Nothing
-}
intersectLS : Line -> Segment -> Maybe CutPt
intersectLS line segment =
    -- first convert the segment to its containing line
    -- and check for an intersection between that line and line1
    case intersectLL line (lineFromSegment segment) of

        Just intersectionPt ->
            -- the lines do intersect. check segment
            intersectPS intersectionPt segment

        Nothing ->
            -- no intersection between lines, so no possible intersection
            -- between the line and the original segment
            Nothing




{-
Find intersection between 2 segments
If segments intersect, returns a CutPt
The Cut directions are determined from the POV of movingSeg
If segments do not intersect, returns Nothing
-}
intersectSS : Segment -> Segment -> Maybe CutPt
intersectSS movingSeg stationarySeg =
    -- first convert segment1 to a line and check for an intersection
    -- between that line and segment2
    case intersectLS (lineFromSegment movingSeg) stationarySeg of

        Just cutPt ->
            -- the line intersected stationarySeg, check if the intersection
            -- point lies within the original segment (segment1)
            -- if so, return a CutPt
            -- otherwise, return Nothing
            intersectPS cutPt.pt movingSeg


        Nothing ->
            -- no intersection between the line and segment
            -- there can be no intersection between the segments
            Nothing



{-
Find intersection between point and segment
If pt lies on the directed segment, returns a CutPt
If pt does not lie on segment, returns Nothing
-}
intersectPS : Point2d -> Segment -> Maybe CutPt
intersectPS pt segment =
    -- getCutType returns a (Maybe CutHor, Maybe CutVer) based on the
    -- nature of the intersection point and directed segment (vector)
    case getCutType pt segment of

        (Just ch2, Just cv2) ->
            -- the intersection was inside the segment's range, so this
            -- is a collision between the line and segment.
            -- return intersection details as CutPt
            Just <| CutPt (Cut ch2 cv2) pt

        _  ->
            -- the intersection was outside of the segment's range, so
            -- there is no intersection between the line and segment
            Nothing




----------------------------------------------
-- Intersection testers by Cut Type         --
----------------------------------------------

{-
Use the primary intersection testers above and filter results
based on a specific expected cut type.
If the wrong cut type is found, return Nothing
If not intersection found, return Nothing
-}

{-
Performs a segment/segment intersection test and
compares the CutHor type of any collisions found to an expected cut type. if
the cut types match, the collision is returned. else Nothing.
-}
intersectSSByCutHor : CutHor -> Segment -> Segment -> Maybe Point2d
intersectSSByCutHor expectedCut movingSeg stationarySeg =
    -- check for segment intersection
    case intersectSS movingSeg stationarySeg of

        Just cutPt ->
            -- segment/segment intersection found. verify it's the expected cut type
            if cutPt.cut.ch == expectedCut then
                -- it is, return the intersection point
                Just cutPt.pt
            else
                Nothing


        Nothing ->
            -- no intersection between segments
            Nothing




{-
Performs a line/segment intersection test and compares the CutVer type
of any intersection found to an expected cut type.
if the cut types match, the intersection pt is returned. else Nothing.
-}
intersectLSByCutVer : CutVer -> Line -> Segment -> Maybe Point2d
intersectLSByCutVer expectedCut line segment =
    -- look for collision between line and segment (vector)
    case intersectLS line segment of

        Just cutPt ->
            -- line/segment intersection found,
            -- verify it's the expected cut type (Top2Bottom/Bottom2Top)
            if cutPt.cut.cv == expectedCut then
                -- it is, return the intersection point
                Just cutPt.pt
            else
                Nothing


        Nothing ->
            -- no intersection between line and segment
            Nothing


{-
Performs a segment/segment intersection test and compares the cut type of any
intersection found to an expected cut type. if the cut types match, the
intersectionPt is returned. else Nothing.
-}
intersectSSByCutVer : CutVer -> Segment -> Segment -> Maybe Point2d
intersectSSByCutVer expectedCut movingSeg stationarySeg =
    -- look for collision between segments
    case intersectSS movingSeg stationarySeg of

        Just cutPt ->
            -- segment/segment intersection found, verify it's the expected cut type
            if cutPt.cut.cv == expectedCut then
                -- it is, return the intersection point
                Just cutPt.pt
            else
                Nothing


        Nothing ->
            -- no intersection between segments
            Nothing



------------------------------------
-- Cut type/direction interpretors --
------------------------------------

{-
Determines the type of intersection cut based on the intersection point and
a directed segment. Returns a tuple of maybes indicating cut directions.
-}
getCutType : Point2d -> Segment -> (Maybe CutHor, Maybe CutVer)
getCutType {x, y} {p1, p2} =
    let
        cutHor = getCutHor x p1.x p2.x
        cutVer = getCutVer y p1.y p2.y
    in
        (cutHor, cutVer)



{-
If value px is within the range x1-x2, Returns a CutHor
indicating the directional nature of the horiz. intersection
(e.g. CHLeft2Right, CHRight2Left)
Otherwise returns Nothing

if x1 is actually less than x2, then by reversing the order in the
comparison (ie. reversing the assumed direction of segment x1->x2
to x2->x1) we detect intersections in both directions
-}
getCutHor : Float -> Float -> Float -> Maybe CutHor
getCutHor px x1 x2 =
    if x1 == x2 && x2 == px then
        Just CHNeutral
    else if betweenIncl px (x1,x2) then   -- ie. x1..ipx..x2
        Just CHLeft2Right
    else if betweenIncl px (x2,x1) then   -- ie. x2..ipx..x1
        Just CHRight2Left
    else
        Nothing

{-
If py is within range y1-y2, returns a CutVer
indicating the directional nature of the vertical intersection
(e.g. CVTop2Bottom, CVBottom2Top)
Otherwise returns Nothing

if y1 is actually less than y2, then by reversing the order in the
comparison (ie. reversing the assumed direction of segment y1->y2
to y2->y1) we detect intersections in both directions
-}
getCutVer : Float -> Float -> Float -> Maybe CutVer
getCutVer py y1 y2 =
    if y1 == y2 && y2 == py then
        Just CVNeutral
    else if betweenIncl py (y1,y2) then -- ie. y1..ipy..y2
        Just CVTop2Bottom
    else if betweenIncl py (y2,y1) then -- ie. y2..ipy..y1
        Just CVBottom2Top
    else
        Nothing





------------------------------------
-- Intersection helpers --
------------------------------------

{-
Returns True if two lines intersect, else False
-}
intersectsLL : Line -> Line -> Bool
intersectsLL line1 line2 = isJust <| intersectLL line1 line2

{-
Returns True if subject is between left and right inclusive, else False
-}
betweenIncl : Float -> (Float, Float) -> Bool
betweenIncl subject (left,right) = left <= subject && subject <= right

{-
Returns True if subject is between left and right exclusive, else False
-}
betweenExcl : Float -> (Float, Float) -> Bool
betweenExcl subject (left,right) = left <  subject && subject <  right

{-
Returns True if subject is within the range of bound1 and bound2 inclusive
Otherwise False
-}
withinIncl : Float -> (Float, Float) -> Bool
withinIncl subject (bound1,bound2) = betweenIncl subject (bound1,bound2)
                                  || betweenIncl subject (bound2,bound1)

{-
Returns True if subject is within the range of bound1 and bound2 exclusive.
Otherwise False
-}
withinExcl : Float -> (Float, Float) -> Bool
withinExcl subject (bound1,bound2) = betweenExcl subject (bound1,bound2)
                                  || betweenExcl subject (bound2,bound1)


{-
Calls intersectLS with the arguments flipped
-}
intersectSL : Segment -> Line -> Maybe CutPt
intersectSL = flip intersectLS

{-
Returns True if a line and segment intersect, otherwise False
-}
intersectsLS : Line -> Segment -> Bool
intersectsLS line seg = isJust <| intersectLS line seg

{-
Calls intersectsLS with the arguments flipped
-}
intersectsSL : Segment -> Line -> Bool
intersectsSL = flip intersectsLS

{-
Returns True if two segments intersect, otherwise False
-}
intersectsSS : Segment -> Segment -> Bool
intersectsSS seg1 seg2 = isJust <| intersectSS seg1 seg2




------------------------------------
-- Line/Segment construction helpers --
------------------------------------

{-
Returns a horizontal line at the given y position
-}
horLine : Float -> Line
horLine y = Line (DC 0) y

{-
Returns a vertical line at the given x position
-}
verLine : Float -> Line
verLine x = Line (DCVer x) 0

{-
Returns a horizontal line segment
-}
horLineSegment : Float -> Float -> Float -> Segment
horLineSegment x1 x2 y = Segment (Point2d x1 y) (Point2d x2 y)

{-
Returns a vertical line segment
-}
verLineSegment : Float -> Float -> Float -> Segment
verLineSegment y1 y2 x =  Segment (Point2d x y1) (Point2d x y2)

{-
Create a Line from a Segment
-}
lineFromSegment : Segment -> Line
lineFromSegment {p1,p2} =
  let dx = p2.x-p1.x
      dy = p2.y-p1.y
  in
    -- dx == 0 implies a vertical line
    if dx == 0 then
        -- return a vertical line at x1
        Line (DCVer p1.x) 0
    else
        -- dx != 0 means non-vertical line
        let
            dc = dy/dx -- slope
            y  = p1.y - dc * p1.x -- y-intercept
        in
            -- return the calculated non-vertical line
            Line (DC dc) y




------------------------------------
-- Line/Segment orientation tests --
------------------------------------

{-
Returns True if line is a horizontal line. otherwise False
-}
isHorL : Line -> Bool
isHorL { dc } = case dc of
  DC dc -> dc == 0
  _     -> False

{-
Returns True if segment is horizontal. otherwise False
-}
isHorS : Segment -> Bool
isHorS seg = seg.p1.y == seg.p2.y

{-
Returns True if line is vertical. otherwise False
-}
isVerL : Line -> Bool
isVerL line = case line.dc of
  DCVer _ -> True
  _       -> False

{-
Returns True if segment is vertical. otherwise False
-}
isVerS : Segment -> Bool
isVerS seg = seg.p1.x == seg.p2.x



{-
Calculate the length of a segment
-}
segLengthByPts : Point2d -> Point2d -> Float
segLengthByPts pt1 pt2 =
    let
        asquared = ((pt2.x-pt1.x)^2)
        bsquared = ((pt2.y-pt1.y)^2)
    in
        sqrt (asquared + bsquared)

{-
wrapper to get Segment Magnitude with Segment param
Takes a Segment
Returns a Float indicating length of segment
-}
segLength : Segment -> Float
segLength {p1,p2} =
    segLengthByPts p1 p2


{-
Takes a vector (an angle and a magnitude)
Calculates x,y components
-}
vecToXY : Float -> Float -> (Float,Float)
vecToXY angle magnitude =
    let
        dx = magnitude * cos angle
        dy = magnitude * sin angle
    in
        (dx,dy)

