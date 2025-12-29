module HAUS.Physics where
    
import HAUS.Types

-- distanceBetwee: Calculates the Euclidean distance between two positions in space. Takes two points as input and returns the linear distance between them.
distanceBetween :: Position -> Position -> Distance
distanceBetween p1 p2 = sqrt ((x1 - x2)**2 + (y1 - y2)**2)
    where 
        (x1, y1) = desCoords p1
        (x2, y2) = desCoords p2

-- angleToTarget: Determines the angle from an origin position to a target position. Useful for calculating the direction in which an object should aim or move.
-- The angle is given by the formula: arctan ((y2 - y1) / (x2 - x1)).
angleToTarget :: Position -> Position -> Radian
angleToTarget p1 p2 = atan2 (y2 - y1) (x2 - x1)
    where
        (x1, y1) = desCoords p1
        (x2, y2) = desCoords p2

-- deg2rad: Converts an angle expressed in degrees to its equivalent in radians.
deg2rad :: Degree -> Radian
deg2rad a = a * 2*pi / 360

-- rad2deg: Converts an angle expressed in radians to its equivalent in degrees.
rad2deg :: Radian -> Degree
rad2deg a = a * 360 / (2*pi)

-- sumVec: Helper function. Sums two vectors, returning a new vector representing their sum.
-- The vector sum formula is: (x1 + x2, y1 + y2).
sumVec :: Vector -> Vector -> Vector
sumVec v1 v2 = (+) <$> v1 <*> v2 

-- subVec: Subtracts two vectors, returning a new vector representing their difference.
-- The vector subtraction formula is: (x1 - x2, y1 - y2).
subVec :: Vector -> Vector -> Vector
subVec v1 v2 = (-) <$> v1 <*> v2

-- centroide: Function that calculates the mean of the coordinates (x,y) of a list of points.
centroide :: [Point] -> Point
centroide [] = pure 0.0  -- pure 0.0 = Coords (0.0, 0.0)
centroide ps = Coords 
        ( (sum [x | Coords (x, _) <- ps]) / len
        , (sum [y | Coords (_, y) <- ps]) / len
        )
    where len = fromIntegral (length ps)

-- getVertices: Generates a list of vertices (points) from four base points and a rotation angle.
{- The coordinates of the new point are calculated as follows:
        X coordinate: cx + cos(a + b) * sqrt((x-cx)^2 + (y-cy)^2)
        Y coordinate: cy + sin(a + b) * sqrt((x-cx)^2 + (y-cy)^2),
    where b is the angle between point p and the horizontal (parallel to X-axis) passing through the centroid. -}
getVertices :: (Point, Point, Point, Point, Radian) -> Point -> [Point]
getVertices (p1, p2, p3, p4, a) c = [rotarPunto c a p | p <- [p1, p2, p3, p4]]
    where
        rotarPunto (Coords (cx, cy)) a (Coords (x, y)) =
            let r = sqrt ((x-cx)**2 + (y-cy)**2)
                b = angleToTarget (Coords (cx,cy)) (Coords (x,y))
                newX = cx + cos (a + b) * r
                newY = cy + sin (a + b) * r
            in Coords (newX, newY)

-- dot: Calculates the dot product between two points treated as vectors.
-- Dot product formula: (x1*x2) + (y1*y2).
dot :: Point -> Point -> Float
dot p1 p2 = x1 * x2 + y1 * y2
    where 
        (x1, y1) = desCoords p1
        (x2, y2) = desCoords p2

-- sub: Subtracts one point from another, returning a new point representing the difference between coordinates.
-- Point subtraction formula: (x1 - x2, y1 - y2).
sub :: Point -> Point -> Point
sub p1 p2 = (-) <$> p1 <*> p2

-- perp: Calculates the perpendicular vector to a given point (treated as a vector).
-- The perpendicular vector to (x, y) is (-y, x).
perp :: Vector -> Vector
perp (Coords (x,y)) = Coords (-y,x)

-- isInBounds: Checks if a point lies within the bounds defined by a given size.
isInBounds :: Point -> Size -> Bool
isInBounds p s = (abs x <= w / 2) && (abs y <= h / 2)
    where 
        (x,y) = desCoords p
        (w,h) = desCoords s

-- vecFromModuleAngle: Function that returns a vector given its magnitude (with sign according to direction) and angle.
vecFromModuleAngle :: Float -> Radian -> Vector
vecFromModuleAngle m a = Coords (m * cos a, m * sin a)

-- mul: Function such that (w,h) `mul` (sw,sh) = (w * sw, h * sh)
-- Multiplies the components of two tuples.
mul :: Fractional a => Coords a -> Coords a -> Coords a
mul s1 s2 = (*) <$> s1 <*> s2 

-- normalizarAngulo: Function that normalizes the angle to the range (-pi, pi).
normalizarAngulo :: Radian -> Radian
normalizarAngulo a
    | a > 2*pi    = a - 2*pi
    | a < (-2*pi) = a + 2*pi
    | otherwise = a

-- desplazarAPosicion: Function that returns the list of points of an entity after shifting it, centering it at a given point.
desplazarAPosicion :: [Position] -> Position -> [Position]
desplazarAPosicion puntos nuevoCentro = (\p -> sumVec p vecDesplazamiento) <$> puntos
    where
        vecDesplazamiento = subVec nuevoCentro (centroide puntos)

-- radioMaxPuntos: Function that calculates the radius of the smallest circle containing the rectangle.
radioMaxPuntos :: [Position] -> Distance
radioMaxPuntos ps = maximum $ (\p -> distanceBetween centro p) <$> ps
    where
        centro = centroide ps

-- redondea: Rounds the number x to n decimal places
redondea :: Integer -> Float -> Float
redondea n x = fromIntegral (round (x * 10^n)) / 10^n
