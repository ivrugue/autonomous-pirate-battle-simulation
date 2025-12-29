module HAUS.Types where

-- Point. A 2D point in space.
type Point = Coords Float

-- Vector. Vector whose origin is always considered to start at (0,0).
type Vector = Coords Float

-- Angle. An angle with decimal precision.
type Angle = Float
-- Subtypes to differentiate when the angle is in degrees or radians.
type Degree = Angle
type Radian = Angle

-- Distance. A distance value with decimals.
type Distance = Float

-- Position. Represents the position of an object in a 2D world.
type Position = Point

-- Size. Type needed for the isInBounds function. Represents the width and height of the bounding rectangle.
type Size = Coords Float


-- Coordinate. Type to wrap tuples (a,a) representing coordinates of points, vectors, or similar.
data Coords a = Coords (a, a)
    deriving (Show, Eq)

-- desCoords: Function to unpack Coords. Allows access to its components.
desCoords :: Coords a -> (a, a)
desCoords (Coords p) = p

-- Functor implementation
instance Functor Coords where  
    -- fmap: Applies function f to both components.
    fmap f (Coords (x, y)) = Coords (f x, f y)

-- Applicative implementation
instance Applicative Coords where  
    -- pure x: Wraps a pure value in Coords (x, x). 
    pure x = Coords (x, x)

    -- (<*>): Applies the functions f and g component-wise.
    (Coords (f, g)) <*> (Coords (x, y)) = Coords (f x, g y)
