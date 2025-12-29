module HAUS.Collisions where

import HAUS.Types 
import HAUS.Entities
import HAUS.Physics
import HAUS.Behavior

-- checkCollision: Checks if two rectangles have collided using the appropriate algorithm.
-- Checks if two rectangles overlap.
-- Projects both rectangles onto axes perpendicular to their edges and checks if there is a gap between the projections.
-- If there is a gap on any axis, there is no collision.
-- If all axes overlap, then a collision occurs.
checkCollision :: [Point] -> [Point] -> Bool
checkCollision rectangulo1 rectangulo2 =
    not $ or [checkGap (findMin rectangulo1 a) (findMax rectangulo1 a) (findMin rectangulo2 a) (findMax rectangulo2 a) | a <- perpStack]
    where
        aristas1 = getAristas rectangulo1
        perparistas1 = getPerpendicular aristas1
        aristas2 = getAristas rectangulo2
        perparistas2 = getPerpendicular aristas2
        perpStack = perparistas1 ++ perparistas2

-- AUXILIARY FUNCTIONS for 'checkCollision'

-- getAristas. Obtains the edges of a rectangle given a list of four points.
{- To do this, subtract each vertex from the next using 'zipWith' with 'sub'.
This unifies the two lists (the original and the one shifted by one position)
and performs the subtraction for each pair.
Thus, we get the sides (edges): p1->p2, p2->p3, p3->p4, p4->p1. -}
getAristas :: [Point] -> [Vector]
getAristas rect = zipWith sub (tail rect ++ [head rect]) rect

-- getPerpendicular. Obtains the vectors perpendicular to each rectangle edge.
getPerpendicular :: [Vector] -> [Vector]
getPerpendicular = map perp 

-- findMin. Gets the minimum projection of the rectangle's vertices with respect to the given vector
-- useful for calculating amin and bmin.
findMin :: [Point] -> Vector -> Float
findMin rectangulo a = minimum [dot v a | v <- rectangulo]

-- findMax. Gets the maximum projection of the rectangle's vertices with respect to the given vector
-- useful for calculating amax and bmax.
findMax :: [Point] -> Vector -> Float
findMax rectangulo a = maximum [dot v a | v <- rectangulo]

-- checkGap. Checks if there is a gap between the projections defined by amin, amax, bmin, and bmax.
-- Returns True if a gap exists, otherwise False.
checkGap :: Float -> Float -> Float -> Float -> Bool
checkGap amin amax bmin bmax = amax < bmin || bmax < amin


-- detectRobotProjectileCollisions: Detects collisions between robots and projectiles.
-- Generates a 'CollisionRobotProjectile' event for each projectile that hits a live robot,
-- as long as the robot is not the one that launched the projectile and their positions collide.
detectRobotProjectileCollisions :: [Robot] -> [Projectile] -> [CollisionEvent]
detectRobotProjectileCollisions robots proyectiles =
    [CollisionRobotProjectile r p | r <- robots, p <- proyectiles, isRobotAlive r && index (info r) /= (robot_lanzador (info p)) && checkCollision (puntos r) (puntos p)] 


-- detectRobotExplosionCollisions: Detects collisions between robots and explosions
detectRobotExplosionCollisions :: [Robot] -> [Explosion] -> [CollisionEvent]
detectRobotExplosionCollisions robots explosiones = [CollisionRobotExplosion r e | r <- robots, e <- explosiones, isRobotAlive r && checkCollision (puntos r) (puntos e)]


-- detectRobotRobotCollisions: Detects collisions between pairs of robots in the game.
-- Iterates over all pairs of live and distinct robots, checking with 'checkCollision' if their areas overlap. 
-- For each detected collision, a 'CollisionRobots' event is generated with the two entities involved.
detectRobotRobotCollisions :: [Robot] -> [CollisionEvent]
detectRobotRobotCollisions robots = [CollisionRobots r1 r2 | r1 <- robots, r2 <- robots
                                                            , isRobotCollidable r1 && isRobotCollidable r2 
                                                            && index (info r1) < index (info r2) 
                                                            && checkCollision (puntos r1) (puntos r2)]


-- detectRobotObstacleCollisions: Detects collisions between robots and obstacles
detectRobotObstacleCollisions :: [Robot] -> [Obstacle] -> [CollisionEvent]
detectRobotObstacleCollisions robots obstaculos =
    [CollisionRobotObstacle r obs | r <- robots, obs <- obstaculos, isRobotAlive r && checkCollision (puntos r) (puntos obs)]


-- isRobotCollidable: Returns True if the robot is alive and not stunned.
isRobotCollidable :: Robot -> Bool
isRobotCollidable r = isRobotAlive r && (tiempoRebote (info r) <= 0.0)
