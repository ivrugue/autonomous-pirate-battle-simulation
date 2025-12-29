module HAUS.Behavior where

import HAUS.Types 
import HAUS.Entities
import HAUS.Physics

-- posicion: Function that calculates the centroid of an Entity's points to determine its center position.
posicion :: Entity a -> Position
posicion entidad = centroide (puntos entidad)

-- detectedAgent: Determine if an agent has detected another when within radar range.
-- Compares the distance between the detecting robot and the target using 'distanceBetween'.
-- If this distance is less than the maximum detection distance, returns True.
detectedAgent :: Robot -> Entity a -> Bool
detectedAgent robot objetivo =
    distanceBetween (posicion robot) (posicion objetivo) <= (radar $ info robot)
 
-- isRobotAlive: True if the robot's health is greater than 0.
-- Checks the robot's life and returns True if greater than 0.
isRobotAlive :: Robot -> Bool
isRobotAlive robot = (vida $ info robot) > 0

-- countActiveRobots: Count the robots that are alive.
-- Filters the robots that are alive and counts them with 'length'.
countActiveRobots :: Game -> Int 
countActiveRobots game = length $ filter isRobotAlive (robots game)

-- updateRobotVelocity: Updates a robot's velocity with a given speed.
-- Updates the magnitude and angle of the velocity vector and rotates the robot's points according to the new angle.
updateRobotVelocity :: Robot -> Float -> Radian -> Robot
updateRobotVelocity robot newV newA = robot {puntos = puntosFinales, velocidad = newV, angulo = newA}
    where
        [p1, p2, p3, p4] = puntos_base (info robot)
        puntosRotados = getVertices (p1, p2, p3, p4, newA) (Coords (0,0))
        puntosFinales = desplazarAPosicion puntosRotados (posicion robot)

-- updateVelocity: Update velocity based on the movement action informed by the bot (see notes).
{- Given the movement action (Forwards, Backwards, StopMovement) it changes the velocity according to the direction and acceleration, 
and given the turn action (TurnRight, TurnLeft, StopTurn), it changes the direction in which it moves. -}
updateVelocity :: Robot -> MovementAction -> TurnAction -> Robot
updateVelocity robot accion_a accion_g = updateRobotVelocity robot newV newA
    where
        v = velocidad robot
        vmax = velMax $ info robot
        acel = aceleracion_avance $ info robot
        newV = case accion_a of
            StopMovement
                | (v == 0) || (abs v < abs acel) -> 0    -- If already stopped or about to stop, stop.
                | v > 0 -> v - acel                      -- If moving, decelerate (accelerate in opposite direction)
                | v < 0 -> v + acel 
            Forwards
                | v > 0 -> min (v + acel) vmax         -- If already moving forward, accelerate only if below vmax.
                | otherwise -> v + acel                  -- Otherwise, accelerate forward.
            Backwards
                | v < 0 -> max (v - acel) (-vmax)      -- If already moving backward, accelerate only if above -vmax.
                | otherwise -> v - acel                  -- Otherwise, accelerate backward.
        dirGiro
            | accion_g == TurnRight = -1 -- Turn right (clockwise) SUBTRACT angle
            | accion_g == TurnLeft = 1   -- Turn left (counterclockwise) ADD angle
            | otherwise = 0
        newA = dirGiro * (paso_giro $ info robot) + (angulo robot)


-- updatePosition: Update a position based on velocity and time increment.
-- Calculates for each point of the entity the updated position using: position + velocity * time
updatePosition :: Robot -> Float -> Game -> Robot
updatePosition robot dt game = robotFinal
        where
            vecVel = vecFromModuleAngle (velocidad robot) (angulo robot)
            puntosFinales = (\p -> sumVec p (pure (*dt) <*> vecVel)) <$> (puntos robot)
            robotFinal = robot { puntos = puntosFinales }
            

-- updateTorreta: Function that updates the turret angle based on the action decided by the robot.
updateTorreta :: TurnAction -> Robot -> Robot
updateTorreta accionGiro r = r { info = (info r) { angulo_torreta = nuevoAngulo } }
    where
        anguloDisparo = angulo_torreta $ info r
        pasoGiro = paso_giro $ info r
        anguloDeGiro = case accionGiro of
            TurnLeft  -> pasoGiro
            TurnRight -> -pasoGiro
            StopTurn  -> 0
        nuevoAngulo = normalizarAngulo (anguloDeGiro + anguloDisparo)

-- updateRobot: Function that updates a robot's body and turret given an Action.
updateRobot :: Action -> Robot -> Robot
updateRobot accion r = if index (info r) == robot accion then rActualizado else r
    where
        rVelocidadActualizada = updateVelocity r (seMueve accion) (giraCuerpo accion)
        rActualizado = updateTorreta (giraTorreta accion) rVelocidadActualizada

-- radioMax: Function that calculates the radius of the smallest circle containing the entity's rectangle.
radioMax :: Entity a -> Distance
radioMax ent = radioMaxPuntos $ puntos ent
