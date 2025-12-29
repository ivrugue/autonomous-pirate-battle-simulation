module HAUS.AI where

import qualified Data.Map as Map
import Data.List
import HAUS.Types 
import HAUS.Memory
import HAUS.Entities
import HAUS.Physics
import HAUS.Behavior
import PilaTA

-- Converts a Haskell list to our TAD Stack.
listToPila :: [Position] -> Pila Position
listToPila = foldr apila vacia

-- Converts our TAD Stack to a Haskell list.
pilaToList :: Pila Position -> [Position]
pilaToList p
    | esVacia p = []
    | otherwise   = cima p : pilaToList (desapila p)

-- Auxiliary function to convert [Position] to MemList type
toMemList :: [Position] -> ValorMemory
toMemList = MemList . map MemPosition

-- scanRobot: Function that scans the radar environment and stores the positions of detected entities in the robot's memory.
scanRobot :: Game -> Robot -> Robot
scanRobot game robot = robot {info = (info robot) {memoria = newMem}}
    where 
        oldMem = memoria (info robot)
        
        posDetectadas cond xs = posicion <$> filter cond xs
        robotEnRadar r = (index (info r) /= index (info robot) && isRobotAlive r && entidadEnRadar r)
        entidadEnRadar ent = detectedAgent robot ent

        posicionRobotsDetectados = posDetectadas robotEnRadar (robots game)
        posicionProjectilesDetectados = posDetectadas entidadEnRadar (proyectiles game)
        posicionExplosionsDetectadas = posDetectadas entidadEnRadar (explosiones game)

        filtraObstacles t = filter (\obs -> tipo_obstaculo (info obs) == t) (obstaculos game)
        posicionObstaclesDetectados t = posDetectadas entidadEnRadar (filtraObstacles t)
        posicionSolidsDetectados = posicionObstaclesDetectados Solid
        posicionHarmfulsDetectados = posicionObstaclesDetectados Harmful 
        posicionExplosivesDetectados = posicionObstaclesDetectados Explosive
        posicionHealingsDetectados = posicionObstaclesDetectados Healing

        esAmenaza = (\pos -> distanceBetween (posicion robot) pos < 60)
        posicionAmenazasDetectadas = posicionProjectilesDetectados
                                  ++ posicionExplosionsDetectadas
                                  ++ filter esAmenaza posicionSolidsDetectados 
                                  ++ filter esAmenaza posicionHarmfulsDetectados 
                                  ++ filter esAmenaza posicionExplosivesDetectados


        -- STACK LOGIC (TAD)
        -- 1. Read the list of positions from memory.
        patrolList = getPositions "patrolPoints" oldMem
        -- 2. Convert that list to our TAD Stack.
        patrolStack = listToPila patrolList
        -- newPatrolStack will contain the stack after applying the logic.
        newPatrolStack = 
            -- Case 1: We see robots
            if not (null posicionRobotsDetectados) then
                let nuevoCentroide = centroide posicionRobotsDetectados
                in
                    -- Do PUSH
                    if esVacia patrolStack then
                        apila nuevoCentroide patrolStack -- Push if empty
                    else 
                        -- Optimization: do not push if the new point is very close to the top
                        let top = cima patrolStack
                        in if distanceBetween top nuevoCentroide < 50 then
                            patrolStack -- Do not push
                           else 
                            apila nuevoCentroide patrolStack -- Push
            
            -- Case 2: No robots detected, but stack is NOT empty.
            else if not (esVacia patrolStack) then
                let top = cima patrolStack
                in
                    -- If near the top point, POP
                    if distanceBetween (posicion robot) top < 50 then
                        desapila patrolStack
                    -- Otherwise, keep going to top, do not change stack
                    else patrolStack
            
            -- Case 3: No robots detected and stack is empty.
            else patrolStack

        -- 3. Convert the TAD Stack back to a list.
        newPatrolList = pilaToList newPatrolStack
        -- 4. Save the list back into memory.
        memConPatrulla = Map.insert "patrolPoints" (toMemList newPatrolList) oldMem

        toMemory = MemList . map MemPosition
        newMem = Map.union
            (Map.fromList 
                [("posicionRobotsDetectados", toMemory posicionRobotsDetectados)
                , ("posicionAmenazasDetectadas", toMemory posicionAmenazasDetectadas)
                , ("posicionHealingsDetectados", toMemory posicionHealingsDetectados)]) 
            (memConPatrulla)


-- acciones: Function that returns an Action for each alive robot in the game.
acciones :: Game -> [Action]
acciones = map decidirComportamiento . filter isRobotAlive . robots


-- decidirComportamiento: Function that returns an Action dedicated to a Robot to decide its next move based on memory.
decidirComportamiento :: Robot -> Action
decidirComportamiento robot = Action {robot = index (info robot), seMueve = avance, giraCuerpo = giroCuerpo, giraTorreta = giroTorreta, dispara = accionDisparoFinal} 
    where
        infoRobot = info robot

        -- Priority 0. Check if the robot is "stunned" by a rebound
        esAturdido = tiempoRebote infoRobot > 0.0

        posicionAmenazasDetectadas = getPositions "posicionAmenazasDetectadas" (memoria infoRobot)
        posicionRobotsDetectados = getPositions "posicionRobotsDetectados" (memoria infoRobot)
        posicionHealingsDetectados = getPositions "posicionHealingsDetectados" (memoria infoRobot)

        cercanos dist = filter (\p -> distanceBetween (posicion robot) p < dist)
        posicionHealingsCercanos = cercanos 80 posicionHealingsDetectados
        posicionRobotsZonaRiesgo = cercanos 150 posicionRobotsDetectados

        -- MOVEMENT DECISIONS
        (avance,giroCuerpo) = 
            -- If stunned, force backward movement
            if esAturdido then
                (Forwards, StopTurn)
            -- If not stunned, execute normal logic
            else case (tipo $ info robot) of                
                Basic
                    -- Priority 1. Approach a Healing if one is nearby.  
                    | not (null posicionHealingsCercanos) ->
                        avanzarHacia robot (masCercano posicionHealingsCercanos robot)

                    -- Priority 2. Smart flee. If more than one threat, flee from their centroid.
                    | length posicionAmenazasDetectadas > 1 ->
                        huirCentroide robot posicionAmenazasDetectadas

                    -- Priority 3. If threats are nearby, move away from them.
                    | not (null posicionAmenazasDetectadas) -> 
                        esquivar robot (masCercano posicionAmenazasDetectadas robot)

                    -- Priority 4. Approach a Healing if one is nearby.  
                    | not (null posicionHealingsDetectados) ->
                        avanzarHacia robot (masCercano posicionHealingsDetectados robot) 

                    -- Priority 5. If robots are too close, maintain safe distance.
                    | not (null posicionRobotsZonaRiesgo) -> 
                        esquivar robot (masCercano posicionRobotsZonaRiesgo robot)  

                    -- Priority 6. If robots are near (safe zone), approach them
                    | not (null posicionRobotsDetectados) ->
                        avanzarHacia robot (masCercano posicionRobotsDetectados robot)

                    -- Priority 7. If radar detects nothing, consult the stack (stored last detected positions).
                    | otherwise -> 
                        let patrolList = getPositions "patrolPoints" (memoria infoRobot)
                        in case patrolList of
                            (top:_) -> avanzarHacia robot top
                            [] -> (Forwards, StopTurn)
                Kamikaze
                    -- 1. If robots exist, move towards them.
                    | not (null posicionRobotsDetectados) -> avanzarHacia robot (masCercano posicionRobotsDetectados robot)
                    -- 2. Otherwise, move forward.
                    | otherwise -> (Forwards, StopTurn)
                _ -> (StopMovement, StopTurn)  -- If another type (Tower), do not move.

        -- SHOOTING DECISION
        (giroTorreta, decisionDisparo)
            -- If stunned, do not shoot or turn turret
            | esAturdido = (StopTurn, DoNotShoot)
            -- Normal logic
            | null posicionRobotsDetectados = (StopTurn,DoNotShoot)
            | otherwise = disparar robot (masCercano posicionRobotsDetectados robot)
        -- Cooldown logic
        listoParaDisparar = enfriamientoActual infoRobot <= 0.0
        -- Final decision
        accionDisparoFinal
            | decisionDisparo == Shoot && listoParaDisparar = Shoot
            | otherwise = DoNotShoot

-- AUXILIARY FUNCTIONS for decidirComportamiento
-- getPositions: Function to facilitate access to lists of detected robots and threats.
getPositions :: String -> Memory -> [Position]
getPositions clave memoria =
    case Map.lookup clave memoria of
        Just (MemList xs) -> [p | MemPosition p <- xs]  -- If exists, extract the list of positions.
        _                 -> []                         -- Otherwise, return empty list.

-- masCercano: Function that returns the closest position to a Robot from a list.
masCercano :: [Position] -> Robot -> Position
masCercano listaPosiciones robot = minimumBy (\p1 p2 -> compare (distanceBetween pos p1) (distanceBetween pos p2)) listaPosiciones
    where pos = posicion robot


-- esquivar: Function that decides robot movement to dodge the closest threat.
-- Calculates the angle between the robot and the threat, and decides movement and turning to avoid collision.
esquivar :: Robot -> Position -> (MovementAction, TurnAction)
esquivar robot amenaza = (Forwards, giro) -- Always flee forward
    where
        alfa = angulo robot
        beta = angleToTarget (posicion robot) amenaza

        -- Calculates a perpendicular angle to create a curved trajectory; avoids straight-line escape.
        anguloHuida = normalizarAngulo (beta + pi/2) -- +90° = left curve
        -- Normalize difference to find shortest path.
        diff = normalizarAngulo (anguloHuida - alfa)

        -- Turn so threat is behind.
        giro
            | diff > 0.15  = TurnLeft
            | diff < -0.15  = TurnRight
            | otherwise = StopTurn

-- disparar: Function that decides turret movement to aim and shoot at the closest robot.
-- Calculates the angle between turret and target, returns turning action and shoot order.
disparar :: Robot -> Position -> (TurnAction, ShootAction)
disparar r1 p = (giro, Shoot) -- Always decides to shoot.
    where
        alfa = angulo_torreta (info r1)
        beta = angleToTarget (posicion r1) p 
        -- Normalize difference to find shortest path.
        diff = normalizarAngulo (beta - alfa)

        -- Turn turret to aim at target
        giro
            | diff > 0.05  = TurnLeft  -- Small threshold to avoid "shaking".
            | diff < -0.05 = TurnRight
            | otherwise    = StopTurn


-- avanzarHacia: Function that returns the necessary action for the robot to move forward and turn body to face target.
avanzarHacia :: Robot -> Position -> (MovementAction, TurnAction)
avanzarHacia r p = (Forwards, giro) -- Always moves forward.
    where
        alfa = angulo r
        beta = angleToTarget (posicion r) p
        diff = normalizarAngulo (beta - alfa)
        
        -- Turn body to face enemy.
        giro
            | diff > 0.01  = TurnLeft
            | diff < -0.01 = TurnRight
            | otherwise    = StopTurn   


-- huirCentroide: Function that calculates the centroid of threats detected by a robot and dodges it.
huirCentroide :: Robot -> [Position] -> (MovementAction, TurnAction)
huirCentroide robot amenazas = esquivar robot centro
    where   
        centro = centroide amenazas
