module HAUS.Spawner where

import HAUS.Types
import HAUS.Physics
import HAUS.Entities
import HAUS.Creators

import Data.List (mapAccumL)
import System.Random

-- randomPosAngle: generates a random position and angle (in radians)
randomPosAngle :: (Float, Float) -> (Float, Float) -> (Float, Float) -> StdGen -> (StdGen, (Position, Float))
randomPosAngle (minX, maxX) (minY, maxY) (minA, maxA) g = (g3, (Coords (x, y), a))
    where
        (x, g1) = randomR (minX, maxX) g
        (y, g2) = randomR (minY, maxY) g1
        (a, g3) = randomR (minA, maxA) g2

-- crearEntitiesAleatorias: Takes a game template and a generator and returns a new game state with robots and obstacles in random positions.
crearEntitiesAleatorias :: StdGen -> Game -> (Game, StdGen)
crearEntitiesAleatorias g game = (game { robots = robotsNuevos, obstaculos = obstaculosNuevos }, g2)
    where
        (Coords (w, h)) = size game
        -- Leave a 50px margin from the edge.
        (minX, maxX) = (-w/2 + 50, w/2 - 50)
        (minY, maxY) = (-h/2 + 50, h/2 - 50)
        
        {- We use 'mapAccumL' to generate a list of random positions, one for each robot.
           We cannot use a normal 'map' because 'randomPos' needs to update the generator (StdGen) each time.
           If we used 'map', we would pass the same generator 'g' each iteration, getting the same position for all robots.

           'mapAccumL' works as follows:
           1. Takes an initial "accumulator" (our generator 'g') and a list (our 'robots game').
           2. For each robot, calls our function. This function receives the current accumulator ('gen') and returns a tuple: (newAccumulator, result).
              In our case: (newGenerator, newRandomPositionAngle).
           3. 'mapAccumL' passes that 'newGenerator' as the accumulator for the next iteration.
           4. At the end, 'mapAccumL' returns a tuple with:
              - 'gX': The final accumulator state (the last random generator).
              - 'posAngList': The list of all results generated (our list of random positions and angles).
        -}

        -- Randomize robots
        (g1, posAngListRobots) = mapAccumL (\gen _ -> randomPosAngle (minX, maxX) (minY, maxY) (0, 2*pi) gen) g (robots game)

        robotsNuevos = zipWith (\r (pos, a) -> r { puntos = desplazarAPosicion (puntos_base (info r)) pos
                                                 , angulo = if (tipo $ info r) == Tower then 0 else a 
                                                 }) (robots game) posAngListRobots
        
        -- Randomize obstacles
        (g2, posAngListObstacles) = mapAccumL (\gen _ -> randomPosAngle (minX, maxX) (minY, maxY) (-pi/4, pi/4) gen) g1 (obstaculos game)

        obstaculosNuevos = zipWith (\o (pos, a) -> o { puntos = puntosColocados o pos a
                                                     , angulo = a
                                                     }) (obstaculos game) posAngListObstacles
            where 
                ptsDesplazados o pos = desplazarAPosicion (puntos o) pos
                puntosColocados o pos a = getVertices (p1,p2,p3,p4,a) pos
                    where [p1,p2,p3,p4] = ptsDesplazados o pos


-- actualizarSpawns: adds Explosive and Healing obstacles if enough time has passed to spawn a new one
actualizarSpawns :: Float -> StdGen -> Game -> (Game, StdGen)
actualizarSpawns dt g_in game =
    let
        tExp = tiempoExplosive game - dt
        tCur = tiempoHealing game - dt
        
        -- Explosive
        (gameExp, finalTExp, g1) = 
            if tExp <= 0.0
            then spawnObstacle Explosive game (intervaloExplosive game) g_in
            else (game { tiempoExplosive = tExp }, tExp, g_in)
        
        -- Healing
        (gameFinal, finalTCur, g_out) = 
            if tCur <= 0.0
            then spawnObstacle Healing gameExp (intervaloHealing gameExp) g1
            else (gameExp { tiempoHealing = tCur }, tCur, g1)
    in
    (gameFinal { tiempoExplosive = finalTExp, tiempoHealing = finalTCur }, g_out)

-- spawnObstacle: adds an obstacle of the indicated type in a random position
spawnObstacle :: TipoObstacle -> Game -> Float -> StdGen -> (Game, Float, StdGen)
spawnObstacle tipo game intervalo gen =
    let
        (Coords (w, h)) = size game
        -- Leave a 50px margin from the edge.
        (minX, maxX) = (-w/2 + 50, w/2 - 50)
        (minY, maxY) = (-h/2 + 50, h/2 - 50)

        -- Generate a random position
        (gen', (pos, ang)) = randomPosAngle (minX, maxX) (minY, maxY) (0, 2*pi) gen  -- 0, 2*pi == random angle between 0º and 360º

        -- Select the type and create the obstacle
        nuevoObstacle = 
            case tipo of
                Solid    -> crearObstacleSolid pos
                Harmful    -> crearObstacleHarmful pos
                Explosive -> crearObstacleExplosive pos
                Healing  -> crearObstacleHealing pos
        
    in
    (game { obstaculos = (obstaculos game ++ [nuevoObstacle]) }     -- Add it to the obstacle list
    , intervalo                                                      -- Reset timer to corresponding interval
    , gen')                                                          -- Return the updated generator
