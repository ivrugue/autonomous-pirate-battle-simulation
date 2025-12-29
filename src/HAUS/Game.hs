module HAUS.Game where

import qualified Data.Map as Map
import System.Random (StdGen)
import Data.List

import HAUS.Types 
import HAUS.Memory
import HAUS.Entities
import HAUS.Physics
import HAUS.Collisions
import HAUS.Behavior
import HAUS.AI
import HAUS.Creators
import HAUS.Stats

-- GAME STATES
data GameState
    = Start    -- The game is waiting to start.
    | Playing  -- The game is ongoing and active.
    | Paused   -- The game is ongoing but paused.
    | Finished (Maybe RobotId) Float -- The game has finished, store the winner's ID (if any) and maintain the state for some time
    | NoGame   -- No game exists (0 or negative tournaments are set)
    deriving (Eq, Show)

-- The world state includes the GameState, the Game, the elapsed time in the match, the Memory with configuration, the number of remaining tournaments,
-- the generator used whenever an entity is placed at a random position, and the tournament statistics.
data WorldState = WorldState 
    { estado_juego :: GameState
    , game :: Game
    , tiempo :: Float
    , config :: Memory
    , torneos_restantes :: Int
    , gen :: StdGen
    , stats :: [Stats]
    , stats_escritas :: Bool
    }

-- actualizarPosicionesEntitiesMoviles: Function that updates the positions of robots and projectiles considering their current angle and speed.
actualizarPosicionesEntitiesMoviles :: Float -> Game -> Game
actualizarPosicionesEntitiesMoviles dt game =
    let
        -- Projectile update logic (simple translation)
        updateProy p = p {puntos = newP}
            where
                Coords (vx, vy) = vecFromModuleAngle (velocidad p) (angulo p)
                newP = (\(Coords (px,py)) -> Coords (px + vx * dt, py + vy * dt)) <$> (puntos p)

        updateObstacle obs
            | (tipo_obstaculo $ info obs) == Explosive && (activo $ info obs) = obs
                { info = (info obs) 
                    { tiempo_espera = (tiempo_espera $ info obs) - dt
                    }
                }
            | otherwise = obs
    in
    game
        { robots = (rebotarSiTocaBorde game . actualizarEnfriamiento dt . (\r -> updatePosition r dt game)) <$> (robots game)
        , proyectiles = updateProy <$> (proyectiles game)
        , explosiones = updateExplosion <$> (explosiones game)
        , obstaculos = updateObstacle  <$> (obstaculos game)
        }

-- aplicarAcciones: Function that adjusts robot velocities based on the decision each robot makes knowing the environment.
aplicarAcciones :: Game -> Game
aplicarAcciones game =
    -- 1. First, all robots scan the environment to update their memory.
    let robotsActualizados = map (scanRobot game) (robots game)
        juegoConScaneo = game { robots = robotsActualizados, eventosColisionActivos = [] }
    -- 2. Then they decide their actions based on that updated memory.
    in foldl aplicarUnaAccion juegoConScaneo (acciones juegoConScaneo)

-- AUXILIARY FUNCTIONS aplicarAcciones
-- aplicarUnaAccion: Function that applies an action to the game state, modifying the robot's speed and angle
-- and creating a projectile if it decided to shoot.
aplicarUnaAccion :: Game -> Action -> Game
aplicarUnaAccion g accion = g { robots = robotsActualizados, proyectiles = proyectiles g ++ nuevosProjectiles }
    where
        -- Apply movement and reset cooldown (if needed).
        robotsActualizados = map (resetCooldownSiDispara accion . updateRobot accion) (robots g)
        robotQueDispara = head $ filter (\r -> index (info r) == robot accion) robotsActualizados
        nuevosProjectiles = if dispara accion == Shoot then [crearProjectile robotQueDispara] else []

-- limpiarEntities: Function that removes dead robots and other entities.
limpiarEntities :: Game -> Game
limpiarEntities game = game 
        { robots = filter isRobotAlive $ quitaDañoArea <$> (robots game)
        , proyectiles = filter (esProjectileValido game) (proyectiles game)
        , obstaculos = filter (not . haTerminadoObstacle) (obstaculos game)
        , explosiones = explosionesActivas ++ nuevasExplosions ++ explosionesMinas
        }
    where
        esProjectileValido :: Game -> Projectile -> Bool
        esProjectileValido g p =
            let enRango = not (haTerminadoProjectile p)
                -- Checks that all projectile vertices are on screen; otherwise, it removes them.
                enPantalla = all (\punto -> isInBounds punto (size g)) (puntos p)
            in enRango && enPantalla

        explosionesActivas = filter (not . haTerminadoExplosion) (explosiones game)
        nuevasExplosions = crearExplosionGrande <$> filter (not . isRobotAlive) (robots game)
        
        obstaculosTerminados = filter haTerminadoObstacle (obstaculos game)
        explosionesMinas = crearExplosionPequeña <$> obstaculosTerminados

        -- For a robot, check if it is within the area of each finished Explosive and subtract daño_area for each one
        quitaDañoArea r = r { info = (info r) {vida = (vida $ info r) - dañoTotal} } 
            where 
                dañoTotal = sum $ [daño_area $ info obs 
                    | obs <- obstaculosTerminados
                    , any (\p -> distanceBetween p (posicion obs) <= (radio_daño $ info obs)) (puntos r)]



-- AUXILIARY FUNCTIONS for limpiarEntities
-- haTerminadoProjectile: Function that returns True if the projectile has exceeded its range limit.
haTerminadoProjectile :: Projectile -> Bool
haTerminadoProjectile p = distanceBetween (posicion p) (posicion_origen $ info p) > alcance (info p)

-- haTerminadoExplosion: Function that returns True if the current frame of the explosion is the last one.
haTerminadoExplosion :: Explosion -> Bool
haTerminadoExplosion explosion = null $ proximas_imagenes $ info explosion

-- haTerminadoObstacle: Function that returns True if the Explosive obstacle has finished its countdown (False for other types).
haTerminadoObstacle :: Obstacle -> Bool
haTerminadoObstacle obstaculo = tipo_obstaculo (info obstaculo) == Explosive && tiempo_espera (info obstaculo) <= 0


-- updateExplosion: Function that advances the explosion animation to the next frame. Returns the updated image field.
updateExplosion :: Explosion -> Explosion
updateExplosion explosion =
    let
        infoExp = info explosion
        framesRestantes = proximas_imagenes infoExp
    in
        -- Check if there are frames left in the animation.
        if null framesRestantes
        then explosion -- If no more frames, the animation is finished (haFinalizado will remove it).
        else
            -- Advance to the next frame:
            -- 1. The new image is the first of the remaining list.
            -- 2. The new list of "next images" is the rest of the list (the tail).
            explosion { imagen = head framesRestantes
                      , info = infoExp { proximas_imagenes = tail framesRestantes }
                      }


-- actualizarEnfriamiento: Function that updates the time remaining for a robot to fire its next projectile.
-- In Game.hs
actualizarEnfriamiento :: Float -> Robot -> Robot
actualizarEnfriamiento dt r =
    let infoRobot = info r
        enfriamiento = enfriamientoActual infoRobot
        rebote = tiempoRebote infoRobot
        
        nuevoEnfriamiento = max 0.0 (enfriamiento - dt)
        nuevoRebote = max 0.0 (rebote - dt)
    -- Updates both timers
    in r { info = infoRobot { enfriamientoActual = nuevoEnfriamiento, tiempoRebote = nuevoRebote } }


-- resetCooldownSiDispara: Function that resets the robot's cooldown if it shoots, restarting the "timer" by setting it to the cooldown value.
resetCooldownSiDispara :: Action -> Robot -> Robot
resetCooldownSiDispara acc r
    -- If it is not the robot of the action or the action was not to shoot, do nothing.
    | index (info r) /= robot acc || dispara acc == DoNotShoot = r
    -- If it is the robot of the action and it fired, reset its counter to the 'cooldown' value.
    | otherwise = r { info = (info r) { enfriamientoActual = cooldown (info r) } }


-- checkCollisions: Main function that coordinates all collision checks, updating the game state with detected collisions.
checkCollisions :: Game -> Game
checkCollisions game = game {eventosColisionActivos = eventosColision}
    where
        colisionesRobotProjectile = detectRobotProjectileCollisions (robots game) (proyectiles game)
        colisionesRobotRobot = detectRobotRobotCollisions (robots game)
        colisionesRobotExplosion = detectRobotExplosionCollisions (robots game) (explosiones game) 
        colisionesRobotObstacle = detectRobotObstacleCollisions (robots game) (obstaculos game) 
        eventosColision = colisionesRobotProjectile ++ colisionesRobotRobot ++ colisionesRobotExplosion ++ colisionesRobotObstacle

-- AUXILIARY FUNCTION for checkCollision
-- gestionarEventosColision: Function that handles all collision events accumulated in the game.
gestionarEventosColision :: Game -> Game
gestionarEventosColision game = gameConDañoAplicado
    where gameConDañoAplicado = foldl resolverEvento game (eventosColisionActivos game)
        
-- AUXILIARY FUNCTIONS for gestionarEventosColision
-- resolverEvento: Helper function that resolves a collision event. Defined for the three possible collision cases.
resolverEvento :: Game -> CollisionEvent -> Game
resolverEvento g (CollisionRobotProjectile robot proyectil) = g'
    { proyectiles = filter (/= proyectil) (proyectiles g')
    , explosiones = explosion:(explosiones g)
    }
    where
        robotId = index (info robot)
        dañoProjectile = daño proyectil
        g' = aplicarDaño robotId dañoProjectile g
        explosion = crearExplosionPequeña proyectil
 
resolverEvento g (CollisionRobots r1 r2) = g''''
    where
        id1 = index (info r1)
        id2 = index (info r2)
        
        -- Step 1: Apply damage.
        g' = aplicarDaño id1 (daño r2) g
        g'' = aplicarDaño id2 (daño r1) g'

        -- Step 2: Apply rebounds.
        g''' = aplicarRebote id1 r2 g''
        g'''' = aplicarRebote id2 r1 g'''

resolverEvento g (CollisionRobotExplosion robot explosion) = g'
    where
        robotId = index (info robot)
        dañoExplosion = daño explosion
        g' =  aplicarDaño robotId dañoExplosion g


resolverEvento g (CollisionRobotObstacle robot obstaculo) = g'''
    where
        robotId = index (info robot)
        dañoObstacle = daño obstaculo
        -- 1. Apply damage (or healing) to the robot
        g' =  aplicarDaño robotId dañoObstacle g
        -- 2. Apply rebound to the robot
        g'' = case (tipo_obstaculo $ info obstaculo) of
            Healing -> g'
            _ -> aplicarRebote robotId obstaculo g'
        -- 3. Change the state of the obstacle
        g''' = case (tipo_obstaculo $ info obstaculo) of
            -- Explosive: activates and changes appearance
            Explosive -> g''
                { obstaculos = sustituir obstaculo
                    (obstaculo 
                        { imagen = imagen_aux $ info obstaculo
                        , info = (info obstaculo) { activo = True }
                        })
                }
            -- Healing: disappears
            Healing -> g'' { obstaculos = elimina obstaculo }
            -- Solid and Harmful: remain unchanged
            _ -> g''
            where
                elimina obs = filter (/= obs) (obstaculos g'')
                sustituir old new = elimina old ++ [new]

-- aplicarDaño: Helper function that applies a damage amount to a robot given its ID.
aplicarDaño :: RobotId -> Float -> Game -> Game
aplicarDaño id daño g = g { robots = map dañarSiCoincide (robots g) }
    where
        dañarSiCoincide r =
            if index (info r) == id
            then r { info = (info r) { vida = min (vidaMax $ info r) ((vida $ info r) - daño) } }
            else r

-- aplicarRebote: Applies a rebound to a robot (inverts its angle).
aplicarRebote :: RobotId -> Entity a -> Game -> Game
aplicarRebote id objeto g = g { robots = map rebotarSiCoincide (robots g) }
  where 
    rebotarSiCoincide r
        | index (info r) == id && tipo (info r) /= Tower =
            let 
                vecRebote = sub (posicion r) (posicion objeto)
                puntosFinales = (\p -> sumVec p (pure (*0.5) <*> vecRebote)) <$> (puntos r)
                enPantalla = all (\p -> isInBounds p (size g)) puntosFinales
                rFinal = r { puntos = puntosFinales }
            in
                if enPantalla then rFinal else r

        | otherwise = r


-- rebotarSiTocaBorde: Function that inverts the robot's velocity if any of its points touch the boundary.
rebotarSiTocaBorde :: Game -> Robot -> Robot
rebotarSiTocaBorde game robot =
    if tiempoRebote (info robot) > 0.0 then
        robot
    -- If not stunned, check collision with the boundary:
    else
        let
            (Coords (sx, sy)) = size game
            (limiteX, limiteY) = (sx / 2, sy / 2)
            listaPuntos = puntos robot 

            tocaBordeVertical   = any (\(Coords (px, _)) -> px <= -limiteX || px >= limiteX) listaPuntos
            tocaBordeHorizontal = any (\(Coords (_, py)) -> py <= -limiteY || py >= limiteY) listaPuntos
        in
            if not (tocaBordeHorizontal || tocaBordeVertical)
            then robot -- If it doesn't touch anything, return the robot as is
            else
                if tipo (info robot) == Tower then
                    robot { velocidad = 0 }
                else
                    let
                        a = angulo robot
                        Coords (rx,ry) = posicion robot
                        signo n = n / (abs n)
                        (newA, vecRebote) = case (tocaBordeHorizontal, tocaBordeVertical) of
                            (True, False)  -> (-a, Coords (0, -5*signo ry))                   -- hits top/bottom: reflect on Y
                            (False, True)  -> (pi - a, Coords (-5*signo rx, 0))               -- hits left/right: reflect on X
                            _              -> (a + pi, Coords (-5*signo rx, -5*signo ry))     -- corner: full turn
                        robotGirado = updateRobotVelocity robot (velocidad robot) (normalizarAngulo newA)
                        puntosFinales = (\p -> sumVec p vecRebote) <$> (puntos robot)
                    in
                        robotGirado 
                            { puntos = puntosFinales
                            , info = (info robot) { tiempoRebote = 0.5 } -- 0.5 s "stun"
                            }
