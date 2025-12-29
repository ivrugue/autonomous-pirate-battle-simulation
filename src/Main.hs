-- Important note: for correct execution, Main must be loaded from the 'src' directory

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import System.Random
import HAUS.Types
import HAUS.Memory
import HAUS.Entities
import HAUS.Behavior
import HAUS.Game
import HAUS.Creators
import HAUS.Renderer
import HAUS.Spawner
import HAUS.Tournaments
import HAUS.Stats


-- buclePrincipal: Function that manages the main game loop.
{-  1. Robots scan the environment and decide their actions.
    2. Actions are applied (movement, shooting).
    3. All entities are moved (physics).
    4. Resulting collisions are detected.
    5. Collision events are handled (damage, rebound).
    6. Dead robots and other entities are removed.
    7. New Explosive or Healing items are added if enough time has passed.
-}

buclePrincipal :: Float -> StdGen -> Game -> (Game, StdGen)
buclePrincipal dt g estado =
    ((actualizarSpawns dt g) .                  -- Step 7
    limpiarEntities .                           -- Step 6
    gestionarEventosColision .                  -- Step 5
    checkCollisions .                           -- Step 4
    (actualizarPosicionesEntitiesMoviles dt) .  -- Step 3
    aplicarAcciones) estado                     -- Steps 1 and 2


-- main: Function that launches the game.
main :: IO ()
main = do
    -- Initializes a random generator
    g_inicial <- getStdGen
    -- Loads configuration data from file into a Memory
    let file = "../config/config.txt"
    putStrLn $ "Reading configuration from " ++ file ++ " ..."
    config <- leeTorneo file
    putStrLn $ "Configuration read successfully"
    -- Load all images.
    assets <- cargarAssets allAssets
    -- Set the number of tournaments
    let numTorneos = getInt config "Number" 3

    -- Get the base game template
    let estadoJuegoInicial = if numTorneos > 0 then Start else NoGame
    let estadoCreado = creaMundo config numTorneos g_inicial []
    let estadisticasInicial = if numTorneos > 0 then stats estadoCreado else []
    let initialWorldState = estadoCreado { estado_juego = estadoJuegoInicial, stats = estadisticasInicial }
    let tiposRobots = map (\r -> (index $ info r, tipo $ info r)) $ robots $ game initialWorldState

    -- Window limits
    let (winW, winH) = desCoords $ size $ game initialWorldState
    let window = InWindow "Pirate Battle" (round winW, round winH) (100, 100)
        
    playIO window (makeColorI 29 138 236 255) 60 initialWorldState (renderIO assets) handleEventIO (updateIO tiposRobots)
        where
            -- handleEvent: Function that handles keyboard events.
            handleEventIO :: Event -> WorldState -> IO WorldState
            handleEventIO evt estado = return $ handleEvent evt estado

            handleEvent :: Event -> WorldState -> WorldState
            handleEvent evt estadoMundo = case (evt, estado_juego estadoMundo) of
                -- Press Space in Start -> start and reset timer
                (EventKey (SpecialKey KeySpace) Down _ _, Start) ->
                    estadoMundo {estado_juego = Playing, tiempo = 0}
                -- Press P while Playing -> pause
                (EventKey (Char 'p') Down _ _, Playing) ->
                    estadoMundo {estado_juego = Paused}
                -- Press P while Paused -> resume
                (EventKey (Char 'p') Down _ _, Paused) ->
                    estadoMundo {estado_juego = Playing}
                -- Default: do not change the state
                _ -> estadoMundo

            -- update: Function that updates the game state each frame.
            -- It only calls buclePrincipal and increments the timer if the state is Playing. Checks if the game has ended.
            updateIO :: [(RobotId, TipoRobot)] -> Float -> WorldState -> IO WorldState
            updateIO tiposRobots dt estado = do
                let newEstado = update dt estado
                case (estado_juego newEstado, stats_escritas estado) of
                    -- If there are no more tournaments, save statistics
                    (NoGame, False) -> do
                        let file = "../out/stats.txt"
                        putStrLn $ "Saving stats in " ++ file ++ " ..."
                        guardarEstadisticas file (stats newEstado) tiposRobots
                        putStrLn $ "Stats saved successfully"
                        return $ newEstado {stats_escritas = True}
                    _ -> return newEstado

            update :: Float -> WorldState -> WorldState
            update dt estadoMundo = case estado_juego estadoMundo of
                Playing ->
                    let (estadoGameActualizado, g2) = buclePrincipal dt (gen estadoMundo) (game estadoMundo)
                        robotsVivos = filter isRobotAlive (robots estadoGameActualizado)
                        explosionesActivas = explosiones estadoGameActualizado
                        tiempo' = (tiempo estadoMundo) + dt
                        stats' = actualizaEstadisticasActual (stats estadoMundo) estadoGameActualizado tiempo' 
                        tiempoMax = getFloat (config estadoMundo) "Max duration" 120
                        esperaFin = 3
                        update_base = estadoMundo {tiempo = tiempo', gen = g2, stats = stats'}
                    in
                        -- If the maximum time is reached, end the tournament
                        if tiempo' >= tiempoMax
                            then update_base 
                                { estado_juego = Finished Nothing esperaFin
                                , game = estadoGameActualizado { proyectiles = [] }
                                }
                        -- If robots run out of life, end the tournament and store the winner if there is one
                        else if countActiveRobots estadoGameActualizado <= 1 && null explosionesActivas
                            then case robotsVivos of
                                    [ganador] -> update_base 
                                        { estado_juego = Finished (Just (index (info ganador))) esperaFin
                                        , game = estadoGameActualizado { proyectiles = [] }
                                        }
                                    [] -> update_base 
                                        { estado_juego = Finished Nothing esperaFin
                                        , game = estadoGameActualizado { proyectiles = [] }
                                        }
                        else update_base {game = estadoGameActualizado}

                -- Stay on the end screen for a few seconds and start the next tournament
                Finished ganador espera
                    | espera > 0 -> estadoMundo {estado_juego = Finished ganador (espera-dt)}
                    | otherwise -> siguienteTorneo estadoMundo
                    
                _ -> estadoMundo
