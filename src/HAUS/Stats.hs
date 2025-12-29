module HAUS.Stats where

import qualified Data.Map as Map
import Data.List (intercalate)
import HAUS.Types 
import HAUS.Memory
import HAUS.Entities
import HAUS.Physics
import HAUS.Collisions
import HAUS.Behavior
import HAUS.AI
import HAUS.Creators


data Stats = Stats
    { aciertos_por_robot :: Map.Map RobotId Integer -- maps each Robot to the number of projectiles hit on other Robots
    , tiempo_vivo :: Map.Map RobotId Float          -- maps each Robot to the time it has been alive during the tournament
    , ganador :: [RobotId]                          -- list of robots alive at the end of the game
    }


iniciaEstadisticas :: Game -> Stats
iniciaEstadisticas game = Stats
    { aciertos_por_robot = Map.fromList [(index (info r), 0) | r <- robots game]
    , tiempo_vivo = Map.fromList [(index (info r), 0) | r <- robots game]
    , ganador = map (\r -> index $ info r) $ robots game
    }

actualizaEstadisticasActual :: [Stats] -> Game -> Float -> [Stats]
actualizaEstadisticasActual [] _ _ = []
actualizaEstadisticasActual [s] g tiempo = [actualizaEstadisticas s g tiempo]
actualizaEstadisticasActual xs g tiempo = (init xs) ++ [actualizaEstadisticas (last xs) g tiempo]

actualizaEstadisticas :: Stats -> Game -> Float -> Stats
actualizaEstadisticas stats g tiempo = stats 
    { aciertos_por_robot = aciertosActualizados
    , tiempo_vivo = tiemposActualizados
    , ganador = robotsVivos
    }
    where
        robotsAcertantes = [(robot_lanzador $ info p) | CollisionRobotProjectile _ p <- eventosColisionActivos g] -- ids of robots whose projectile hit another robot
        recuentoAcertantes = Map.fromListWith (+) [(r, 1) | r <- robotsAcertantes]  -- counts projectiles hit at this moment
        aciertosActualizados = Map.unionWith (+) (aciertos_por_robot stats) recuentoAcertantes  -- update hit count for each robot

        robotsVivos = map (\r -> index $ info r) $ filter isRobotAlive $ robots g  -- ids of robots still alive
        tiemposVivos = Map.fromList [(r, tiempo) | r <- robotsVivos]    -- maps each alive robot to its current time
        tiemposActualizados = Map.union tiemposVivos (tiempo_vivo stats)    -- update time only for alive robots

guardarEstadisticas :: FilePath -> [Stats] -> [(RobotId, TipoRobot)] -> IO ()
guardarEstadisticas file stats tiposRobots = do
    let contenido = if null stats
                    then "No tournaments"
                    else unlines [estadisticasToString est n tiposRobots | (est, n) <- zip stats [1..(length stats)]] ++ estadisticasGlobalesToString stats tiposRobots
    writeFile file contenido


estadisticasToString :: Stats -> Int -> [(RobotId, TipoRobot)] -> String
estadisticasToString est n tiposRobots = formateaTitulo ("TOURNAMENT " ++ show n) ++
                             (           unlines $ map mostrarRobot idsRobots) ++ "\n" 
    where
        idsRobots = Map.keys $ tiempo_vivo est
        mostrarRobot r = "\n> Robot " ++ show r ++ " (" ++ show (tipoPorId tiposRobots r) ++ ")" ++ esGanador ++
                         "\n\tTime alive: " ++ show (redondea 2 $ tiempoVivo r est) ++ "s (" ++ show (redondea 2 $ porcTiempoVivo r est) ++ "%)" ++
                         "\n\tProjectiles hit: " ++ show (aciertos r est)
            where esGanador = if r `elem` ganador est then " - WINNER" else ""

estadisticasGlobalesToString :: [Stats] -> [(RobotId, TipoRobot)] -> String
estadisticasGlobalesToString xs tiposRobots = formateaTitulo "GLOBAL STATISTICS" ++
                                              (unlines $ map mostrarRobot datosPorRobot) ++ mostrarGlobal ++ "\n"
    where
        idsRobots = Map.keys $ tiempo_vivo (head xs)
        datosPorRobot = map (\r -> (r, listaPorcTiempos r, listaAciertos r, ganados r)) idsRobots

        mostrarRobot (r, t, a, g) = "\n> Robot " ++ show r ++ " (" ++ show (tipoPorId tiposRobots r) ++ ")" ++
                                    "\n\tAverage time alive per tournament: " ++ show (redondea 2 $ mediaTiempos t) ++ "%" ++
                                    "\n\tLongest time alive in a tournament: " ++ show (redondea 2 $ maximum t) ++ "%" ++
                                    "\n\tAverage projectiles hit per tournament: " ++ show (redondea 2 $ mediaAciertos a) ++
                                    "\n\tMost projectiles hit in a tournament: " ++ show (maximum a) ++
                                    "\n\tTournaments won: " ++ show g
        mostrarGlobal = "\n>> TOTAL" ++
                        "\n\tAverage time alive per robot and tournament: " ++ show (redondea 2 $ mediaTiempos todosPorcTiempos) ++ "%" ++
                        "\n\tLongest time alive by a robot in a tournament: " ++ show (redondea 2 $ maximum todosPorcTiempos) ++ "%" ++
                        "\n\tAverage projectiles hit per robot and tournament: " ++ show (redondea 2 $ mediaAciertos todosAciertos) ++
                        "\n\tMost projectiles hit by a robot in a tournament: " ++ show (maximum todosAciertos) ++
                        "\n\tRobot with most wins: " ++ (intercalate ", " (map (\r -> "Robot " ++ show r) ganadores))
        
        -- Extracts the data for a single robot
        listaPorcTiempos r = map (\est -> porcTiempoVivo r est) xs
        listaAciertos r = map (\est -> aciertos r est) xs
        ganados r = length $ filter (\e -> r `elem` (ganador e)) xs

        -- Extracts all global data
        extraeGlobal f = concat $ map f datosPorRobot
        todosPorcTiempos = extraeGlobal (\(_,t,_,_) -> t) 
        todosAciertos = extraeGlobal (\(_,_,a,_) -> a)
        maxVictorias = maximum $ map (\(_,_,_,v) -> v) datosPorRobot
        ganadores = [r | (r,_,_,v) <- datosPorRobot, v == maxVictorias]

        mediaTiempos ts = sum ts / fromIntegral (length ts)
        mediaAciertos as = fromIntegral (sum as) / fromIntegral (length as)
            

-- Auxiliary functions to calculate statistics
tiempoVivo :: RobotId -> Stats -> Float        
tiempoVivo r est = Map.findWithDefault 0 r (tiempo_vivo est)

tiempoTorneo :: Stats -> Float
tiempoTorneo est = maximum (Map.elems (tiempo_vivo est))

porcTiempoVivo :: RobotId -> Stats -> Float    
porcTiempoVivo r est = ((tiempoVivo r est) / (tiempoTorneo est)) * 100

aciertos :: RobotId -> Stats -> Integer
aciertos r est = Map.findWithDefault 0 r (aciertos_por_robot est)

-- Auxiliary functions to format text
formateaTitulo :: String -> String
formateaTitulo texto = linea ++ titulo ++ linea
    where 
        linea = replicate (30 + length texto) '=' ++ "\n"
        titulo = replicate 15 ' ' ++ texto ++ "\n"

tipoPorId :: [(RobotId, TipoRobot)] -> RobotId -> TipoRobot
tipoPorId ids_tipos r = head [t | (i,t) <- ids_tipos, i == r]
