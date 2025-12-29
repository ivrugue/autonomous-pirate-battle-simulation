module HAUS.Tournaments where

import qualified Data.Map as Map
import Control.Exception (catch, SomeException, evaluate)
import System.Random
import HAUS.Types
import HAUS.Memory
import HAUS.Entities
import HAUS.Game
import HAUS.Creators
import HAUS.Spawner
import HAUS.Stats

-- creaMundo: Function that creates an initial WorldState.
creaMundo :: Memory -> Int -> StdGen -> [Stats] -> WorldState
creaMundo config torneosRestantes g1 stats = WorldState 
    { estado_juego = Playing
    , game = gameAleatorio
    , tiempo = 0
    , config = config
    , torneos_restantes = torneosRestantes
    , gen = g2
    , stats = stats++[statsInicial]
    , stats_escritas = False
    }
    where
        game_base = Game
            { robots = asignaIndices (robotsBasic ++ robotsTorre ++ robotsKamikaze)
            , proyectiles = []
            , explosiones = []
            , obstaculos = obstaculosSolid ++ obstaculosHarmful
            , size = getSize config "Area" (Coords (800, 600))
            , fondo = "mar.png"
            , eventosColisionActivos = []
            , tiempoExplosive = tExplosive
            , intervaloExplosive = tExplosive
            , tiempoHealing = tHealing
            , intervaloHealing = tHealing
            }

        listaEntities crear n = map crear $ replicate n $ Coords (0,0)

        robotsBasic = listaEntities crearRobotBasic $ getInt config "Basic" 3
        robotsTorre = listaEntities crearRobotTorre $ getInt config "Tower" 1
        robotsKamikaze = listaEntities crearRobotKamikaze $ getInt config "Kamikaze" 1
        -- Assign unique IDs to robots.
        asignaIndices rs = [r { info = (info r) { index = i } } | (i, r) <- zip [1..] rs]

        obstaculosSolid = listaEntities crearObstacleSolid $ getInt config "Solid" 3
        obstaculosHarmful = listaEntities crearObstacleHarmful $ getInt config "Harmful" 2
        tExplosive = getFloat config "Explosive" 6
        tHealing = getFloat config "Healing" 4

        -- Generate the random game state.
        (gameAleatorio, g2) = crearEntitiesAleatorias g1 game_base

        statsInicial = iniciaEstadisticas gameAleatorio


-- siguienteTorneo: Resets the game and starts a new tournament
siguienteTorneo :: WorldState -> WorldState
siguienteTorneo estado = nuevoEstado
    where 
        restantes = (torneos_restantes estado)-1
        nuevoEstado = if restantes > 0 
                      then creaMundo (config estado) restantes (gen estado) (stats estado) 
                      else estado {estado_juego = NoGame}


-- leeTorneo: Reads a file whose relevant lines have format "Field: Data" and stores the data in a Memory
leeTorneo :: FilePath -> IO Memory
leeTorneo file = do
    contenido <- catch (readFile file)
               (\err -> (putStrLn (show (err :: SomeException) ++ ". Running default game...") >> return ""))
    let datos = filter (\s -> elem ':' s) $ lines contenido -- ignore lines that do not meet the base format
    let tuplas = map formateaLinea datos
    mapM_ (evaluate . snd) tuplas   -- due to lazy evaluation, force evaluation to detect errors before running play
    return $ Map.fromList tuplas

-- AUXILIARY FUNCTIONS for leeTorneo
-- formateaDatos: Given a line "Field: Data", separates the field name from the data
formateaLinea :: String -> (String, ValorMemory)
formateaLinea texto = (nombre, dato)
    where
        (nombreConPar, datoStr) = break (==':') texto
        nombre = trim $ takeWhile (/='(') nombreConPar -- remove parentheses part
        dato = parseaDato nombre (trim $ tail datoStr)  -- remove ": " and transform

-- trim: Removes spaces at the beginning and end
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile (==' ')

-- parseaDato: Given a string containing a value, parses it according to the field if it matches the expected format
parseaDato :: String -> String -> ValorMemory
parseaDato campo dato
    | esFloat sx && esFloat sy && campo == "Area" = MemSize (Coords (read sx :: Float, read sy :: Float))                 -- FloatxFloat format
    | esFloat dato && (campo `elem` ["Explosive","Healing","Max duration"]) = MemFloat (read dato :: Float)               -- Float format
    | esInt dato && (campo `elem` ["Basic","Tower","Kamikaze","Solid","Harmful","Number"]) = MemInt (read dato :: Int)    -- Int format
    | otherwise = error $ "Unrecognized format for \"" ++ campo ++ ": " ++ dato ++ "\""                                   -- incorrect field or format
    where 
        esInt xs = (not $ null xs) && (all (\c -> c >= '0' && c <= '9') xs)
        esFloat xs = esInt (filter (/='.') xs)
                    && (length (filter (=='.') xs) <= 1)
                    && head xs /= '.'
                    && last xs /= '.'
        (n1,n2) = break (=='x') dato
        (sx,sy) = (trim n1, trim $ if null n2 then "" else tail n2)
