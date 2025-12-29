module HAUS.Renderer where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Graphics.Gloss.Juicy (loadJuicy)

import HAUS.Types
import HAUS.Physics
import HAUS.Entities
import HAUS.Behavior
import HAUS.Game
import HAUS.Creators

-- Assets: type for loading images.
-- Inserting them in a dictionary helps avoid loading them every frame, since once stored we can just fetch them from there.
type Assets = Map.Map String Picture

-- cargarImagen: Function to load an image.
cargarImagen :: FilePath -> IO (Maybe Picture)
cargarImagen path = do
    dir <- getCurrentDirectory
    let fullPath = dir </> ".." </> "assets" </> path
    putStrLn $ "Loading: " ++ fullPath
    loadJuicy fullPath

-- cargarAssets: Function to load all necessary images.
cargarAssets :: [String] -> IO Assets
cargarAssets nombres = do
    mAssets <- mapM cargarImagen nombres
    -- Filter the Nothings and map the Just values to the original path.
    let pares = zip nombres mAssets
    return $ Map.fromList [(name, p) | (name , Just p) <- pares] 

-- List of all images we need to load.
allAssets :: [String]
allAssets =
    [ "bala.png"
    , "barco_base.png"
    , "barco_suicida.png"
    , "cañon.png"
    , "mar.png"
    , "torre.png"
    , "isla.png"
    , "piedras.png"
    , "mina.png"
    , "mina_activa.png"
    , "cofre.png"
    , "cofre_abierto.png"
    ] ++ framesExplosion

-- ratioImagen: Returns the scale to reduce the image to fit the entity's real size
ratioImagen :: [Position] -> Float -> Float
ratioImagen ps largoImagen = 2 * (radioMaxPuntos ps) / largoImagen

-- Returns the scale to adjust the image to the window size
ratioImagenVentana :: (Float,Float) -> (Float,Float) -> Float
ratioImagenVentana (winW, winH) (anchoImg, altoImg) =
    let escalaX = winW / anchoImg
        escalaY = winH / altoImg
    in max escalaX escalaY


-- render: Function that draws the game according to the WorldState.
-- If the images exist it will show them, otherwise it will display the default HAUS5 drawing to avoid errors.
renderIO :: Assets -> WorldState -> IO Picture
renderIO assets estado = return $ render assets estado

render :: Assets -> WorldState -> Picture
render assets estado = Pictures (elementosJuego ++ [mensajeEstado, contadorTiempo, contadoresJuego, instPausa])
    where
        estadoGame = game estado
        -- Common elements (robots, projectiles, explosions).
        renderRobots = creaRobot assets <$> (robots estadoGame)
        renderProjectiles = creaProjectile assets <$> (proyectiles estadoGame)
        renderExplosions = creaExplosion assets <$> (explosiones estadoGame)
        renderObstacles = creaObstacle assets <$> (obstaculos estadoGame)

        (winW, winH) = desCoords $ size estadoGame

        -- Background
        escalaFondo = ratioImagenVentana (winW, winH) (1120,928)
        fondoPicture = case Map.lookup (fondo estadoGame) assets of
            Just img -> Scale escalaFondo escalaFondo $ img
            -- If the image fails, no background is created and only a blue background will be visible (in Main's play function).
            Nothing -> Blank
            
        elementosJuego = fondoPicture : renderObstacles ++ renderRobots ++ renderProjectiles ++ renderExplosions

        -- Text according to game state.
        mensajeEstadoFino = case (estado_juego estado) of
            Start -> Translate (-210) 0 $ Scale 0.3 0.3 $ Color (dark red) $ Text "Press SPACE to start"
            Finished (Just ganadorId) _ -> Translate (-225) 0 $ Scale 0.3 0.3 $ Color blue $ Text ("Robot " ++ show ganadorId ++ " is the winner!")
            Finished Nothing _ -> Translate (-90) 0 $ Scale 0.3 0.3 $ Color blue $ Text "It's a tie!" -- If no winner.
            Paused -> Translate (-80) 0 $ Scale 0.3 0.3 $ Color white $ Text "Paused"
            NoGame -> Translate (-195) 0 $ Scale 0.3 0.3 $ Text "End of tournaments"
            _ -> Blank -- Does not show message while playing.
        -- Shows the message overlapped to make it appear bolder
        mensajeEstado = Pictures [mensajeEstadoFino, Translate 1 0 $ mensajeEstadoFino]

        -- Helper function to display status texts.
        creaTexto movX movY txt = Translate (-winW/2 + 20 + movX) (winH/2 - 30 + movY) -- Text position in the window.
                                $ Scale (0.15) (0.15)
                                $ Text txt
                            
        -- Time counter.
        strTiempo = "Time: " ++ show (round (tiempo estado) :: Int)
        contadorTiempo = creaTexto 0 0 strTiempo

        -- Counters for robots, projectiles and explosions.
        strRobots = "Robots: " ++ show (countActiveRobots estadoGame)
        strProjectiles = "Projectiles: " ++ show (length $ proyectiles estadoGame)
        strExplosions = "Explosions: " ++ show (length $ explosiones estadoGame)
        strCombinado = strRobots ++ " | " ++ strProjectiles ++ " | " ++ strExplosions
        contadoresJuego = creaTexto 0 (-30) strCombinado
        
        -- Pause instruction
        strPausa = "Pause [P]"
        instPausa = creaTexto (winW - 130) 0 strPausa

        -- creaRobot: Draws a robot, including its ID.
        creaRobot assets robot = Pictures [cuerpo, torreta, barra, textoID]
            where
                -- hitbox = Color red $ colocaTorreta $ Translate 10 0 $ Line $ desCoords <$> puntos_torreta    -- (Uncomment only for debug)
                (rx,ry) = desCoords $ posicion robot
                puntos_torreta = [Coords (-15,5), Coords (15,5), Coords (15,-5), Coords (-15,-5)]

                -- Parameters according to robot type.
                colorTorreta = greyN 0.2
                (colorCuerpo,largoImagen) = case (tipo $ info robot) of
                    Basic -> (makeColorI 208 124 81 255, 560)  -- Image width is 700, but 560 is the width without front and back parts.
                    Tower  -> (makeColorI 204 149 78 255, 750)
                    _      -> (greyN 0.3, 560)

                -- Helper to calculate body and turret scales.
                escalaCuerpo = ratioImagen (puntos robot) largoImagen
                escalaTorreta = ratioImagen puntos_torreta 500
            
                -- Calculate the robot's body.
                cuerpo = case Map.lookup (imagen robot) assets of
                    Just img -> Translate rx ry
                                $ Rotate (-(rad2deg $ angulo robot))
                                $ Scale escalaCuerpo escalaCuerpo
                                $ img
                    Nothing -> Color colorCuerpo 
                                $ Polygon $ desCoords <$> puntos robot

                -- Calculate the turret.
                torreta = case Map.lookup (imagen_torreta $ info robot) assets of
                    Just img -> colocaTorreta 
                                $ Translate 10 0
                                $ Scale escalaTorreta escalaTorreta
                                $ img
                    Nothing -> Color colorTorreta
                                $ colocaTorreta 
                                $ Translate 10 0
                                $ Polygon $ desCoords <$> puntos_torreta
                                
                colocaTorreta pic = Translate rx ry
                                    $ Rotate (-(rad2deg $ angulo_torreta $ info robot))
                                    $ pic
            
                barra = creaBarra 60 5
                -- creaBarra: Draws the health bar.
                creaBarra w h = Translate x (y + 10 + radioMax robot)
                            $ Pictures [base_barra,vida_barra,borde_barra]
                    where
                        (x,y) = desCoords $ posicion robot
                        base_barra = Color white
                                $ rectangleSolid w h
                        borde_barra = Color black
                                    $ rectangleWire w h

                        vMax = vidaMax $ info robot
                        v = vida $ info robot
                        ancho_vida = v * w / vMax
                        -- Colors according to remaining health: between green (max) and red (0).
                        colorBarra 
                            | v >= vMax/2 = makeColorI (round $ (vMax-v)*255 / (vMax/2)) 255 0 255
                            | otherwise = makeColorI 255 (round $ v*255 / (vMax/2)) 0 255
                        vida_barra = Color colorBarra
                                $ Translate (-(w/2) + ancho_vida/2) 0
                                $ rectangleSolid ancho_vida h

                -- textoID: Position the text above the health bar.  
                textoID = Translate (rx - 25) (ry + 20 + radioMax robot) 
                            $ Scale 0.1 0.1
                            $ Color black
                            $ Text ("Robot " ++ show (index (info robot)))

        -- creaProjectile: Draws a projectile.
        creaProjectile assets proyectil =
            -- Looks up the image associated with the projectile in the map.
            case Map.lookup (imagen proyectil) assets of 
                -- If found, displays the image
                Just img -> Translate x y $ Scale escalaProjectile escalaProjectile $ Rotate (-(rad2deg $ angulo proyectil)) $ img
                -- If not found, draws a black circle
                Nothing -> Color black $ Translate x y $ circleSolid (radioMax proyectil) 
            where
                (x,y) = desCoords $ posicion proyectil
                escalaProjectile = ratioImagen (puntos proyectil) 768
            

        -- creaExplosion: Draws an explosion.
        creaExplosion assets explosion = 
            -- Looks up the image associated with the explosion in the assets map.
            case Map.lookup (imagen explosion) assets of
                -- If found, displays the image
                Just img -> Translate x y $ Scale escalaExplosion escalaExplosion $ img
                -- If not found, draws two figures: the smoke and the explosion base.
                Nothing -> Pictures [humo, base_explosion]
            where
                (x,y) = desCoords $ posicion explosion
                escalaExplosion = ratioImagen (puntos explosion) 700    -- The image width is 1023, but we want the outer edges to not cause damage.

                -- Animation for the explosion drawing (if image is missing).
                base_explosion = Color orange
                                $ Translate x y
                                $ circleSolid (radioMax explosion)
                humo = Color (greyN 0.5)
                    $ Translate x y
                    $ Scale escala_humo escala_humo
                    $ circleSolid (radioMax explosion)

                numFrames = num_frames $ info explosion -- Total number of frames (animation stages) of the explosion.
                numProximas = length $ proximas_imagenes $ info explosion -- Number of future images (remaining) in the animation.
                escala_humo = 1.0 + (fromIntegral (numFrames - numProximas - 1) / fromIntegral numFrames) -- Calculates a progressive scale for the smoke, it grows as frames advance.

        -- creaObstacle: Draws an obstacle.
        creaObstacle assets obstaculo = Pictures [area, cuerpo]
            -- Looks up the image associated with the obstacle in the map.
            where
                -- hitbox = Color red $ Line $ desCoords <$> puntos obstaculo

                (x,y) = desCoords $ posicion obstaculo
                escalaObstacle = ratioImagen (puntos obstaculo) largoImagen

                -- Parameters according to obstacle type.
                (colorObstacle,largoImagen) = case (tipo_obstaculo $ info obstaculo) of
                    Solid    -> (makeColorI 232 195 117 255, 520) 
                    Harmful    -> (makeColorI 120 120 120 255, 530)
                    Explosive -> (black, 600)
                    _         -> (makeColorI 139 69 19 255, 525)

                imagen_obstaculo
                    | tipo_obstaculo (info obstaculo) == Healing && any (\r -> distanceBetween (posicion r) (posicion obstaculo) < 50) (robots estadoGame) = 
                        imagen_aux $ info obstaculo     -- The chest will appear open when a robot is nearby
                    | otherwise = imagen obstaculo
                cuerpo = case Map.lookup imagen_obstaculo assets of 
                    -- If found, displays the image
                    Just img -> Translate x y 
                            $ Scale escalaObstacle escalaObstacle 
                            $ Rotate (-(rad2deg $ angulo obstaculo)) 
                            $ img
                    -- If not found, draws a polygon with the corresponding color
                    Nothing -> Color colorObstacle 
                            $ Polygon $ desCoords <$> puntos obstaculo
                            
                -- Check if the obstacle is Explosive to draw the damage area.
                area
                    | tipo_obstaculo (info obstaculo) == Explosive && activo (info obstaculo) =
                            Color color_area 
                          $ Translate x y
                          $ circleSolid (radio_daño $ info obstaculo)
                    | otherwise = Blank
                    where color_area
                            | tiempo_espera (info obstaculo) < 0.5 = makeColorI 255 0 0 100
                            | otherwise = makeColorI 255 0 0 25
