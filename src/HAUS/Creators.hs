{-# LANGUAGE FlexibleInstances #-}
-- Needed so that instance allows writing an alias (Robot/Projectile/Explosion/Obstacle).

module HAUS.Creators where

import Data.Default
import qualified Data.Map as Map
import HAUS.Types 
import HAUS.Entities
import HAUS.Physics
import HAUS.Behavior

framesExplosion = (\x -> "explosion"++(show x)++".png") <$> [1..16]

instance Default Robot where    -- Default Robot (of type Basic).
    def = Entity 
        { imagen = "barco_base.png"
        , puntos = [Coords (-30,15), Coords (30,15), Coords (30,-15), Coords (-30,-15)]
        , angulo = 0
        , velocidad = 0
        , daño = 2    -- each frame touching another robot, it will remove 2 life points
        , info = RobotInfo
            { index = 0
            , tipo = Basic
            , vida = 100
            , vidaMax = 100
            , paso_giro = deg2rad 5
            , aceleracion_avance = 10
            , velMax = 50
            , cooldown = 2
            , imagen_torreta = "cañon.png"
            , radio_disparo = 800
            , angulo_torreta = 0
            , radar = 500       
            , memoria = Map.empty
            , enfriamientoActual = 2
            , tiempoRebote = 0.0
            -- The base model that will never change (must be identical to the points above).
            , puntos_base = [Coords (-30,15), Coords (30,15), Coords (30,-15), Coords (-30,-15)]
            }
    }

instance Default Projectile where   -- Default Projectile.
    def = Entity 
        { imagen = "bala.png"
        , puntos = [Coords (-5,5), Coords (5,5), Coords (5,-5), Coords (-5,-5)]
        , angulo = 0
        , velocidad = 300
        , daño = 10
        , info = ProjectileInfo
            { alcance = 800
            , robot_lanzador = 0
            , posicion_origen = Coords (0,0)
            }
    }

instance Default Explosion where   -- Default Explosion (large).
    def = Entity 
        { imagen = head framesExplosion
        , puntos = [Coords (-30,30), Coords (30,30), Coords (30,-30), Coords (-30,-30)]
        , angulo = 0
        , velocidad = 0
        , daño = 1.5  -- each frame touching the large explosion removes 1.5 life points (max 24)
        , info = ExplosionInfo
            { proximas_imagenes = tail framesExplosion
            , num_frames = length framesExplosion
            }
    }

instance Default Obstacle where   -- Default Obstacle (solid).
    def = Entity 
        { imagen = "isla.png"
        , puntos = [Coords (-30,20), Coords (30,20), Coords (30,-20), Coords (-30,-20)]
        , angulo = 0
        , velocidad = 0
        , daño = 0
        , info = ObstacleInfo
            { tipo_obstaculo = Solid
            , activo = True
            , tiempo_espera = 0
            , radio_daño = 0
            , daño_area = 0
            , imagen_aux = ""
            }
    }

-- Using the Default instances, we name specific functions for each entity type.
defR = def :: Robot
defP = def :: Projectile
defE = def :: Explosion
defO = def :: Obstacle


-- FUNCTIONS TO CREATE ENTITIES
-- Based on the predefined parameters in defR, defP, and defE, concrete robots, projectiles, and explosions are created.
crearRobotBasic :: Position -> Robot
crearRobotBasic pos = defR
        { puntos = desplazarAPosicion (puntos defR) pos
        }

crearRobotTorre :: Position -> Robot
crearRobotTorre pos = defR
        { imagen = "torre.png"
        , puntos = desplazarAPosicion puntosTorre pos
        , info = (info defR)
            { tipo = Tower
            , vida = vidaTorre
            , vidaMax = vidaTorre
            , velMax = 0
            , radar = 800
            , cooldown = cooldownTorre
            , enfriamientoActual = cooldownTorre
            , puntos_base = puntosTorre
            }
        }
    where
        puntosTorre = [Coords (-20,20), Coords (20,20), Coords (20,-20), Coords (-20,-20)]
        vidaTorre = 120
        cooldownTorre = 1.5

crearRobotKamikaze :: Position -> Robot
crearRobotKamikaze pos = defR
        { imagen = "barco_suicida.png"
        , daño = 1
        , info = (info defR)
            { tipo = Kamikaze
            , vida = vidaKamikaze
            , vidaMax = vidaKamikaze
            , velMax = 120
            , cooldown = cooldownKamikaze
            , enfriamientoActual = cooldownKamikaze
            , radio_disparo = 400
            , puntos_base = puntosKamikaze
            }
        }
    where
        puntosKamikaze = [Coords (-20,15), Coords (20,15), Coords (20,-15), Coords (-20,-15)]
        vidaKamikaze = 80
        cooldownKamikaze = 1

crearProjectile :: Robot -> Projectile
crearProjectile robotLanzador = defP
        { puntos = desplazarAPosicion (puntos defP) posicionInicial -- Positions the points at the firing location.
        , angulo = angulo_torreta $ info robotLanzador
        , info = (info defP)
            { alcance = radio_disparo $ info robotLanzador
            , robot_lanzador = index $ info robotLanzador
            , posicion_origen = posicionInicial
            }
        }
    where
        posicionInicial = posicion robotLanzador

crearExplosionGrande :: Entity a -> Explosion  
crearExplosionGrande ent = defE
    { puntos = desplazarAPosicion (puntos defE) posicionInicial
    }
        where 
            posicionInicial = posicion ent

crearExplosionPequeña :: Entity a -> Explosion
crearExplosionPequeña ent = defE
    { imagen = head framesExplosion
    , puntos = desplazarAPosicion puntos_base posicionInicial
    , daño = 0
    , info = ExplosionInfo
            { proximas_imagenes = tail framesExplosion
            , num_frames = length framesExplosion
            }
    }
        where 
            puntos_base = [Coords (-10,10), Coords (10,10), Coords (10,-10), Coords (-10,-10)]
            posicionInicial = posicion ent

crearObstacleSolid :: Position -> Obstacle
crearObstacleSolid pos = defO
    { puntos = desplazarAPosicion (puntos defO) pos
    }

crearObstacleHarmful :: Position -> Obstacle
crearObstacleHarmful pos = defO
    { imagen = "piedras.png"
    , puntos = desplazarAPosicion puntos_base pos
    , daño = 5
    , info = (info defO)
        { tipo_obstaculo = Harmful
        }
    }
        where puntos_base = [Coords (-20,20), Coords (20,20), Coords (20,-20), Coords (-20,-20)]

crearObstacleExplosive :: Position -> Obstacle
crearObstacleExplosive pos = defO
    { imagen = "mina.png"
    , puntos = desplazarAPosicion puntos_base pos
    , info = (info defO)
        { tipo_obstaculo = Explosive
        , activo = False
        , tiempo_espera = 3
        , radio_daño = 80
        , daño_area = 20
        , imagen_aux = "mina_activa.png"
        }
    }
        where puntos_base = [Coords (-12,12), Coords (12,12), Coords (12,-12), Coords (-12,-12)]


crearObstacleHealing :: Position -> Obstacle
crearObstacleHealing pos = defO
    { imagen = "cofre.png"
    , puntos = desplazarAPosicion puntos_base pos
    , daño = -20    -- the robot receives -20 damage = recovers 20 life points
    , info = (info defO)
        { tipo_obstaculo = Healing
        , imagen_aux = "cofre_abierto.png"
        }
    }
        where puntos_base = [Coords (-15,7), Coords (15,7), Coords (15,-7), Coords (-15,-7)]
