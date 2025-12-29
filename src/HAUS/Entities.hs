module HAUS.Entities where

import HAUS.Types
import HAUS.Memory

{- Entity type: Represents an entity in the game.
Its attributes are:
    imagen: Path to the entity image.
    puntos: Position of the 4 points of the entity on screen. From them, the entity position (centroid) is obtained.
    angulo: Direction the entity is facing.
    velocidad: (Scalar) speed of the entity. If positive it moves forward, if negative it moves backward.
    daño: Life points removed from a robot when the entity comes into contact with it.
    info: Contains additional attributes for each type of entity (will be of type RobotInfo, ProjectileInfo, etc).
-}
{- Note: The velocity vector is split into angulo (direction) and velocidad (magnitude and sign)
to make modifications to each property individually easier.
To obtain the vector, the function vecFromModuleAngle can be used. -}
data Entity a = Entity
  { imagen     :: String
  , puntos     :: [Position]
  , angulo     :: Radian
  , velocidad  :: Float
  , daño       :: Float
  , info       :: a
  } deriving (Show, Eq)

type Robot = Entity RobotInfo
type Projectile = Entity ProjectileInfo
type Explosion = Entity ExplosionInfo
type Obstacle = Entity ObstacleInfo


{- RobotInfo type: Represents the information of a robot that moves around the screen and fires projectiles.
Its attributes are:
    index: Represents the robot id, useful to identify the robot that fired a given projectile.
    tipo: Robot type according to the strategy it follows.
    vida: Represents the life points the robot currently has.
    vidaMax: Maximum life a robot can have.
    paso_giro: Angle by which the robot direction changes when turning once.
    aceleracion_avance: Amount by which the speed changes when it is modified once.
    velMax: Maximum speed the robot can reach.
    cooldown: Time that must pass before firing another projectile.
    imagen_torreta: Path to the turret image.
    enfriamientoActual: Remaining time before being able to fire another projectile after the last one.
    radio_disparo: Maximum distance projectiles from this robot can reach after being fired.
    angulo_torreta: Turret firing angle.
    radar: Robot detection radius.
    memoria: Map containing information about the entities detected by the robot.
    puntos_base: List of robot points centered at (0,0), never changes.
    tiempoRebote: Time the robot will move backwards after colliding with another robot or with the borders.
-}
type RobotId = Int
data RobotInfo = RobotInfo
  { index              :: RobotId
  , tipo               :: TipoRobot
  , vida               :: Float
  , vidaMax            :: Float
  , paso_giro          :: Radian
  , aceleracion_avance :: Float
  , velMax             :: Float
  , cooldown           :: Float
  , enfriamientoActual :: Float
  , imagen_torreta     :: String
  , radio_disparo      :: Distance
  , angulo_torreta     :: Radian
  , radar              :: Distance
  , memoria            :: Memory
  , puntos_base        :: [Position]
  , tiempoRebote       :: Float
  } deriving (Show, Eq)

{- There are three types of robots (strategies):
    Basic: Follows a priority list related to dodging threats or attacking nearby robots, among others.
    Tower: Does not move, only shoots.
    Kamikaze: Does not try to dodge projectiles, it simply attacks enemies.
-}
data TipoRobot = Basic | Tower | Kamikaze
    deriving (Show, Eq)


{- ProjectileInfo type: Represents the information of the projectiles fired by robots.
Its attributes are:
    alcance: Distance the projectile can travel from the position where it was fired.
    robot_lanzador: Id of the robot that fired the projectile (the projectile will not affect it on collision).
    posicion_origen: Position where the center of the projectile is located.
-}
data ProjectileInfo = ProjectileInfo
  { alcance          :: Distance
  , robot_lanzador   :: RobotId
  , posicion_origen  :: Position
  } deriving (Show, Eq)


{- ExplosionInfo type: Represents the information of the explosion that appears when a robot dies (large) or when it is shot (small).
Its attributes are:
    proximas_imagenes: List of paths to the next images that will be shown to display the explosion animation
                       (when the explosion appears it will continuously change until it disappears).
    num_frames: Total number of images sequenced in the explosion. Only for basic rendering.
-}
data ExplosionInfo = ExplosionInfo
  { proximas_imagenes  :: [String]
  , num_frames         :: Int
  } deriving (Show, Eq)


{- ObstacleInfo type: Represents the information of the obstacles that appear in the scenario.
Its attributes are:
    tipo_obstaculo: Type of obstacle according to the effect it produces.
    activo: True if the obstacle is active (for Explosive, if active the countdown will start).
    tiempo_espera: Time that must pass before the effect is applied.
    radio_daño: Radius in which the effect will be applied.
    daño_area: Life points removed from robots inside the radius when the effect is applied
               (different from 'daño', which only removes life on collision).
    imagen_aux: Path to the auxiliary image of the entity, if it changes when applying the effect.
-}
data ObstacleInfo = ObstacleInfo
    { tipo_obstaculo    :: TipoObstacle
    , activo            :: Bool
    , tiempo_espera     :: Float
    , radio_daño        :: Distance
    , daño_area         :: Float
    , imagen_aux        :: String
    } deriving (Show, Eq)

{- There are four types of obstacles:
    Solid: Robots collide with them but they do no damage.
    Harmful: Robots collide with them and they deal damage.
    Explosive: Robots collide with them and after some time they deal area damage.
    Healing: Robots pick them up and recover life.
-}
data TipoObstacle = Solid | Harmful | Explosive | Healing
    deriving (Show, Eq)

{- Game type: Represents the current state of the game.
Its attributes are:
    robots: List of robots present in the game.
    proyectiles: List of projectiles present in the game.
    explosiones: List of explosions present in the game.
    size: Size of the rectangle that bounds the scenario. These are positive coordinates representing the height and width of the screen.
    fondo: Path to the background image.
    eventosColisionActivos: List of active collision events.
    tiempoExplosive: Remaining time for the creation of a new Explosive.
    intervaloExplosive: Waiting time used to reset the counter after creating a new Explosive.
    tiempoHealing: Remaining time for the creation of a new Healing.
    intervaloHealing: Waiting time used to reset the counter after creating a new Healing.
-}
data Game =
    Game {
        robots :: [Robot],
        proyectiles :: [Projectile],
        explosiones :: [Explosion],
        obstaculos :: [Obstacle],
        size :: Size,
        fondo :: String,
        eventosColisionActivos :: [CollisionEvent],
        tiempoExplosive :: Float,
        intervaloExplosive :: Float,
        tiempoHealing :: Float,
        intervaloHealing :: Float
    } deriving (Show, Eq)


{- Action type: Represents the actions that can be applied to a bot.
Its attributes are:
    robot: Id of the robot to which the action will be applied.
    seMueve: Type of movement that will be applied.
    giraCuerpo: Type of rotation applied to the robot body.
    giraTorreta: Type of rotation applied to the robot turret.
    dispara: Type of action applied with respect to shooting.
-}
data Action =
    Action {
        robot :: RobotId, 
        seMueve :: MovementAction,
        giraCuerpo :: TurnAction,
        giraTorreta :: TurnAction,
        dispara :: ShootAction
    } deriving (Show, Eq)

-- Movement actions
data MovementAction = Forwards | Backwards | StopMovement
    deriving (Show, Eq)

-- Turning actions
data TurnAction = TurnRight | TurnLeft | StopTurn
    deriving (Show, Eq)

-- Shooting actions
data ShootAction = Shoot | DoNotShoot
    deriving (Show, Eq)


-- Collision events
data CollisionEvent
    = CollisionRobots Robot Robot
    | CollisionRobotProjectile Robot Projectile
    | CollisionRobotExplosion Robot Explosion
    | CollisionRobotObstacle Robot Obstacle
    deriving (Show, Eq)
