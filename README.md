# Autonomous Pirate Battle Simulation
A 2D top-down naval combat simulation built in Haskell. This project features autonomous pirate ships with different behaviors (AI), a custom physics engine for collisions, and a tournament system with real-time statistics.

## Features
- **Autonomous AI Ships**: Each ship decides its movement and firing strategy without player input.
- **Projectile and Collision Physics**: Ships fire projectiles with realistic 2D collision detection.
- **Obstacles and Interactive Environment**: Solid, harmful, explosive and healing obstacles affect ship behavior.
- **Dynamic Game Arena**: Ships and obstacles spawn in random positions, some obstacles appear over time.
- **Real-Time Rendering**: Visual representation of ships, projectiles, explosions and the arena using Gloss.
- **Statistics Tracking**: Tracks hits, survival time, and winners across multiple tournaments.

## Gameplay
![Autonomous pirate battle simulation](media/demo.gif)

## Installation and Execution
1. Install [Haskell 8.6.5](https://downloads.haskell.org/platform/8.6.5/).
2. Install dependencies:
```bash
cabal update
cabal install gloss gloss-juicy random data-default containers
```
3. Clone this repository:
```bash
git clone https://github.com/ivrugue/autonomous-pirate-battle-simulation
cd autonomous-pirate-battle-simulation/src
```
4. Run the simulation:
```bash
ghci Main.hs
main
```

## Project Structure
- ```HAUS.Types```: Core data structures and type aliases.
- ```HAUS.Entities```: Definitions of robots, projectiles, explosions and obstacles.
- ```HAUS.Tournaments```: Tournament orchestration and game configuration.
- ```HAUS.Game```: Core game loop and state updates.
- ```HAUS.Physics```: Vector math and movement calculations.
- ```HAUS.Collisions```: 2D collision detection using the Separating Axis Theorem (SAT).
- ```HAUS.Behavior```: Movement logic and velocity updates.
- ```HAUS.AI```: Decision-making algorithms and memory management.
- ```HAUS.Spawner```: Random entity generation and timed events.
- ```HAUS.Stats```: Statistics tracking and exporting.
- ```HAUS.Renderer```: Functions for rendering the simulation using Gloss.

## Configuration
You can customize the simulation by editing ```config/config.txt```. You can modify:
- ```Basic```/```Tower```/```Kamikaze```: Number of ships per type.
- ```Solid```/```Harmful```/```Explosive```/```Healing```: Number of obstacles per type.
- ```Area```: Map dimensions.
- ```Max duration```: Time limit per match.
- ```Number```: Amount of tournaments to play.

## Controls
- **Space**: Start the simulation.
- **P**: Pause/Resume the simulation.