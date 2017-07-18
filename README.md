# games

### to build and run

    $ rebar3 shell

### to test

open ```http://localhost:8080/connect4``` in 2 browser tabs (one for each player)

![GitHub Logo](/games.png)

## Overview

* Serves the HTML + JS for the list of games located in ```priv``` folder
* manages the pool of players who joined a specific game, and broadcast each moves.

## API

* ```recv``` incoming websocket message
* ```resp``` respond to client
* ```bcast``` message to a group of clients

#### List of players in game room

* recv ```{msg:"players", game: "connect4"}```
* resp ```{key:"joined", val: "Joe"}```

#### Join a game room

* recv ```{msg:"join", game: "connect4", player: "Joe"}```
* bcast ```{key:"joined", val: "Joe"}```

#### Start a game with selected players

* recv ```{msg:"start", players: ["Joe", "Mike"]}```
* bcast ```{key:"started", val: "56576-767867-6876786-786768"}```

#### Make a move 

"3,4@1" means: 'player 1 moved to coordinates 3,4 on the board'

* recv ```{msg:"move", move: "3,4"}```
* bcast ```{key:"move", val: "3,4@1"}```

#### Quit a game

* recv ```{msg:"quit"}```
* bcast ```{key:"quit", val: "Joe"}```
