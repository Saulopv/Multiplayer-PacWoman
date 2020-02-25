-module(server).
-export([start/1, loop/4]).
-include_lib("eunit/include/eunit.hrl").

% @doc Spawns a game loop process and registers it as the game server.
% Uses the map found in sampleMap.txt.
start(IP) ->
    Map = world:parseWorldFromFile("src/map/sampleMap.txt"),
    Pid = spawn(server, loop, [Map, maps:new(), maps:new(), []]),
    networking:registerAsServer(Pid, IP).

% @doc Handles requests from actors to move on and get information from the map.
loop({Map, Dimensions, Points, PlayerSpawns, NPCSpawns}, Players, NPCPid, Tiles) ->
    WholeMap = {Map, Dimensions, Points, PlayerSpawns, NPCSpawns},

    receive
	{getMap, Pid} ->
	    Pid ! {Map, Dimensions},
	    loop(WholeMap, Players, NPCPid, Tiles);
	{changedTiles, Pid} ->
	    Pid ! Tiles,
	    loop(WholeMap, Players, NPCPid, Tiles);
	{initiatePlayer, Pid} ->
	    initializePlayer(WholeMap, Players, Pid, NPCPid, Tiles);
	{keypress, Pid, Pos, Dir, PixelPos, Tileswitch, GhostPid} ->
	    keypress(WholeMap, Players, NPCPid, Tiles, Pid, Pos, Dir, PixelPos, Tileswitch, GhostPid);
	{ghostPos, Pid, GhostPid} ->
	    ghostPos(Pid, GhostPid, NPCPid),
	    loop(WholeMap, Players, NPCPid, Tiles);
	{npc, Pid, Pos, Dir, PlayerPid} ->
	    npc(WholeMap, Players, NPCPid, Tiles, Pid, Pos, Dir, PlayerPid);
	{otherPlayers, Pid, PlayerPid} ->
	    OtherPlayers = maps:remove(PlayerPid,Players),
            Pid ! OtherPlayers,
	    loop(WholeMap, Players, NPCPid, Tiles);
	{otherGhosts,Pid,GhostPid} ->
	    OtherGhosts = maps:remove(GhostPid, NPCPid),
	    Pid ! OtherGhosts,
	    loop(WholeMap, Players, NPCPid, Tiles);
	Msg ->
	    io:format("Mismatched message: ~w~n", [Msg])
    end.

% -----------------------------------------------------------------------------
% ------------------------------ Helper fuctions ------------------------------
% -----------------------------------------------------------------------------

% Super basic and shitty initializing function
% TODO: Pick a random one of the spawn points.
initializePlayer({Map, Dimensions, Points, [{X, Y}| T], NPCSpawns}, Players, Pid, NPCPid,Tiles) ->
    PlayerPos = {X,Y},
    NewPlayers = maps:put(Pid, {PlayerPos,{X * 30 - 15, Y * 30 - 15},0}, Players),
    MapWithPlayer = maps:put(PlayerPos, {playerSpawn, empty, Pid}, Map),

    {GhostsPos, NewNPCPid, GhostPid, Value} = ghostSpawn(NPCSpawns, 4, PlayerPos, NPCPid, Pid, Map),
    MapWithGhost = maps:put(GhostsPos, {ghostSpawn, empty, npc}, MapWithPlayer),

    Pid ! {PlayerPos, GhostsPos, GhostPid, Value},
    io:format("Player ~p joined~n", [Pid]),
    loop({MapWithGhost, Dimensions, Points, [{X, Y}| T], NPCSpawns}, NewPlayers, NewNPCPid, Tiles).

% @doc Spawns a ghost process which will chase the player with the given pid.
% Returns the ghosts position, map of npc pids, and the ghosts pid.
% TODO: Pick a random one of the spawn points.
ghostSpawn([H | _], _, PlayerPos, NPCPid, PlayerPid, Map) ->
    Value = rand:uniform(2),
    Graph = path_finder:create_graph(Map),
    Pid = spawn(ghost, ghostMove, [H, PlayerPos, PlayerPid, Graph, 0, Value]),
    NewNPCPid = maps:put(Pid,H,NPCPid),
    {H, NewNPCPid, Pid, Value}.

% @doc Moves a player one step on the map if the movement is allowed, or updates
% the players pixel position. If the player does not exist in the map of players,
% a death message will be sent to the player and its ghost. 
% TODO: allow for several ghosts?
keypress({Map, Dimensions, Points, PlayerSpawns, NPCSpawns}, Players, NPCPid, Tiles, Pid, Pos, Dir, PixelPos, Tileswitch, GhostPid) ->
    WholeMap = {Map, Dimensions, Points, PlayerSpawns, NPCSpawns},
    PlayerExists = maps:is_key(Pid, Players),
    if 
	not PlayerExists ->
	    exit(GhostPid, dead),
	    Pid ! {-1, Pos},
	    loop(WholeMap, Players, maps:remove(GhostPid, NPCPid), Tiles);
	(Tileswitch == true) ->
	    {NewMap, NewPos, NewPoints, ChangedTile, NewPlayers} = 
		move(Pos, PixelPos, Dir, Map, Points, Players, Pid),
	    sendResult(Pos, NewPos, Points, NewPoints, Pid, Dir, NPCPid),
	    loop({NewMap, Dimensions, NewPoints, PlayerSpawns, NPCSpawns}, 
		 NewPlayers, NPCPid, addTile(ChangedTile, Tiles));
	true ->
	    {_, NewMap} = updateMapPlayer(Pid, Pos, PixelPos, 0, Map, Players),
	    Pid ! {Dir, Pos},
	    NewPlayers = maps:put(Pid, {Pos,PixelPos,Dir},Players),
	    loop({NewMap, Dimensions, Points, PlayerSpawns, NPCSpawns} , NewPlayers, NPCPid,Tiles)
    end.

% @doc Sends the direction the player has moved in and the new position
% to the given pid. If there are no more points on the map the player has won
% and the number 5 is sent instead of the directions 0-4. The game will then 
% restart and kill all ghost processes.
sendResult(OldPos, NewPos, Points, NewPoints, Pid, Dir, NPCPid) ->
    if
	NewPoints == 0 ->	
	    Pid ! {5, NewPos, Points - NewPoints},
	    Map = world:parseWorldFromFile("src/map/sampleMap.txt"), 
	    killAllGhosts(NPCPid),
	    flush(),
	    loop(Map, maps:new(), maps:new(), []);
	(Points > 0) and (NewPos /= OldPos) ->
	    Pid ! {Dir, NewPos, Points - NewPoints};
	true ->
	    Pid ! {0, NewPos, Points - NewPoints}
    end.

% @doc Sends an exit message to all ghost processes.
killAllGhosts(NPCPid) ->
    AllPids = maps:keys(NPCPid),
    killAllGhostsAux(AllPids).

killAllGhostsAux([Pid | []]) ->
    exit(Pid, gameOver);
killAllGhostsAux([Pid | Pids]) ->
    exit(Pid, gameOver),
    killAllGhostsAux(Pids).
    
% @doc Clears the mailbox. Inspired by Stackoverflow:
% https://stackoverflow.com/questions/11989627/empty-process-mail-box-in-erlang
flush() ->
    receive
	{keypress, Pid, Pos, _, _, _, _} ->
	    Pid ! {5, Pos, 0},
	    flush();
	{changedTiles, Pid} ->
	    Pid ! [],
	    flush();
	{ghostPos, Pid, _} ->
	    Pid ! {},
	    flush();
	{_, Pid, _} ->
	    Pid ! maps:new(),
	    flush();
	_ ->
	    flush()
    after
	200 -> ok
    end.

npc({Map, Dimensions, Points, PlayerSpawns, NPCSpawns}, Players, NPCPid, Tiles, Pid, Pos, Dir, PlayerPid) ->
    WholeMap = {Map, Dimensions, Points, PlayerSpawns, NPCSpawns},
    PlayerExists = maps:is_key(PlayerPid, Players),
    if 
	not PlayerExists ->
	    loop(WholeMap,
		 Players, NPCPid, Tiles);	
	true ->	
	    {NewMap, GhostPos, NewPlayers} = ghostMove(Pos, Dir, Map, Players),
	    NewNPCPid = maps:put(Pid,GhostPos,NPCPid),
	    Pid ! {move, GhostPos, maps:get(PlayerPid, Players)},
	    loop({NewMap, Dimensions, Points, PlayerSpawns, NPCSpawns},
		 NewPlayers, NewNPCPid, Tiles)
    end.

% @doc Returns an updated map, position, number of points, and changed tiles
% if the movement is allowed. Otherwise the same/no values are returned.
move(Pos, PixelPos, Dir, Map, Points, Players, Pid) ->
    PossibleGo = tileAllowed(Pos, Dir, Map),
    if
        PossibleGo ->
            updateAll(Pos, PixelPos, Dir, Map, Points, Players, Pid);
        not PossibleGo ->
            {Map, Pos, Points, {}, Players}
    end.

% @doc Returns an updated map and position if the movement is allowed,
% otherwise the same values are returned.
ghostMove(Pos, Dir, Map, Players) ->
    PossibleGo = tileAllowed(Pos, Dir, Map),
    if
        PossibleGo ->
	    {NewPlayers, NewMap} = updateMap(npc, Pos, placeholder, Dir, Map, Players),
            NewPos = coordinateInDir(Pos, Dir),
            {NewMap, NewPos, NewPlayers};
        not PossibleGo ->
            {Map, Pos, Players}
    end.

% @doc Returns updated values following a player movement.
% The desired position must exist on the map.
updateAll(Pos, PixelPos, Dir, Map, Points, Players, Pid) ->
    {NewPlayers, NewMap} = updateMap(Pid, Pos, PixelPos, Dir, Map, Players),
    NewPos = coordinateInDir(Pos, Dir),
    NewPoints = updatePoints(Pid, NewPos, Map, Points),
    ChangedTile = getChangedTile(NewPos, Map),
    {NewMap, NewPos, NewPoints, ChangedTile, NewPlayers}.

% @doc Returns the coordinates of a tile if there is an item on it.
% The position must exist on the map.
getChangedTile(Pos, Map) ->
    {_, Item, _} = maps:get(Pos, Map),
    if
	Item == empty ->
	    {};
	not (Item == empty) ->
	    Pos
    end.

% @doc Decrements points by one if the actor is a player and the position
% contains a point, otherwise returns the same number of points.
% The desired position must exist on the map.
updatePoints(Actor, Pos,  Map, Points) ->
    IsPlayer = isPlayer(Actor),
    PosHasPoint = posHasPoint(Pos, Map),
    if
	IsPlayer and PosHasPoint ->
	    Points - 1;
	not (IsPlayer and PosHasPoint) ->
	    Points
    end.

% @doc Returns true if the actor is not an NPC.
isPlayer(Actor) ->
   not ((Actor == npc) or (Actor == empty)).

% @doc Returns true if the tile at the position contains a point.
% The position must exist on the map.
posHasPoint(Pos, Map) ->
    {_, Thing, _} = maps:get(Pos, Map),
    Thing == point.

% @doc Returns a map where the actor has been moved one step in the specified
% direction. If the actor is a player, they will "eat" whatever object is on
% the new position (fruit, points etc).
% The desired position must exist on the map.
% TODO: Make "eating" fruits/powerUps do something besides just removing them
updateMap(Actor, Pos, PixelPos, Dir, Map, Players) ->
    IsPlayer = isPlayer(Actor),
    if 
	IsPlayer ->
	    updateMapPlayer(Actor, Pos, PixelPos, Dir, Map, Players);
	not IsPlayer ->
	    updateMapNPC(Pos, Dir, Map, Players)
    end.

updateMapPlayer(Pid, Pos, PixelPos, Dir, Map, Players) ->
    ClearedMap = maps:put(Pos, {tile, empty, empty}, Map),
    {_, Thing, Actor} = maps:get(coordinateInDir(Pos, Dir), Map),
    if 
	Actor == npc ->
	    {maps:remove(Pid, Players), maps:put(coordinateInDir(Pos, Dir), {tile, Thing, empty}, ClearedMap)};
	true ->
	    ClearedPlayers = maps:remove(Pid, Players),
	    NewPlayers = maps:put(Pid, {coordinateInDir(Pos, Dir), PixelPos, Dir}, ClearedPlayers),
	    {NewPlayers, maps:put(coordinateInDir(Pos, Dir), {tile, empty, Pid}, ClearedMap)}
    end.

updateMapNPC(Pos, Dir, Map, Players) ->
    {_, OldThing, _} = maps:get(Pos, Map),
    ClearedMap = maps:put(Pos, {tile, OldThing, empty}, Map),
    {_, NewThing, Actor} = maps:get(coordinateInDir(Pos, Dir), Map),
    IsPlayer = isPlayer(Actor),
    if
	IsPlayer ->
	    {maps:remove(Actor, Players), maps:put(coordinateInDir(Pos, Dir), {tile, NewThing, empty}, ClearedMap)};
	not IsPlayer ->
	    {Players, maps:put(coordinateInDir(Pos, Dir), {tile, NewThing, npc}, ClearedMap)}
    end.

% @doc Sends the logical position of the specified ghost process
% and gives the ghost the signal to move again.
ghostPos(Pid, GhostPid, NPCPid) ->
    Pid ! maps:get(GhostPid, NPCPid),
    GhostPid ! go.

% @doc Returns true if an actor is allowed to move in the specified direction.
% TODO: Appear at the other end of the screen if you go outside it
tileAllowed(Pos, Dir, Map) ->
    DesiredPos = coordinateInDir(Pos, Dir),
    TileExists = maps:is_key(DesiredPos, Map),
    if
	TileExists ->
	    notAWall(DesiredPos, Map);
	not TileExists ->
	    false
    end.

% @doc Returns true if the tile on a position of a map is not a wall.
% The position must exist on the map.
notAWall(Pos, Map) ->
    {Type, _, _} = maps:get(Pos, Map),
    if
	Type == wall ->
	    false;
	not (Type == wall) ->
	    true
    end.

% @doc Adds a tuple of coordinates to a list, unless the tuple is empty.
addTile(Tile, Tiles) ->
    if 
	Tile == {} ->
	    Tiles;
	true ->
	    [Tile | Tiles]
    end.

% @doc Returns the coordinates of the position one step in the specified
% direction.
coordinateInDir({X, Y}, 1) ->
    {X, Y - 1};
coordinateInDir({X, Y}, 2) ->
    {X, Y + 1};
coordinateInDir({X, Y}, 3) ->
    {X - 1, Y};
coordinateInDir({X, Y}, 4) ->
    {X + 1, Y};
coordinateInDir({X, Y}, 0) ->
    {X, Y}.


% -----------------------------------------------------------------------------
% ---------------------------------- Tests ------------------------------------
% -----------------------------------------------------------------------------
% NOTE: ALL TESTS ARE PROBABLY GARBAGE SINCE STUFF CHANGES ALL THE TIME
%notAWall_test() ->
%    ?assert(notAWall({1,1}, #{{1,1} => {wall, empty, empty}}) == false),
%    ?assert(notAWall({1,1}, #{{1,1} => {tile, empty, empty}}) == true).
%
%tileAllowed_test() ->
%    Map = #{{1,1} => {tile, empty, empty},
%	    {2,1} => {wall, empty, empty},
%	    {0,1} => {tile, point, empty}},
%    ?assert(tileAllowed({1,1}, 4, Map) == false),
%    ?assert(tileAllowed({1,1}, 3, Map) == true),
%    ?assert(tileAllowed({5,5}, 1, Map) == false).
%
%updateMap_test() ->
%    Map = #{{1,1} => {tile, empty, madlad1337},
%	    {0,1} => {tile, point, empty},
%	    {2,1} => {tile, powerUp, empty}},
%    ?assert(updateMap(madlad1337, {1,1}, 3, Map) ==
%		#{{1,1} => {tile, empty, empty},
%		  {0, 1} => {tile, empty, madlad1337},
%		  {2,1} => {tile, powerUp, empty}}),
%    ?assert(updateMap(madlad1337, {1,1}, 4, Map) ==
%		#{{1,1} => {tile, empty, empty},
%		  {0, 1} => {tile, point, empty},
%		  {2,1} => {tile, empty, madlad1337}}),
%    ?assert(updateMap(npc, {1,1}, 3, Map) ==
%		#{{1,1} => {tile, empty, empty},
%		  {0, 1} => {tile, point, npc},
%		  {2,1} => {tile, powerUp, empty}}).
%
%updatePoints_test() ->
%    Map = #{{1,1} => {tile, empty, empty},
%	    {0,1} => {tile, point, empty},
%	    {2,1} => {tile, fruit, empty}},
%    ?assert(updatePoints(madlad1337, {1,1}, Map, 10) == 10),
%    ?assert(updatePoints(madlad1337, {0,1}, Map, 10) == 9),
%    ?assert(updatePoints(madlad1337, {2,1}, Map, 10) == 10),
%    ?assert(updatePoints(npc, {0,1}, Map, 10) == 10).
%
