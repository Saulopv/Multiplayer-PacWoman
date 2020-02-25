-module(ghost).
-export([ghostMove/6, wait/6]).
-include_lib("eunit/include/eunit.hrl").

%Pseudo-documentation: Asks the server to move and receives its new coordinates
ghostMove(Pos, PlayerPos, PlayerPid, Graph, PlayerDirection, AlgoValue) ->
    requestMovement(Pos, PlayerPos, PlayerPid, Graph, PlayerDirection, AlgoValue),
    receive 
	{move, NewPos, {NewPlayerPos, _, Dir}} ->
	    wait(NewPos, NewPlayerPos, PlayerPid, Graph, Dir, AlgoValue)
    end.

%Pseudo-documentation: Waits for the server to ask where it is, answers
%THEN goes back to the movement
%TODO: May have to add counter when several players
wait(Pos, PlayerPos, PlayerPid, Graph, PlayerDir, AlgoValue) ->
    receive 
	go ->
	    ghostMove(Pos, PlayerPos, PlayerPid, Graph, PlayerDir, AlgoValue)
    end.

%Sends a request to the server to move in a direction
requestMovement(Pos, PlayerPos, PlayerPid, Graph, PlayerDir, AlgoValue) ->
  if AlgoValue == 1 ->
      networking:serverAtom() ! {npc, self(), Pos, calculateDir(Pos, PlayerPos, Graph), PlayerPid};
     AlgoValue == 2 ->
      networking:serverAtom() ! {npc, self(), Pos, calculateBot(Pos, PlayerPos, Graph, PlayerDir), PlayerPid}
  end.

% Calculates which direction the ghost should move in to get
% closer to the player using A* algorithm with the player as
% the goal node.
calculateDir({Posx, Posy}, {PlayerPosx, PlayerPosy}, Graph) ->
	path_finder:find_a_star(Graph, {Posx, Posy}, {PlayerPosx, PlayerPosy},no).

% Calculates which direction the ghostBot should move in to get
% closer to the player using A* algorithm where the goal node
% is 3 nodes added to the direction of the player.
calculateBot({Posx, Posy}, {PlayerPosx, PlayerPosy}, Graph, PlayerDir) ->
  bot:ambush_bot(Graph, {Posx, Posy}, {PlayerPosx, PlayerPosy}, PlayerDir).


    
