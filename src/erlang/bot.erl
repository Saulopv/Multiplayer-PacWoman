-module(bot).
-export([ambush_bot/4, stepsAhead/3]).
-include_lib("eunit/include/eunit.hrl").

%% @doc Gives the best next direction by estimating the path ahead fo the player
%% with a variable N.
%% If the bot is less than 3 tiles away from the player then it will find the
%% shortest path to the player using a_star.
%% @return A direction as represented by a digit.
ambush_bot({Map, Graph}, Pos, PlayerPos, PlayerDir) ->
  {Steps, Path} = path_finder:find_a_star({Map, Graph}, Pos, PlayerPos, yes),
  if
    Steps >= 3 ->
      NewPlayerPos = stepsAhead(PlayerPos, PlayerDir, Map),
      {_, NewPath} = path_finder:find_a_star({Map, Graph}, Pos, NewPlayerPos, yes),
      path_finder:get_direction(Pos, NewPath);
    Steps =< 2 ->
      path_finder:get_direction(Pos, Path)
  end.

% -----------------------------------------------------------------------------
% ------------------------------ Helper functions ------------------------------
% -----------------------------------------------------------------------------

%% @doc Given the direction the player choose last, this function
%% does the calculation of the Nth next tiles the player will take.
stepsAhead(PlayerPos, PlayerDir, _) when PlayerDir == 0 ->
  PlayerPos;
stepsAhead(PlayerPos, PlayerDir, Map) ->
  F = fun(N1, N2, Pred) -> if Pred == add -> N1 + N2;
                              Pred /= add -> N1 - N2 end end,
  N = 3, %The amount of steps to look ahead
    if
      PlayerDir == 1 -> %up
        is_validY(PlayerPos, F, neg, Map, N);
      PlayerDir == 2 -> %down
        is_validY(PlayerPos, F, add, Map, N);
      PlayerDir == 3 -> %left
        is_validX(PlayerPos, F, neg, Map, N);
      PlayerDir == 4 -> %right
        is_validX(PlayerPos, F, add, Map, N)
    end.

%% @doc Checks the map for a position.
is_validY(Pos, _, _, _, N) when N == 0 ->
  Pos;
is_validY({X,Y}, F, Pred, Map, N) ->
  NewY = F(Y, N, Pred),
  Valid = maps:get({X,NewY}, Map, no),
  case Valid of
    no -> is_validY({X,Y}, F, Pred, Map, N-1);
    _  -> {X,NewY}
  end.

%% @doc Checks the map for a given position.
is_validX(Pos, _, _, _, N) when N == 0 ->
  Pos;
is_validX({X,Y}, F, Pred, Map, N) ->
  NewX = F(X, N, Pred),
  Valid = maps:get({NewX,Y}, Map, no),
  case Valid of
    no -> is_validX({X,Y}, F, Pred, Map, N-1);
    _  -> {NewX,Y}
  end.

% -----------------------------------------------------------------------------
% ---------------------------------- Tests ------------------------------------
% -----------------------------------------------------------------------------

stepsAhead_test() ->
  {Map,_,_,_,_} = world:parseWorldFromFile("../map/testPathMap.txt"),
  M = path_finder:map_without_walls(Map),
  ?assert({1,1} == stepsAhead({1,1}, 1, M)),
  ?assert({1,3} == stepsAhead({1,1}, 2, M)),
  ?assert({1,1} == stepsAhead({1,1}, 3, M)),
  ?assert({4,1} == stepsAhead({1,1}, 4, M)),
  ?assert({6,1} == stepsAhead({5,1}, 4, M)).
