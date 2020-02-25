-module(world).
-export([parseWorldFromFile/1, getWorldDimensions/1, countPoints/1, getPlayerSpawns/1, getNpcSpawns/1, getTestWorld/0]).

-include_lib("eunit/include/eunit.hrl").

% @doc Parses a txt file into a world map.
% See testMap.txt for a descripton of how a map file is formatted
% Causes an error if there is a problem reading the file.
% See http://erlang.org/doc/man/file.html#read_file-1 for the different possible reasons.
parseWorldFromFile(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} -> 
            Map = parse(binary_to_list(Content)),
            Dimensions = getWorldDimensions(Map),
            Points = countPoints(Map),
            PlayerSpawns = getPlayerSpawns(Map),
            NpcSpawns = getNpcSpawns(Map), 
            {Map, Dimensions, Points, PlayerSpawns, NpcSpawns};
        {error, Reason} -> 
            error("Couldn't read " ++ Filename ++ " because " ++ atom_to_list(Reason))
    end.

% @doc Gets the dimension of the map. 
% Does this by finding the largest key and adding 1 to each of its coordinate.
getWorldDimensions(Map) ->
    {X, Y} = maps:fold(fun(K,_,Acc) when K > Acc -> K;
                          (_,_,Acc) -> Acc end, 
                       {-1,-1}, 
                       Map),
    {X + 1, Y + 1}.

% @doc Counts how many point atoms there are on the map.
countPoints(Map) ->
    maps:fold(fun(_,{_,point,_},Acc) -> Acc + 1;
                 (_,_,Acc) -> Acc end,
              0, 
              Map).

% @doc Returns a list containing the coordinates of every playerSpawn atom on the map.
getPlayerSpawns(Map) ->
    maps:fold(fun(Coordinates, {playerSpawn,_,_}, Acc) -> [Coordinates|Acc];
                 (_,_,Acc) -> Acc end,
              [], 
              Map).

% @doc Returns a list containing the coordinates of every npcSpawn atom on the map.
getNpcSpawns(Map) ->
    maps:fold(fun(Coordinates, {npcSpawn,_,_}, Acc) -> [Coordinates|Acc];
                 (_,_,Acc) -> Acc end,
              [], 
              Map).

% ---------------------------------------------------------------------------------
% -------------------------------- Helper fuctions --------------------------------
% ---------------------------------------------------------------------------------

% @doc Takes a map file as a string and parses it into a world state.
% The map file must obey the rules explained in the testMap.txt file.
parse(String) -> parse(0, 0, #{}, String).


% @doc Helper function for parse/1.
parse(_, _, Map, []) -> Map;

parse(X, Y, Map, [$%|T]) -> 
    [_, NewList] = string:split(T, "\n"),
    parse(X, Y, Map, NewList);

parse(X, Y, Map, [$\n|T]) -> 
    if 
        X == 0 -> parse(0, Y, Map, T);
        X > 0 -> parse(0, Y + 1, Map, T)
    end;

parse(X, Y, Map, [H|T]) when (H == $ ) or (H == $\t)-> 
    parse(X, Y, Map, T);

parse(X, Y, Map, [H|T]) -> 
    NewMap = maps:put({X,Y}, {type(H), content(H), empty}, Map),
    parse(X + 1, Y, NewMap, T).


% @doc Translates map characters into corresponding type atoms.
% Causes an error if the character wasn't recognized.
type($W) -> wall;
type($0) -> tile;
type($P) -> playerSpawn;
type($N) -> npcSpawn;
type($x) -> tile;
type($u) -> tile;
type($f) -> tile;
type(X) -> error("Got an error when parsing " ++ [X]).


% @doc Translates map characters into corresponding content atoms.
% Causes an error if the character wasn't recognized.
content($W) -> empty;
content($0) -> empty;
content($P) -> empty;
content($N) -> empty;
content($x) -> point;
content($u) -> powerUp;
content($f) -> fruit;
content(X) -> error("Got an error when parsing " ++ [X]).

% ----------------------------------------------------------------------------
% -------------------------------- Unit tests --------------------------------
% ----------------------------------------------------------------------------

% ?assert() true/false
% ?assertEqual() Expected =:= Result 
% ?assertMatch() Pattern = Result
% ?assertError() Error caused by Expression

% @doc Gets a hardcoded 7x7 test world.
getTestWorld() ->
    #{{0,0}=>{wall, empty, empty}, {1,0}=>{wall, empty, empty}, {2,0}=>{wall, empty, empty}, {3,0}=>{wall,        empty, empty},
      {0,1}=>{wall, empty, empty}, {1,1}=>{tile, point, empty}, {2,1}=>{tile, empty, empty}, {3,1}=>{playerSpawn, empty, empty}, 
      {0,2}=>{wall, empty, empty}, {1,2}=>{tile, point, empty}, {2,2}=>{wall, empty, empty}, {3,2}=>{wall,        empty, empty},
      {0,3}=>{wall, empty, empty}, {1,3}=>{tile, point, empty}, {2,3}=>{tile, fruit, empty}, {3,3}=>{npcSpawn,    empty, empty},
      {0,4}=>{wall, empty, empty}, {1,4}=>{tile, point, empty}, {2,4}=>{wall, empty, empty}, {3,4}=>{wall,        empty, empty},
      {0,5}=>{wall, empty, empty}, {1,5}=>{tile, point, empty}, {2,5}=>{tile, empty, empty}, {3,5}=>{playerSpawn, empty, empty}, 
      {0,6}=>{wall, empty, empty}, {1,6}=>{wall, empty, empty}, {2,6}=>{wall, empty, empty}, {3,6}=>{wall,        empty, empty},

      {4,0}=>{wall, empty, empty},   {5,0}=>{wall, empty, empty}, {6,0}=>{wall, empty, empty},
      {4,1}=>{tile, empty, empty},   {5,1}=>{tile, point, empty}, {6,1}=>{wall, empty, empty},
      {4,2}=>{wall, empty, empty},   {5,2}=>{tile, point, empty}, {6,2}=>{wall, empty, empty},
      {4,3}=>{tile, powerUp, empty}, {5,3}=>{tile, point, empty}, {6,3}=>{wall, empty, empty},
      {4,4}=>{wall, empty, empty},   {5,4}=>{tile, point, empty}, {6,4}=>{wall, empty, empty},
      {4,5}=>{tile, empty, empty},   {5,5}=>{tile, point, empty}, {6,5}=>{wall, empty, empty},
      {4,6}=>{wall, empty, empty},   {5,6}=>{wall, empty, empty}, {6,6}=>{wall, empty, empty}}.

parseWorldFromFile_test() ->
    {Map, Dimensions, Points, PlayerSpawns, NpcSpawns} = parseWorldFromFile("src/map/testMap.txt"),
    TestMap = getTestWorld(),
    [?assertEqual(TestMap, Map),
     ?assertEqual({7,7}, Dimensions),
     ?assertEqual(10, Points),
     ?assertEqual([{3,5}, {3,1}], PlayerSpawns),
     ?assertEqual([{3,3}], NpcSpawns),
     ?assertError(_, parseWorldFromFile(""))].

getWorldDimensions_test() ->
    TestMap = getTestWorld(),
    [?assertEqual({7,7}, getWorldDimensions(TestMap)),
     ?assertEqual({0,0}, getWorldDimensions(#{}))].

countPoints_test() ->
    TestMap = getTestWorld(),
    [?assertEqual(10, countPoints(TestMap)),
     ?assertEqual(0, countPoints(#{}))].

getPlayerSpawns_test() ->
    TestMap = getTestWorld(),
    [?assertEqual([{3,5}, {3,1}], getPlayerSpawns(TestMap)),
     ?assertEqual([], getPlayerSpawns(#{}))].

getNpcSpawns_test() ->
    TestMap = getTestWorld(),
    [?assertEqual([{3,3}], getNpcSpawns(TestMap)),
     ?assertEqual([], getNpcSpawns(#{}))].

parse_test() ->
    MapString = "WWWWWWW\nWx0P0xW\nWxWWWxW\nWxfNuxW\nWxWWWxW\nWx0P0xW\nWWWWWWW\n",
    Map = parse(MapString),
    TestMap = getTestWorld(),
    [?assertEqual(TestMap, Map),
     ?assertEqual(#{}, parse(""))].

type_test() ->
    [?assertEqual(wall,        type($W)),
     ?assertEqual(tile,        type($0)),
     ?assertEqual(playerSpawn, type($P)),
     ?assertEqual(npcSpawn,    type($N)),
     ?assertEqual(tile,        type($x)),
     ?assertEqual(tile,        type($u)),
     ?assertEqual(tile,        type($f)),
     ?assertError(_, type($a))].

content_test() ->
    [?assertEqual(empty,   content($W)),
     ?assertEqual(empty,   content($0)),
     ?assertEqual(empty,   content($P)),
     ?assertEqual(empty,   content($N)),
     ?assertEqual(point,   content($x)),
     ?assertEqual(powerUp, content($u)),
     ?assertEqual(fruit,   content($f)),
     ?assertError(_, content($a))].
