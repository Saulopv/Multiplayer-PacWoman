-module(path_finder).
-export([create_graph/1,find_a_star/4, map_without_walls/1, get_direction/2]).
-include_lib("eunit/include/eunit.hrl").

-record(graph, {
  type       :: graphtype(),
  graph      :: digraph:graph(),
  weightType :: weighttype()
}).
-type graph()      :: #graph{}.
-type vertex()     :: term().
-type edge()       :: {vertex(), vertex()}.
-type graphtype()  :: directed | undirected.
-type weight()     :: number().
-type weighttype() :: unweighted | d | f.

%% @doc Creates a undirected weighted graph from a map.
%% Weight = 1.
%% The total amount of Vertices are the total amount of entries in
%% the Map - (the entries that contains wall).
%% @returns a graph containing all the possible nodes.
create_graph(Map) ->
  M = map_without_walls(Map),
  Vertices = maps:keys(M),
  Edges = connecting_nodes(Vertices,M,[]),
  {M, init_graph(Vertices,Edges)}.

%% @doc Finds the fastest path from A to B using "A star algorithm".
%% @returns The next best possible direction to move.
%% The function can be modified so it can return {Cost,Path},
%% Cost, total amount of steps, in weight.
%% Path, a list with all the steps from A to B.
find_a_star({_, G},Npc,Player,Yes) ->
  F = fun({X1, Y1}, {X2, Y2}) -> abs(X1 - X2) + abs(Y1 - Y2) end,
  if
    Yes == yes ->
      a_star:run(G,Npc,Player,F);
    Yes /= yes ->
      {_, Path} = a_star:run(G,Npc,Player,F),
      get_direction(Npc,Path)
  end.

% ---------------------------------------------------------------------------------
% -------------------------------- Helper functions -------------------------------
% ---------------------------------------------------------------------------------

%% @doc Initialize a weighted undirected graph.
%% @returns Graph.
init_graph(Vertices, Edges) ->
  G = #graph{type=undirected,graph=digraph:new(),weightType=d},
  ok = init_vertices(G, Vertices),
  ok = init_edges(G, Edges),
  G.

%% @doc Initialize a list of Vertices.
%% @returns A list with all the Vertices initialized.
init_vertices(G, Vertices) ->
  lists:foreach(fun(V) -> add_vertex(G, V) end, Vertices).

%% @doc Add a vertex to a graph
-spec add_vertex(graph(), vertex()) -> vertex().

add_vertex(G, V) ->
  digraph:add_vertex(G#graph.graph, V).

%% @doc Initialize the edges.
init_edges(_, []) -> ok;
init_edges(G ,[H|T]) ->
  {V1, V2, W} = H,
  _ = add_edge(G, V1, V2, W),
  init_edges(G, T).

%% @doc Add an edge to a weighted graph
-spec add_edge(graph(), vertex(), vertex(), weight()) -> edge() | {error, not_numeric_weight}.

add_edge(#graph{type=directed, graph=G}, From, To, W) when is_number(W) ->
  digraph:add_edge(G, {From, To}, From, To, W);
add_edge(#graph{type=undirected, graph=G}, From, To, W) when is_number(W) ->
  digraph:add_edge(G, {From, To}, From, To, W),
  digraph:add_edge(G, {To, From}, To, From, W);
add_edge(_G, _From, _To, _W) ->
  {error, not_numeric_weight}.

%% @doc Connecting all the Edges.
connecting_nodes([],_,Edges) ->
  Edges;
connecting_nodes([{X,Y}|T],Map,Edges) ->
  L = default(X,Y,Map),
  connecting_nodes(T,Map,L ++ Edges).

default(X,Y,M) ->
  L = [],
  Right = {X+1,Y},
  Bot = {X,Y+1},
  Left = {X-1,Y},
  Top = {X,Y-1},
  Temp = [Right,Bot,Left,Top],
  is_valid({X,Y},M,Temp,L).

%% @doc Filters out all key mappings to a value that consist of the atom "wall".
%% @returns A new Map with all no walls.
map_without_walls(Map) ->
  F = fun(_,{W,_,_}) -> W /= wall end,
  maps:filter(F,Map).

%% @doc Checks if it exist a key in the Map.
%% @returns A list with Edges.
is_valid(_,_,[],L) ->
  L;
is_valid(K,Map,[H|Tail],L) ->
  T = maps:get(H,Map,no),
  case T of
    no -> is_valid(K,Map,Tail,L);
    _  ->
      Edge = {K,H,1},
      is_valid(K,Map,Tail,[Edge|L])
  end.

get_direction(_,[]) ->
  0;
get_direction(_,[_|T]) when T == [] ->
  0;
get_direction(K,[_,Next]) ->
  direction(K, Next);
get_direction(K, [_|T]) ->
  [H|_] = T,
  direction(K,H).

%% @doc Evaluates what direction to go.
%% @returns The value which is represented as a direction.
direction({X,Y},{NX,NY}) ->
  if
    NY == Y-1 ->
      1; %%Up
    NY == Y+1 ->
      2; %%Down
    NX == X-1 ->
      3; %Left
    NX == X+1 ->
      4  %Right
  end.

% -----------------------------------------------------------------------------
% ---------------------------------- Tests ------------------------------------
% -----------------------------------------------------------------------------

createGraph_test() ->
  {Map,_,_,_,_} = world:parseWorldFromFile("../map/testPathMap.txt"),
  Graph = create_graph(Map),
  Vertices = graph:vertices(Graph),
  Edges = graph:edges(Graph),
  VL = [{3,3},{6,1},{5,6},{6,2},{1,3},{4,1},{6,6},{4,6},{4,3},
        {6,3},{4,2},{2,3},{6,5},{2,1},{5,1},{3,1},{1,2},{3,6},
        {5,3},{2,6},{6,4},{2,2},{5,2},{1,6},{1,1},{3,2}],
  EL = [{{1,6},{2,6}},{{4,3},{3,3}},{{5,6},{6,6}},{{6,4},{6,3}},
        {{6,1},{5,1}},{{3,2},{3,3}},{{4,2},{4,3}},{{4,1},{3,1}},
        {{3,1},{3,2}},{{6,2},{6,1}},{{5,6},{4,6}},{{2,2},{2,3}},
        {{1,1},{1,2}},{{6,6},{6,5}},{{1,3},{1,2}},{{2,1},{1,1}},
        {{5,2},{4,2}},{{5,3},{4,3}},{{5,2},{5,3}},{{1,3},{2,3}},
        {{5,2},{6,2}},{{4,6},{3,6}},{{2,2},{1,2}},{{2,2},{3,2}},
        {{2,1},{2,2}},{{4,2},{3,2}},{{3,6},{2,6}},{{2,1},{3,1}},
        {{6,3},{6,2}},{{5,1},{4,1}},{{6,3},{5,3}},{{3,3},{2,3}},
        {{4,1},{4,2}},{{5,2},{5,1}},{{6,5},{6,4}}],
  ?assert(VL =:= Vertices),
  ?assert(EL =:= Edges).


connectingNodes_test() ->
  Map = #{{3,3}=>{fruit, empty, empty}, {3,2}=>{tile, empty, empty},
          {2,3}=>{point, empty, empty}, {4,3}=>{tile, empty, empty},
          {3,4}=>{point, empty, empty}},
  Edges = connecting_nodes([{3,3}],Map,[]),
  L = [{{3,3},{3,2},1}, {{3,3},{2,3},1}, {{3,3},{3,4},1}, {{3,3},{4,3},1}],
  ?assert(L =:= Edges).

findAstar_test() ->
  {Map,_,_,_,_} = world:parseWorldFromFile("../map/testPathMap.txt"),
  Graph = create_graph(Map),
  NextMove1 = find_a_star(Graph,{1,1},{1,6},no),
  ?assert(4 =:= NextMove1),
  NextMove2 = find_a_star(Graph,{2,1},{1,6},no),
  ?assert(4 =:= NextMove2),
  NextMove3 = find_a_star(Graph,{3,1},{1,6},no),
  ?assert(4 =:= NextMove3),
  NextMove4 = find_a_star(Graph,{4,1},{1,6},no),
  ?assert(4 =:= NextMove4),
  NextMove5 = find_a_star(Graph,{5,1},{1,6},no),
  ?assert(4 =:= NextMove5),
  NextMove6 = find_a_star(Graph,{6,1},{1,6},no),
  ?assert(2 =:= NextMove6),
  NextMove7 = find_a_star(Graph,{6,2},{1,6},no),
  ?assert(2 =:= NextMove7),
  NextMove8 = find_a_star(Graph,{6,3},{1,6},no),
  ?assert(2 =:= NextMove8),
  NextMove9 = find_a_star(Graph,{6,4},{1,6},no),
  ?assert(2 =:= NextMove9),
  NextMove10 = find_a_star(Graph,{6,5},{1,6},no),
  ?assert(2 =:= NextMove10),
  NextMove11 = find_a_star(Graph,{6,6},{1,6},no),
  ?assert(3 =:= NextMove11),
  NextMove12 = find_a_star(Graph,{5,6},{1,6},no),
  ?assert(3 =:= NextMove12),
  NextMove13 = find_a_star(Graph,{4,6},{1,6},no),
  ?assert(3 =:= NextMove13),
  NextMove14 = find_a_star(Graph,{3,6},{1,6},no),
  ?assert(3 =:= NextMove14),
  NextMove15 = find_a_star(Graph,{2,6},{1,6},no),
  ?assert(3 =:= NextMove15),
  NextMove16 = find_a_star(Graph,{1,6},{1,6},no),
  ?assert(0 =:= NextMove16).

