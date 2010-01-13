-module(tman).
%-export([init/0, init/2]).
-compile(export_all).

-define(NODES_DEFAULT, 900).
-define(DEGREE_DEFAULT, 20).
-define(CYCLES_DEFAULT, 50).
-define(SIZE, 30).

-record(node, {id, x, y, random_nodes=[], structured_nodes=[]}).

init() ->
    init(?NODES_DEFAULT, ?DEGREE_DEFAULT, ?CYCLES_DEFAULT, ?SIZE).

init(Nodes, Degree, Cycles, Size) ->
    main(Nodes, Degree, Cycles, Size).

main(Nodes, Degree, Cycles, Size) ->
    io:format("Running T-Man with NODES ~w of DEGREE  ~w over CYCLES ~w in space of SIZE ~w ~n", 
	      [Nodes, Degree, Cycles, Size]).

init_nodes(NodeNumber, Degree, Size) ->
    init_nodes(NodeNumber, Degree, Size, []).

init_nodes(0, Degree, _, Nodes) ->
    init_nodes_neighbors(Nodes, Degree);
init_nodes(NodeNumber, Degree, Size, Nodes) ->
    init_nodes(NodeNumber - 1, Degree, Size, [#node{id=NodeNumber,x=1,y=2}|Nodes]).  

init_nodes_neighbors(Nodes, Degree) ->
    [Nodes, Degree].

generate_random_unique_coordinate(NodeNumber, Size, _) when (NodeNumber > Size * Size) ->
    exit(too_many_nodes);
generate_random_unique_coordinate(NodeNumber, _, Coordinates) when (length(Coordinates) >= NodeNumber) ->
    exit(available_coordinates_exhausted);
generate_random_unique_coordinate(NodeNumber, Size, Coordinates) ->
    generate_random_unique_coordinate(NodeNumber, Size, Coordinates, []).

generate_random_unique_coordinate(NodeNumber, Size, Coordinates, []) ->
    Coord = [generate_random_coordinate(Size), generate_random_coordinate(Size)],
    case coordinate_is_unique(Coord, Coordinates) of
	true -> [Coord, [Coord|Coordinates]];
	false -> generate_random_unique_coordinate(NodeNumber, Size, Coordinates, [])
    end.

generate_random_coordinate(Size) ->
    random:uniform(Size) - 1.

coordinate_is_unique(Coord, Coordinates) ->
    not lists:member(Coord, Coordinates).

node_distance(N1, N2) ->
    abs(N1#node.x - N2#node.x) + abs(N1#node.y - N2#node.y).
