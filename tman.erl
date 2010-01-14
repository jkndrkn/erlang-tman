-module(tman).
%-export([init/0, init/2]).
-compile(export_all).

-define(NODES_DEFAULT, 900).
-define(DEGREE_DEFAULT, 20).
-define(CYCLES_DEFAULT, 50).
-define(SIZE, 30).

-record(node, {id, x, y, neighbors=[]}).

init() ->
    init(?NODES_DEFAULT, ?DEGREE_DEFAULT, ?CYCLES_DEFAULT, ?SIZE).

init(Nodes, Degree, Cycles, Size) ->
    main(Nodes, Degree, Cycles, Size).

main(NodeNumber, Degree, Cycles, Size) ->
    io:format("# Running T-Man with NODES ~w of DEGREE  ~w over CYCLES ~w in space of SIZE ~w ~n",
	      [NodeNumber, Degree, Cycles, Size]),
    Nodes = init_nodes(NodeNumber, Degree, Size),
    Nodes.

init_nodes(NodeNumber, Degree, Size) ->
    init_nodes(NodeNumber, Degree, Size, [], [], NodeNumber).

init_nodes(0, Degree, _, Nodes, _, _) ->
    init_nodes_neighbors(Nodes, Degree);
init_nodes(NodeNumber, Degree, Size, Nodes, Coordinates, NodesTotal) ->
    [[X,Y],CoordinatesNew] = generate_random_unique_coordinate(NodesTotal, Size, Coordinates),
    init_nodes(NodeNumber - 1, Degree, Size, [#node{id=NodeNumber,x=X,y=Y}|Nodes], CoordinatesNew, NodesTotal).

init_nodes_neighbors(Nodes, Degree) ->
    init_nodes_neighbors(Nodes, Degree, length(Nodes), []).

init_nodes_neighbors([], _, _, NodesOutput) ->
    NodesOutput;
init_nodes_neighbors([Node|NodesInput], Degree, NodeNumber, NodesOutput) ->
    Neighbors = generate_neighbors(Node#node.id, NodeNumber, Degree),
    init_nodes_neighbors(NodesInput, Degree, NodeNumber, [Node#node{neighbors=Neighbors}|NodesOutput]).

generate_neighbors(_, NodeNumber, Degree) when Degree > NodeNumber ->
    exit(degree_too_high);
generate_neighbors(ThisNode, NodeNumber, Degree) ->
    generate_neighbors(ThisNode, NodeNumber, Degree, []).

generate_neighbors(_, _, 0, Neighbors) ->
    Neighbors;
generate_neighbors(ThisNode, NodeNumber, Degree, Neighbors) ->
    Neighbor = random:uniform(NodeNumber),
    case ThisNode =:= Neighbor of
	true -> generate_neighbors(ThisNode, NodeNumber, Degree, Neighbors);
	false -> generate_neighbors(ThisNode, NodeNumber, Degree - 1, [Neighbor|Neighbors])
    end.

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
    manhattan_distance(N1#node.x, N2#node.x, N1#node.y, N2#node.y).

manhattan_distance(X1, Y1, X2, Y2) ->
    abs(X1 - X2) + abs(Y1 - Y2).

print_nodes_pretty([]) ->
    true;
print_nodes_pretty([Node|Nodes]) ->
    io:format("~w~n", [Node]),
    print_nodes_pretty(Nodes).
    
sum_of_distances(Nodes) ->
    lists:sum([neighbor_distance(Nodes, N) || N <- Nodes]).

neighbor_distance(Nodes, Node) ->
    NeighborIds = Node#node.neighbors,
    NodeDistances = [node_distance(Node, node_lookup(Nodes, NeighborId)) || NeighborId <- NeighborIds],
    lists:sum(NodeDistances).

node_lookup(Nodes, NodeId) ->
    {_, Node} = lists:keysearch(NodeId, 2, Nodes),
    Node.
