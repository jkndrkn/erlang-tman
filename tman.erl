-module(tman).
%-export([init/0, init/2]).
-compile(export_all).

-include("tman.hrl").

init() ->
    main(?NODES_DEFAULT, ?DEGREE_DEFAULT, ?CYCLES_DEFAULT, ?SIZE_DEFAULT, false).

init(Nodes, Degree) ->
    main(Nodes, Degree, ?CYCLES_DEFAULT, ?SIZE_DEFAULT, false).

init(Nodes, Degree, Cycles, Size, GraphOutput) ->
    main(Nodes, Degree, Cycles, Size, GraphOutput).

main(NodeNumber, Degree, Cycles, Size, GraphOutput) ->
    io:format("# Running T-Man with NODES ~w of DEGREE  ~w over CYCLES ~w in space of SIZE ~w ~n~n",
	      [NodeNumber, Degree, Cycles, Size]),
    io:format("# Iteration Distance~n"),
    Nodes = init_nodes(NodeNumber, Degree, Size),
    TimeStart = time_microseconds(),
    evolve(Nodes, Degree, Cycles, Size, GraphOutput),
    TimeEnd = time_microseconds(),
    TimeElapsed = TimeEnd - TimeStart,
    io:format("# TimeElapsed: ~w sec~n", [TimeElapsed / 1000000.0]).

graph_output(_, _, _, false) ->
    ok;
graph_output(Nodes, Cycle, Size, true) ->
    Fun = fun(_, N, Acc) -> string:concat(Acc, graph_node(Nodes, N, Size)) end,
    Dimensions =  [?OUTPUT_DIMENSIONS, ?OUTPUT_DIMENSIONS],
    Graph = array:foldl(Fun, "", Nodes),
    ImagickFormat = "#!/bin/bash~nconvert -size ~wx~w xc:white -fill white -stroke black \\~n~s    ~s",
    Filename = io_lib:format("~s/~w.sh", [?GRAPH_DIRECTORY, Cycle]),
    Imagename = io_lib:format("~w.gif", [Cycle]),
    ImagickData = io_lib:format(ImagickFormat, Dimensions ++ [Graph, Imagename]),
    {ok, File} = file:open(Filename, write),
    io:format(File, "~s", [ImagickData]).

graph_node(Nodes, Node, Size) ->
    Neighbors = Node#node.neighbors,
    NeighborsSublist = lists:sublist(sort_view(Node, Nodes, Neighbors), ?GRAPH_NEIGHBORS),
    lists:flatten([imagick_format(Node, node_lookup(Nodes, Neighbor), Size) || Neighbor <- NeighborsSublist]).

imagick_format(Node, Neighbor, Size) ->
    EdgeCoords = [Node#node.x, Node#node.y, Neighbor#node.x, Neighbor#node.y],
    ScalingFactor = ?OUTPUT_DIMENSIONS / Size,
    EdgeCoordsScaled = [round(X * ScalingFactor) || X <- EdgeCoords],
    io_lib:format("    -draw \"line ~w,~w ~w,~w\" \\~n", EdgeCoordsScaled).

evolve(Nodes, Degree, Cycles, Size, GraphOutput) ->
    evolve(Nodes, Degree, Cycles, Size, GraphOutput, 1).

evolve(Nodes, _, Cycles, _, _, Iteration) when Iteration > Cycles ->
    Nodes;
evolve(Nodes, Degree, Cycles, Size, GraphOutput, Iteration) ->
    graph_output(Nodes, Iteration, Size, GraphOutput),
    NodesEvolved = evolve_nodes(Nodes, Degree),
    NodesUpdated = update_nodes(Nodes, NodesEvolved),
    Distance = sum_of_distances(Nodes),
    io:format("~w ~w~n", [Iteration, Distance]),
    evolve(NodesUpdated, Degree, Cycles, Size, GraphOutput, Iteration + 1).

init_nodes(NodeNumber, Degree, Size) ->
    init_nodes(NodeNumber, Degree, Size, array:new(NodeNumber), [], NodeNumber).

init_nodes(0, Degree, _, Nodes, _, NodesTotal) ->
    init_nodes_neighbors(Nodes, Degree, NodesTotal);
init_nodes(NodeNumber, Degree, Size, Nodes, Coordinates, NodesTotal) ->
    [[X,Y],CoordinatesNew] = generate_random_unique_coordinate(NodesTotal, Size, Coordinates),
    Node = #node{id=NodeNumber,x=X,y=Y},
    NodeNumberNew = NodeNumber - 1,
    init_nodes(NodeNumberNew, Degree, Size, array:set(NodeNumberNew, Node, Nodes), CoordinatesNew, NodesTotal).

init_nodes_neighbors(Nodes, Degree, NodesTotal) ->
    init_nodes_neighbors(Nodes, Degree, NodesTotal, array:new(NodesTotal), 0).

init_nodes_neighbors(_, _, NodeNumber, NodesOutput, Index) when Index >= NodeNumber ->
    NodesOutput;
init_nodes_neighbors(NodesInput, Degree, NodeNumber, NodesOutput, Index) ->
    Node = array:get(Index, NodesInput),
    Neighbors = generate_neighbors(Node#node.id, NodeNumber, Degree),
    NodeUpdated = Node#node{neighbors=Neighbors},
    init_nodes_neighbors(NodesInput, Degree, NodeNumber, array:set(Index, NodeUpdated, NodesOutput), Index + 1).

generate_neighbors(_, NodeNumber, Degree) when Degree > NodeNumber ->
    exit(degree_too_high);
generate_neighbors(ThisNode, NodeNumber, Degree) ->
    generate_neighbors(ThisNode, NodeNumber, Degree, []).

generate_neighbors(_, _, 0, Neighbors) ->
    Neighbors;
generate_neighbors(ThisNode, NodeNumber, Degree, Neighbors) ->
    Neighbor = random:uniform(NodeNumber),
    case lists:member(Neighbor, [ThisNode|Neighbors]) of
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

manhattan_distance(X1, X2, Y1, Y2) ->
    abs(X1 - X2) + abs(Y1 - Y2).

print_nodes_pretty([]) ->
    true;
print_nodes_pretty([Node|Nodes]) ->
    io:format("~w~n", [Node]),
    print_nodes_pretty(Nodes).
    
sum_of_distances(Nodes) ->
    Sum = fun(_, Val, Acc) -> neighbor_distance(Nodes, Val) + Acc end,
    array:foldl(Sum, 0, Nodes).

neighbor_distance(Nodes, Node) ->
    NeighborIds = Node#node.neighbors,
    NodeDistances = [node_distance(Node, node_lookup(Nodes, NeighborId)) || NeighborId <- NeighborIds],
    lists:sum(NodeDistances).

node_lookup(Nodes, NodeId) ->
    array:get(NodeId - 1, Nodes).

select_peer(Node, Nodes, NeighborIds) ->
    [Peer|_] = sort_view(Node, Nodes, NeighborIds),
    Peer.

sort_view(Node, Nodes, View) ->
    SortFun = fun(N1, N2) -> node_sort_by_distance(Node, Nodes, N1, N2) end,
    lists:sort(SortFun, View).

node_sort_by_distance(Node, Nodes, N1, N2) ->
    D1 = node_distance(Node, node_lookup(Nodes, N1)),
    D2 = node_distance(Node, node_lookup(Nodes, N2)),
    D1 =< D2.    

merge_view(View1, View2) ->
    S1 = sets:from_list(View1),
    S2 = sets:from_list(View2),
    S3 = sets:union(S1, S2),
    MergeView = sets:to_list(S3),
    lists:sort(MergeView).

evolve_node(Node,Nodes,Degree) ->
    View = Node#node.neighbors,
    Peer = node_lookup(Nodes, select_peer(Node, Nodes, View)),
    Descriptor = Node#node.id,
    RandomView = random_view(Descriptor, Nodes, Degree),
    Buffer = merge_view([Descriptor], merge_view(View, RandomView)),
    {PeerUpdated, BufferPeer} = evolve_peer(Nodes,Degree,Peer,Buffer),
    ViewNew = select_view(Node, Nodes, merge_view(BufferPeer,View), Degree),
    NodeUpdated = Node#node{neighbors=ViewNew},
    [NodeUpdated, PeerUpdated].

evolve_peer(Nodes,Degree,Peer,BufferRemote) ->
    Descriptor = Peer#node.id,
    View = Peer#node.neighbors,
    RandomView = random_view(Descriptor, Nodes, Degree),
    BufferLocal = merge_view([Descriptor], merge_view(View, RandomView)),
    ViewNew = select_view(Peer, Nodes, merge_view(BufferRemote,View), Degree),
    {Peer#node{neighbors=ViewNew}, BufferLocal}.

evolve_nodes(Nodes, Degree) ->
    lists:flatten(evolve_nodes(Nodes, Degree, array:size(Nodes), [], 0)).

evolve_nodes(_, _, NodesNumber, NodesOutput, Index) when Index >= NodesNumber ->
    NodesOutput;
evolve_nodes(Nodes, Degree, NodesNumber, NodesOutput, Index) ->
    Node = array:get(Index, Nodes),
    NodeEvolved = evolve_node(Node, Nodes, Degree),
    evolve_nodes(Nodes, Degree, NodesNumber, [NodeEvolved|NodesOutput], Index + 1).

select_view(Node, Nodes, View, Degree) ->
    lists:sublist(sort_view(Node, Nodes, View), Degree).

update_nodes(Nodes, []) ->
    Nodes;
update_nodes(Nodes, [NodeUpdated|NodesUpdated]) ->
    update_nodes(update_node(Nodes, NodeUpdated), NodesUpdated).

update_node(Nodes, Node) ->
    array:set(Node#node.id - 1, Node, Nodes).

random_view(NodeId, Nodes, Degree) ->
    random_view(NodeId, Nodes, Degree, array:size(Nodes)).

random_view(_, _, Degree, NodesNumber) when Degree >= NodesNumber ->
    exit(nodes_list_too_large);
random_view(NodeId, Nodes, Degree, NodesNumber) ->
    random_view(NodeId, Nodes, Degree, NodesNumber, []).

random_view(_, _, Degree, _, View) when Degree =:= length(View) ->
     View;
random_view(NodeId, Nodes, Degree, NodesNumber, View) ->
     NodeRandom = select_random_node(Nodes),
     NodeInvalid = lists:member(NodeRandom, [NodeId|View]),
     case NodeInvalid of
	 true -> random_view(NodeId, Nodes, Degree, NodesNumber, View);
         false -> random_view(NodeId, Nodes, Degree, NodesNumber, [NodeRandom|View])
     end.

select_random_node(Nodes) ->
    random:uniform(array:size(Nodes)).

init_test_vals() ->
    Nodes = init_nodes(4, 2, 4),
    [node_lookup(Nodes, 1), Nodes].

time_microseconds() ->
    {MS, S, US} = now(),
    (MS * 1.0e+12) + (S * 1.0e+6) + US.
