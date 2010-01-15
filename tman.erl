-module(tman).
%-export([init/0, init/2]).
-compile(export_all).

-include("tman.hrl").

init() ->
    init(?NODES_DEFAULT, ?DEGREE_DEFAULT, ?CYCLES_DEFAULT, ?SIZE_DEFAULT).

init(Nodes, Degree) ->
    main(Nodes, Degree, ?CYCLES_DEFAULT, ?SIZE_DEFAULT).

init(Nodes, Degree, Cycles, Size) ->
    main(Nodes, Degree, Cycles, Size).

main(NodeNumber, Degree, Cycles, Size) ->
    io:format("# Running T-Man with NODES ~w of DEGREE  ~w over CYCLES ~w in space of SIZE ~w ~n~n",
	      [NodeNumber, Degree, Cycles, Size]),
    io:format("# Iteration Distance~n"),
    Nodes = init_nodes(NodeNumber, Degree, Size),
    evolve(Nodes, Degree, Cycles, Size).

evolve(Nodes, Degree, Cycles, Size) ->
    evolve(Nodes, Degree, Cycles, Size, 1).

evolve(Nodes, _, Cycles, _, Iteration) when Iteration > Cycles ->
    Nodes;
evolve(Nodes, Degree, Cycles, Size, Iteration) ->
    NodesEvolved = evolve_nodes(Nodes, Degree),
    NodesUpdated = update_nodes(Nodes, NodesEvolved),
    Distance = sum_of_distances(Nodes),
    io:format("~w ~w~n", [Iteration, Distance]),
    evolve(NodesUpdated, Degree, Cycles, Size, Iteration + 1).

init_nodes(NodeNumber, Degree, Size) ->
    init_nodes(NodeNumber, Degree, Size, [], [], NodeNumber).

% TODO: Modify system to use an array of Nodes rather than a list in an effort to improve performancex
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

manhattan_distance(X1, X2, Y1, Y2) ->
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
    tman_keyfind(NodeId, 2, Nodes).

tman_keyfind(Key, N, TupleList) ->
    {value, Tuple} = lists:keysearch(Key, N, TupleList),
    Tuple.

select_peer(Node, Nodes, NeighborIds) ->
    [Peer|_] = sort_view(Node, Nodes, NeighborIds),
    Peer.

sort_view(Node, Nodes, View) ->
    SortFun = fun(N1, N2) -> node_sort_by_distance(Node, Nodes, N1, N2) end,
    lists:sort(SortFun, View).

node_sort_by_distance(Node, Nodes, N1, N2) ->
    D1 = node_distance(Node, node_lookup(Nodes, N1)),
    D2 = node_distance(Node, node_lookup(Nodes, N2)),
    ?TRACE("N1: ~w ~w N2: ~w ~w~n", [N1, D1, N2, D2]),
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
    lists:flatten([evolve_node(Node, Nodes, Degree) || Node <- Nodes]).

select_view(Node, Nodes, View, Degree) ->
    lists:sublist(sort_view(Node, Nodes, View), Degree).

update_nodes(Nodes, []) ->
    Nodes;
update_nodes(Nodes, [NodeUpdated|NodesUpdated]) ->
    update_nodes(update_node(Nodes, NodeUpdated), NodesUpdated).

update_node(Nodes, Node) ->
    lists:keyreplace(Node#node.id, 2, Nodes, Node).

random_view(_, Nodes, Degree) when Degree >= length(Nodes) ->
    exit(nodes_list_too_large);
random_view(Node, Nodes, Degree) ->
    random_view(Node, Nodes, Degree, []).

random_view(_, _, Degree, View) when Degree =:= length(View) ->
    View;
random_view(Node, Nodes, Degree, View) ->
    NodeRandom = select_random_node(Nodes),
    case lists:member(NodeRandom, [Node|View]) of
	true -> random_view(Node, Nodes, Degree, View);
	false -> random_view(Node, Nodes, Degree, [NodeRandom|View])
    end.

select_random_node(Nodes) ->
    Node = lists:nth(random:uniform(length(Nodes)), Nodes),
    Node#node.id.
