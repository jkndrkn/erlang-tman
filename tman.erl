-module(tman).
-compile(export_all).

-include("tman.hrl").

% Executes T-Man algorithm with default specified in tman.hrl
init() ->
    main(?NODES_DEFAULT,?DEGREE_DEFAULT,?CYCLES_DEFAULT,?SIZE_DEFAULT,false).

% Executes T-Man algorithm with user-supplied quanitity of nodes and node degree
init(Nodes,Degree) ->
    main(Nodes,Degree,?CYCLES_DEFAULT,?SIZE_DEFAULT,false).

% Executes T-Man algorithm with fully-user-supplied parameters:
% node quantity, node degree, algorithm cycles, space size, and graph output toggle
init(Nodes,Degree,Cycles,Size,GraphOutput) ->
    main(Nodes,Degree,Cycles,Size,GraphOutput).

% Initial node setup and execution of T-Man evolution. Includes timing functions.
main(NodeNumber,Degree,Cycles,Size,GraphOutput) ->
    io:format("# Running T-Man with NODES ~w of DEGREE  ~w over CYCLES ~w in space of SIZE ~w ~n~n",
	      [NodeNumber,Degree,Cycles,Size]),
    io:format("# Iteration Distance~n"),

    % Initialize nodes
    Nodes = init_nodes(NodeNumber,Degree,Size),
    TimeStart = time_microseconds(),

    % Call T-Man algorithm
    evolve(Nodes,Degree,Cycles,Size,GraphOutput),
    TimeEnd = time_microseconds(),
    TimeElapsed = TimeEnd - TimeStart,
    io:format("# TimeElapsed: ~w sec~n",[TimeElapsed / 1000000.0]).

% Generates a BASH script that calls Imagemagick to produce a graphical display of nodes and node edges
graph_output(_,_,_,false) ->
    ok;
graph_output(Nodes,Cycle,Size,true) ->
    % Lambda function used by array:foldl to generate edges
    Fun = fun(_,N,Acc) -> string:concat(Acc,graph_node(Nodes,N,Size)) end,
    Dimensions =  [?OUTPUT_DIMENSIONS,?OUTPUT_DIMENSIONS],

    % Generate node edges
    Graph = array:foldl(Fun,"",Nodes),
    ImagickFormat = "#!/bin/bash~nconvert -size ~wx~w xc:white -fill white -stroke black \\~n~s    ~s",
    Filename = io_lib:format("~s/~w.sh",[?GRAPH_DIRECTORY,Cycle]),
    Imagename = io_lib:format("~w.gif",[Cycle]),
    ImagickData = io_lib:format(ImagickFormat,Dimensions ++ [Graph,Imagename]),
    {ok,File} = file:open(Filename,write),
    io:format(File,"~s",[ImagickData]).

% Return all edges corresponding to Node
graph_node(Nodes,Node,Size) ->
    Neighbors = Node#node.neighbors,
    NeighborsSublist = lists:sublist(sort_view(Node,Nodes,Neighbors),?GRAPH_NEIGHBORS),
    lists:flatten([imagick_format(Node,node_lookup(Nodes,Neighbor),Size) || Neighbor <- NeighborsSublist]).

% Generate a node edge anchored at Node and Neighbor coordinates
imagick_format(Node,Neighbor,Size) ->
    EdgeCoords = [Node#node.x,Node#node.y,Neighbor#node.x,Neighbor#node.y],
    ScalingFactor = ?OUTPUT_DIMENSIONS / Size,
    EdgeCoordsScaled = [round(X * ScalingFactor) || X <- EdgeCoords],
    io_lib:format("    -draw \"line ~w,~w ~w,~w\" \\~n",EdgeCoordsScaled).

% Main T-Man evolution function. Executes Cycles number of times.
evolve(Nodes,Degree,Cycles,Size,GraphOutput) ->
    evolve(Nodes,Degree,Cycles,Size,GraphOutput,1).

evolve(Nodes,_,Cycles,_,_,Iteration) when Iteration > Cycles ->
    Nodes;
evolve(Nodes,Degree,Cycles,Size,GraphOutput,Iteration) ->
    % Generate graph output if GraphOutput set to true
    graph_output(Nodes,Iteration,Size,GraphOutput),

    % Evolve all nodes in the node list
    NodesEvolved = evolve_nodes(Nodes,Degree),
    
    % Update the node list using NodesEvolved
    NodesUpdated = update_nodes(Nodes,NodesEvolved),

    % Distance is the computed sum_of_distances
    Distance = sum_of_distances(Nodes),
    io:format("~w ~w~n",[Iteration,Distance]),
    evolve(NodesUpdated,Degree,Cycles,Size,GraphOutput,Iteration + 1).

% Returns an array of Node records
init_nodes(NodeNumber,Degree,Size) ->
    init_nodes(NodeNumber,Degree,Size,array:new(NodeNumber),[],NodeNumber).

init_nodes(0,Degree,_,Nodes,_,NodesTotal) ->
    init_nodes_neighbors(Nodes,Degree,NodesTotal);
init_nodes(NodeNumber,Degree,Size,Nodes,Coordinates,NodesTotal) ->
    [[X,Y],CoordinatesNew] = generate_random_unique_coordinate(NodesTotal,Size,Coordinates),
    % Create new Node with random unique coordinates X and Y and sequential ID NodeNumber
    Node = #node{id=NodeNumber,x=X,y=Y},
    NodeNumberNew = NodeNumber - 1,
    init_nodes(NodeNumberNew,Degree,Size,array:set(NodeNumberNew,Node,Nodes),CoordinatesNew,NodesTotal).

% Assign a randomly created list of neighbors to each Node in Nodes
init_nodes_neighbors(Nodes,Degree,NodesTotal) ->
    init_nodes_neighbors(Nodes,Degree,NodesTotal,array:new(NodesTotal),0).

init_nodes_neighbors(_,_,NodeNumber,NodesOutput,Index) when Index >= NodeNumber ->
    NodesOutput;
init_nodes_neighbors(NodesInput,Degree,NodeNumber,NodesOutput,Index) ->
    Node = array:get(Index,NodesInput),
    
    % Generate random neighbors list
    Neighbors = generate_neighbors(Node#node.id,NodeNumber,Degree),
    NodeUpdated = Node#node{neighbors=Neighbors},
    init_nodes_neighbors(NodesInput,Degree,NodeNumber,array:set(Index,NodeUpdated,NodesOutput),Index + 1).

% Generate random neighbors list
generate_neighbors(_,NodeNumber,Degree) when Degree > NodeNumber ->
    exit(degree_too_high);
generate_neighbors(ThisNode,NodeNumber,Degree) ->
    generate_neighbors(ThisNode,NodeNumber,Degree,[]).

generate_neighbors(_,_,0,Neighbors) ->
    Neighbors;
generate_neighbors(ThisNode,NodeNumber,Degree,Neighbors) ->
    Neighbor = random:uniform(NodeNumber),
    case lists:member(Neighbor,[ThisNode|Neighbors]) of
	true -> generate_neighbors(ThisNode,NodeNumber,Degree,Neighbors);
	false -> generate_neighbors(ThisNode,NodeNumber,Degree - 1,[Neighbor|Neighbors])
    end.

% Generate a random Node coordinate that is not already occupied
generate_random_unique_coordinate(NodeNumber,Size,_) when (NodeNumber > Size * Size) ->
    exit(too_many_nodes);
generate_random_unique_coordinate(NodeNumber,_,Coordinates) when (length(Coordinates) >= NodeNumber) ->
    exit(available_coordinates_exhausted);
generate_random_unique_coordinate(NodeNumber,Size,Coordinates) ->
    generate_random_unique_coordinate(NodeNumber,Size,Coordinates,[]).

generate_random_unique_coordinate(NodeNumber,Size,Coordinates,[]) ->
    Coord = [generate_random_coordinate(Size),generate_random_coordinate(Size)],
    case coordinate_is_unique(Coord,Coordinates) of
	true -> [Coord,[Coord|Coordinates]];
	false -> generate_random_unique_coordinate(NodeNumber,Size,Coordinates,[])
    end.

% Generate a random coordinate in range [0, Size - 1]
generate_random_coordinate(Size) ->
    random:uniform(Size) - 1.

% Return true if a given Coord is not found in Coordinates
coordinate_is_unique(Coord,Coordinates) ->
    not lists:member(Coord,Coordinates).

% Returns manhattan distance of Node N1 and Node N2
node_distance(N1,N2) ->
    manhattan_distance(N1#node.x,N2#node.x,N1#node.y,N2#node.y).

% Return manhattan distance
manhattan_distance(X1,X2,Y1,Y2) ->
    abs(X1 - X2) + abs(Y1 - Y2).

% Return the sum of distances of the Nodes array
sum_of_distances(Nodes) ->
    Sum = fun(_,Val,Acc) -> neighbor_distance(Nodes,Val) + Acc end,
    array:foldl(Sum,0,Nodes).

% Return the sum of distances of a given Node and all its neighbors
neighbor_distance(Nodes,Node) ->
    NeighborIds = Node#node.neighbors,
    NodeDistances = [node_distance(Node,node_lookup(Nodes,NeighborId)) || NeighborId <- NeighborIds],
    lists:sum(NodeDistances).

% Return a given Node in Nodes by its unique ID
node_lookup(Nodes,NodeId) ->
    array:get(NodeId - 1,Nodes).

% Return neighbor in NeighborIds nearest to Node
select_peer(Node,Nodes,NeighborIds) ->
    [Peer|_] = sort_view(Node,Nodes,NeighborIds),
    Peer.

% Sort a given View (list of neighbors) by nearest neighbor first
sort_view(Node,Nodes,View) ->
    SortFun = fun(N1,N2) -> node_sort_by_distance(Node,Nodes,N1,N2) end,
    lists:sort(SortFun,View).

% Sort function used in sort_view/3
node_sort_by_distance(Node,Nodes,N1,N2) ->
    D1 = node_distance(Node,node_lookup(Nodes,N1)),
    D2 = node_distance(Node,node_lookup(Nodes,N2)),
    D1 =< D2.    

% Combine View1 and View2. Uses set operations to remove duplicate nodes.
merge_view(View1,View2) -> 
    S1 = sets:from_list(View1),
    S2 = sets:from_list(View2),
    S3 = sets:union(S1,S2),
    MergeView = sets:to_list(S3),
    lists:sort(MergeView).

% Evolve a given Node. This function corresponds to Fig. 1 (a) in the T-Man paper.
evolve_node(Node,Nodes,Degree) ->
    View = Node#node.neighbors,
    Peer = node_lookup(Nodes,select_peer(Node,Nodes,View)),
    Descriptor = Node#node.id,
    RandomView = random_view(Descriptor,Nodes,Degree),
    Buffer = merge_view([Descriptor],merge_view(View,RandomView)),
    {PeerUpdated,BufferPeer} = evolve_peer(Nodes,Degree,Peer,Buffer),
    ViewNew = select_view(Node,Nodes,merge_view(BufferPeer,View),Degree),
    NodeUpdated = Node#node{neighbors=ViewNew},
    [NodeUpdated,PeerUpdated].

% Evolve a Peer of Node selected in evolve_node/3. Corresponds to Fig. 1 (b).
evolve_peer(Nodes,Degree,Peer,BufferRemote) ->
    Descriptor = Peer#node.id,
    View = Peer#node.neighbors,
    RandomView = random_view(Descriptor,Nodes,Degree),
    BufferLocal = merge_view([Descriptor],merge_view(View,RandomView)),
    ViewNew = select_view(Peer,Nodes,merge_view(BufferRemote,View),Degree),
    {Peer#node{neighbors=ViewNew},BufferLocal}.

% Apply one iteration of sequential T-Man algorithm to Nodes
evolve_nodes(Nodes,Degree) ->
    lists:flatten(evolve_nodes(Nodes,Degree,array:size(Nodes),[],0)).

evolve_nodes(_,_,NodesNumber,NodesOutput,Index) when Index >= NodesNumber ->
    NodesOutput;
evolve_nodes(Nodes,Degree,NodesNumber,NodesOutput,Index) ->
    Node = array:get(Index,Nodes),
    NodeEvolved = evolve_node(Node,Nodes,Degree),
    evolve_nodes(Nodes,Degree,NodesNumber,[NodeEvolved|NodesOutput],Index + 1).

% Accepts a View, sorts it by proximity to Node, and truncates it to length Degree
select_view(Node,Nodes,View,Degree) ->
    lists:sublist(sort_view(Node,Nodes,View),Degree).

% Accept Nodes and an array of updated nodes and updates Nodes to reflect evolution in the network.
update_nodes(Nodes,[]) ->
    Nodes;
update_nodes(Nodes,[NodeUpdated|NodesUpdated]) ->
    update_nodes(update_node(Nodes,NodeUpdated),NodesUpdated).

% Update a given Node in Nodes
update_node(Nodes,Node) ->
    array:set(Node#node.id - 1,Node,Nodes).

% Generate a random view (neighbor list) of size Degree
random_view(NodeId,Nodes,Degree) ->
    random_view(NodeId,Nodes,Degree,array:size(Nodes)).

random_view(_,_,Degree,NodesNumber) when Degree >= NodesNumber ->
    exit(nodes_list_too_large);
random_view(NodeId,Nodes,Degree,NodesNumber) ->
    random_view(NodeId,Nodes,Degree,NodesNumber,[]).

random_view(_,_,Degree,_,View) when Degree =:= length(View) ->
     View;
random_view(NodeId,Nodes,Degree,NodesNumber,View) ->
     NodeRandom = select_random_node(Nodes),
     NodeInvalid = lists:member(NodeRandom,[NodeId|View]),
     case NodeInvalid of
	 true -> random_view(NodeId,Nodes,Degree,NodesNumber,View);
         false -> random_view(NodeId,Nodes,Degree,NodesNumber,[NodeRandom|View])
     end.

% Return a random Node in Nodes.
select_random_node(Nodes) ->
    random:uniform(array:size(Nodes)).

% Debugging function used during testing
init_test_vals() ->
    Nodes = init_nodes(4,2,4),
    [node_lookup(Nodes,1),Nodes].

% Returns number of microseconds since the Unix epoch
time_microseconds() ->
    {MS,S,US} = now(),
    (MS * 1.0e+12) + (S * 1.0e+6) + US.
