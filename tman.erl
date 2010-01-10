-module(tman).
%-export([init/0, init/2]).
-compile(export_all).

-define(NODES_DEFAULT, 900).
-define(NEIGHBORS_DEFAULT, 20).

-record(node, {id, x, y, random_nodes=[], structured_nodes=[]}).

init() ->
    init(?NODES_DEFAULT, ?NEIGHBORS_DEFAULT).

init(Nodes, Neighbors) ->
    main(Nodes, Neighbors).

main(Nodes, Neighbors) ->
    io:format("Running T-Man with NODES ~w NEIGHBORS ~w~n", [Nodes, Neighbors]),
    Node1 = #node{id=1,x=2,y=3},
    Node2 = #node{id=1,x=4,y=1},
    io:format("Node: ~w ~w ~w~n", [Node1, Node2, node_distance(Node1, Node2)]).

node_distance(N1, N2) ->
    abs(N1#node.x - N2#node.x) + abs(N1#node.y - N2#node.y).
