-module(tman).
%-export([init/0, init/2]).
-compile(export_all).

-define(NODES_DEFAULT, 900).
-define(NEIGHBORS_DEFAULT, 20).

init() ->
    init(?NODES_DEFAULT, ?NEIGHBORS_DEFAULT).

init(Nodes, Neighbors) ->
    main(Nodes, Neighbors).

main(Nodes, Neighbors) ->
    io:format("Running T-Man with NODES ~w NEIGHBORS ~w~n", [Nodes, Neighbors]).
