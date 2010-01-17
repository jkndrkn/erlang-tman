% Constants
-define(NODES_DEFAULT, 900).
-define(DEGREE_DEFAULT, 20).
-define(CYCLES_DEFAULT, 50).
-define(SIZE_DEFAULT, 30).
-define(GRAPH_DIRECTORY, "graphs").
-define(OUTPUT_DIMENSIONS, 600).
-define(GRAPH_NEIGHBORS, 4).

% Debugging function
-ifdef(debug).
-define(TRACE(Format, Data), io:format(string:concat("TRACE ~p:~p ", Format), [?MODULE, ?LINE] ++ Data)).
-else.
-define(TRACE(Format, Data), void).
-endif.

% Node record definition
-record(node, {id, x, y, neighbors=[]}).
