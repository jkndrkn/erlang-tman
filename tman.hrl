-define(NODES_DEFAULT, 900).
-define(DEGREE_DEFAULT, 20).
-define(CYCLES_DEFAULT, 50).
-define(SIZE_DEFAULT, 30).

-ifdef(debug).
-define(TRACE(Format, Data), io:format(string:concat("TRACE ~p:~p ", Format), [?MODULE, ?LINE] ++ Data)).
-else.
-define(TRACE(Format, Data), void).
-endif.

-record(node, {id, x, y, neighbors=[]}).
