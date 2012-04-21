-module( simplechat_room_sup ).

-behaviour( supervisor ).
-export( [ init/1 ] ).

-export( [ start_link/0, start_room/1 ] ).

start_link() ->
	supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

init( _ ) ->
	{ ok, { { one_for_one, 5, 10 }, [] } }.
	
start_room( Name ) ->
	io:format( "Creating room: ~s~n", [ Name ] ),
	supervisor:start_child( ?MODULE, { Name, { simplechat_room, start_link, [] }, permanent, 5000, worker, [ simplechat_room ] } ).
