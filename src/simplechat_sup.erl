-module( simplechat_sup ).

-behaviour( supervisor ).
-export( [ init/1 ] ).

-export( [ start_link/0, start_room/1 ] ).

start_link() ->
	supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

init( _ ) ->
	{ ok, { { one_for_one, 5, 10 }, [
		{ rooms, { simplechat_room_sup, start_link, [] }, permanent, 5000, supervisor, [ simplechat_room_sup ] }
	] } }.
	
start_room( Id ) ->
	supervisor:start_child( ?MODULE, { Id, { gen_event, start_link, [] }, permanent, 5000, worker, [ gen_event ] } ).
