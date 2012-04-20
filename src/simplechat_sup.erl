-module( simplechat_sup ).

-behaviour( supervisor ).
-export( [ init/1 ] ).

-export( [ start_link/0 ] ).

start_link() ->
	supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

init( _ ) ->
	{ ok, { { one_for_one, 5, 10 }, [
		{ rooms,   { simplechat_room_sup,   start_link, [] }, permanent, 5000, supervisor, [ simplechat_room_sup   ] },
		{ clients, { simplechat_client_sup, start_link, [] }, permanent, 5000, supervisor, [ simplechat_client_sup ] }
	] } }.