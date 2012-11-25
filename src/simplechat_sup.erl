-module( simplechat_sup ).

-behaviour( supervisor ).
-export( [ init/1 ] ).

-export( [ start_link/0, start_worker/4, start_worker/3 ] ).

start_link() ->
	supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

start_worker( M, F, A ) ->
	start_worker( M, M, F, A ).

start_worker( Id, M, F, A ) ->
	Mfa = { M, F, A },
	Child = { Id,  Mfa, permanent, 5000, worker, [ M ] },
	supervisor:start_child( ?MODULE, Child ).

init( _ ) ->
	{ ok, { { one_for_one, 5, 10 }, [
		{ rooms,   { simplechat_room_sup,   start_link, [] }, permanent, 5000, supervisor, [ simplechat_room_sup   ] },
		{ clients, { simplechat_client_sup, start_link, [] }, permanent, 5000, supervisor, [ simplechat_client_sup ] }
	] } }.
