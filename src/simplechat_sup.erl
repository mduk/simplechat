-module( simplechat_sup ).

-behaviour( supervisor ).
-export( [ init/1 ] ).

-export( [ start_link/0, event/0 ] ).

start_link() ->
	supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

event() ->
	case proplists:lookup( events, supervisor:which_children( ?MODULE ) ) of
		{ _, Pid, _, _ } -> Pid;
		_ -> throw( no_server_event_manager )
	end.

init( _ ) ->
	{ ok, { { one_for_one, 5, 10 }, [
		{ events,  { gen_event,             start_link, [] }, permanent, 5000, supervisor, [ gen_event             ] },
		{ rooms,   { simplechat_room_sup,   start_link, [] }, permanent, 5000, supervisor, [ simplechat_room_sup   ] },
		{ clients, { simplechat_client_sup, start_link, [] }, permanent, 5000, supervisor, [ simplechat_client_sup ] }
	] } }.
