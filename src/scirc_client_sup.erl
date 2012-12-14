-module( scirc_client_sup ).

-behaviour( supervisor ).
-export( [ init/1 ] ).

-export( [ start_link/0, start_client/1 ] ).

start_link() ->
	supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

init( _ ) ->
	{ ok, { { one_for_one, 5, 10 }, [] } }.

start_client( Sock ) ->
	Mfa = { scirc_client, start_link, [ Sock ] },
	ChildSpec = { Sock, Mfa, permanent, 5000, worker, [ scirc_client ] },
	supervisor:start_child( ?MODULE, ChildSpec ).
