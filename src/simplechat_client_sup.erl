-module( simplechat_client_sup ).

-behaviour( supervisor ).
-export( [ init/1 ] ).

-export( [ start_link/0, start_client/1, terminate_client/1 ] ).

start_link() ->
	supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

init( _ ) ->
	{ ok, { { one_for_one, 5, 10 }, [] } }.

start_client( Id ) ->
	Mfa = { simplechat_client, start_link, [] },
	ChildSpec = { Id, Mfa, temporary, 5000, worker, [ simplechat_client ] },
	supervisor:start_child( ?MODULE, ChildSpec ).

terminate_client( Id ) ->
	supervisor:terminate_child( ?MODULE, Id ).
