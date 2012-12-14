-module( scirc_client ).

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [ start_link/1 ] ).

start_link( Sock ) ->
	gen_server:start_link( ?MODULE, Sock, [] ).

init( Sock ) -> 
	io:format( "Client Spawned~n" ),
	{ ok, Sock }.

handle_call( _, _, S ) -> { reply, { error, unknown_call }, S }.
handle_cast( _, S ) -> { noreply, S }.

handle_info( { tcp, _, Msg }, S ) ->
	io:format( "Got ~p~n", [ Msg ] ),
	{ noreply, S };
handle_info( _, S ) -> { noreply, S }.

terminate( _, _ ) -> ok.

code_change( _, _, S ) -> { ok, S }.