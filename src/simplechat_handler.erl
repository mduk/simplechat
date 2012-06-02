-module( simplechat_handler ).

-behaviour( gen_event ).
-export( [ init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3 ] ).

init( ClientPid ) ->
	{ ok, ClientPid }.

handle_event( Event, ClientPid ) ->
	io:format( "Relaying server event ~p to client ~p~n", [ Event, ClientPid ] ),
	ClientPid ! Event,
	{ ok, ClientPid }.
	
handle_call( _Msg, State ) ->
	{ ok, { error, bad_query }, State }.

handle_info( _Msg, State ) ->
	{ ok, State }.

terminate( _Reason, _State ) ->
	ok.

code_change( _OldVsn, _State, _Opts ) ->
	ok.
