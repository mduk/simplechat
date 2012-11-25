-module( simplechat_subscribe ).

-behaviour( gen_event ).
-export( [ init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3 ] ).

init( Pid ) when is_pid( Pid ) ->
	init( { Pid, undefined } );
init( { Pid, Key } ) when is_pid( Pid ), is_atom( Key ) ->
    { ok, { Pid, Key } }.

handle_event( Event, S = { Pid, undefined } ) ->
	Pid ! Event,
	{ ok, S };
handle_event( Event, S = { Pid, Key } ) ->
	Pid ! { Key, Event },
	{ ok, S }.

handle_call( _Msg, State ) ->
	{ ok, { error, invalid_call }, State }.

handle_info( _Msg, State ) ->
	{ ok, State }.

terminate( _Reason, _State ) ->
	ok.

code_change( _OldVsn, _State, _Opts ) ->
	ok.
