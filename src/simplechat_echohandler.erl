-module( simplechat_echohandler ).

-behaviour( gen_event ).
-export( [ init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3 ] ).

init( Banner ) ->
    { ok, Banner }.

handle_event( Message, Banner ) ->
	io:format( "~s: ~p~n", [ Banner, Message ] ),
	{ ok, Banner }.

handle_call( _Msg, State ) ->
	{ ok, { error, bad_query }, State }.

handle_info( _Msg, State ) ->
	{ ok, State }.

terminate( _Reason, _State ) ->
	ok.

code_change( _OldVsn, _State, _Opts ) ->
	ok.
