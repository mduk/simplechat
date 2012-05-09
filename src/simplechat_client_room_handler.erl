-module( simplechat_client_room_handler ).

-behaviour( gen_event ).
-export( [ init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3 ] ).

-record( state, { client } ).

% Initialise the client room handler. Record the client pid in the state.
init( Pid ) ->
    { ok, #state{
    	client = Pid
    } }.

% Cast all event straight to the client process
handle_event( Event, S = #state{ client = Pid } ) ->
	gen_server:cast( Pid, { room_event, Event } ),
	{ ok, S }.

handle_call( _Msg, State ) ->
	{ ok, { error, bad_query }, State }.

handle_info( _Msg, State ) ->
	{ ok, State }.

terminate( _Reason, _State ) ->
	ok.

code_change( _OldVsn, _State, _Opts ) ->
	ok.
