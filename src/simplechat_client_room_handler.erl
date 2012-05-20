-module( simplechat_client_room_handler ).

-behaviour( gen_event ).
-export( [ init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3 ] ).

-record( state, { client, room } ).

init( { Client, Room } ) ->
    { ok, #state{
    	client = Client,
    	room = Room
    } }.

handle_event( Event, State = #state{ client = Client, room = Room } ) ->
	gen_server:cast( Client, { room_event, Room, Event } ),
	{ ok, State }.

handle_call( _Msg, State ) ->
	{ ok, { error, bad_query }, State }.

handle_info( _Msg, State ) ->
	{ ok, State }.

terminate( _Reason, _State ) ->
	ok.

code_change( _OldVsn, _State, _Opts ) ->
	ok.
