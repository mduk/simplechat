-module( simplechat_client_room_handler ).

-behaviour( gen_event ).
-export( [ init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3 ] ).

-record( state, { client, room, subscribed_events } ).

init( { Client, Room, Subscribed } ) ->
	{ ok, #state{
		client = Client,
		room = Room,
		subscribed_events = Subscribed
	} }.

% Client is subscribed to everything
handle_event( Event, State = #state{ client = Client, room = Room, subscribed_events = all } ) ->
	gen_server:cast( Client, { room_event, Room, Event } ),
	{ ok, State };

% Client is only subscribed to a subset of event types
handle_event( Event, State = #state{ subscribed_events = Subscribed } ) ->
	case lists:member( element( 1, Event ), Subscribed ) of
		true  -> 
			#state{ client = Client, room = Room } = State,
			gen_server:cast( Client, { room_event, Room, Event } );
		false -> 
			ok
	end,
	{ ok, State }.

handle_call( _Msg, State ) ->
	{ ok, { error, bad_query }, State }.

handle_info( _Msg, State ) ->
	{ ok, State }.

terminate( _Reason, _State ) ->
	ok.

code_change( _OldVsn, _State, _Opts ) ->
	ok.
