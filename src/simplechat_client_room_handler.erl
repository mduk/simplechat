% Pass events from the room event manager to the client process
%
% This can either just pass across all events indiscriminately,
% or a list of specific event types that the client is interested
% can be specified and only those events are passed. The difference
% between being a fully fledged member of the room, or just observing
% the state of the room.

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
	Client ! { room_event, Room, Event },
	{ ok, State };

% Client is only subscribed to a subset of event types
handle_event( Event, State = #state{ subscribed_events = Subscribed } ) ->
	case lists:member( element( 1, Event ), Subscribed ) of
		true  -> 
			#state{ client = Client, room = Room } = State,
			Client ! { room_event, Room, Event };
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
