-module( simplechat_client ).

% This module is a chat client. A new client process
% is spawned for every client that connects.

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [ 
	start_link/0, 
	add_handler/3, 
	proxy_call/2, 
	nick/1, 
	nick/2, 
	active_rooms/1, 
	joined_rooms/1, 
	join/2, 
	part/2, 
	member_list/2,
	quit/1 
] ).

-record( state, { 
	pid,				% Client pid
	event,				% Client event handler
	nick = undefined,	% The nickname of the user
	rooms = []			% Propertylist of rooms the client has joined
} ).

start_link() ->
	gen_server:start_link( ?MODULE, [], [] ).
	
add_handler( Client, Module, Args ) ->
	gen_server:call( Client, { add_handler, Module, Args } ).

proxy_call( Client, Call ) ->
	{ Call, gen_server:call( Client, Call ) }.

nick( Client ) ->
	gen_server:call( Client, nick ).

nick( Client, Nick ) ->
	gen_server:call( Client, { nick, Nick } ).

active_rooms( Client ) ->
	gen_server:call( Client, active_rooms ).

joined_rooms( Client ) ->
	gen_server:call( Client, joined_rooms ).

join( Client, Room ) ->
	gen_server:call( Client, { join, Room } ).

part( Client, Room ) ->
	gen_server:call( Client, { part, Room } ).

member_list( Client, Room ) ->
	gen_server:call( Client, { member_list, Room } ).

quit( Client ) ->
	gen_server:call( Client, quit ). 
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Behaviour: gen_server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init( _ ) ->
	{ ok, EventPid } = gen_event:start_link(), 
	
	% Subscribe to server level events - this needs to be moved so the
	% client can choose when to be subscribed and to what events. eg, when the
	% server room list is visible, you would want to hear about changes in room
	% state and when a room was opened or closed. 
	simplechat_server_event:subscribe(),
	
	% Add a handler just to output events to the shell, for debugging
	gen_event:add_handler( EventPid, simplechat_echohandler, "Client Event" ),
	
	{ ok, #state{
		pid = self(),
		event = EventPid
	} }.

%===============================================================================
% handle_cast/2
%===============================================================================
handle_cast( _, State ) -> { noreply, State }.

%===============================================================================
% handle_call/3
%===============================================================================
% Add Handler
%-------------------------------------------------------------------------------
handle_call( { add_handler, Module, Args }, _From, State ) ->
	Result = gen_event:add_handler( State#state.event, Module, Args ),
	{ reply, Result, State };
%-------------------------------------------------------------------------------
% Set Nick
%-------------------------------------------------------------------------------
handle_call( { nick, Nick }, _From, State ) ->
	case valid_nick( Nick ) of
		true  -> { reply, ok, State#state{ nick = Nick } };
		false -> { reply, { error, invalid_nick }, State }
	end;
%-------------------------------------------------------------------------------
% Get Nick 
%-------------------------------------------------------------------------------
handle_call( nick, _From, State = #state{ nick=Nick } ) ->
	{ reply, { ok, Nick }, State };
%-------------------------------------------------------------------------------
% Active rooms (equiv to irc /list, all server rooms)
%-------------------------------------------------------------------------------
handle_call( room_list, _From, State ) ->
	lists:foreach( fun( { _, Room } ) ->
		simplechat_room:info( Room )
	end, simplechat_room_sup:rooms() ),
	{ reply, pending, State };
%-------------------------------------------------------------------------------
% List of rooms the client has joined
%-------------------------------------------------------------------------------
handle_call( joined_rooms, _From, State = #state{ rooms = Rooms } ) ->
	{ reply, { joined_rooms, Rooms }, State };
%-------------------------------------------------------------------------------
% Join Room
%-------------------------------------------------------------------------------
handle_call( { join, Room }, _From, State ) ->
	case proplists:lookup( Room, State#state.rooms ) of
		
		% Already present in room, ok.
		{ Room, RoomPid } ->
			{ reply, { ok, RoomPid }, State };
			
		% Not present in room, join.
		none -> 
			% Start room if not started
			{ ok, RoomPid } = simplechat_room_sup:room( Room ),
			
			% Join room
			simplechat_room:join( RoomPid, State#state.nick ),
			
			% Return join result and update state
			{ reply, pending, State }
	end;
%-------------------------------------------------------------------------------
% Part Room
%-------------------------------------------------------------------------------
handle_call( { part, Room }, _From, State ) ->
	room_call( Room, part, State ),
	{ reply, ok, State };
%-------------------------------------------------------------------------------
% Get a room member list
%-------------------------------------------------------------------------------
handle_call( { member_list, Room }, _, State ) ->
	case room_call( Room, member_list, State ) of
		{ ok, _ } -> { reply, pending, State };
		Error          -> { reply, Error, State }
	end;
%-------------------------------------------------------------------------------
% Set a room topic
%-------------------------------------------------------------------------------
handle_call( { set_topic, Room, Topic }, _From, State ) ->
	
	case proplists:lookup( Room, State#state.rooms ) of
		
		{ Room, RoomPid } ->
			Result = simplechat_room:topic( RoomPid, Topic ),
			{ reply, Result, State };
			
		none ->
			{ reply, { error, not_present_in_room }, State }
			
	end;
%-------------------------------------------------------------------------------
% Lock room topic
%-------------------------------------------------------------------------------
handle_call( { Lock, Room }, _From, State ) when Lock =:= lock_topic; Lock =:= unlock_topic ->
	case proplists:lookup( Room, State#state.rooms ) of
		
		{ Room, RoomPid } ->
			Call = case Lock of 
				lock_topic   -> lock;
				unlock_topic -> unlock
			end,
			Result = simplechat_room:topic( RoomPid, Call ),
			{ reply, Result, State };
			
		none ->
			{ reply, { error, not_present_in_room }, State }
			
	end;
%-------------------------------------------------------------------------------
% Say something in a room
%-------------------------------------------------------------------------------
handle_call( { say, Room, Message }, _From, State ) ->
	case proplists:lookup( Room, State#state.rooms ) of
		
		{ Room, RoomPid } ->
			Result = simplechat_room:say( RoomPid, Message ),
			{ reply, Result, State };
			
		none ->
			{ reply, { error, not_present_in_room }, State }
			
	end;
%-------------------------------------------------------------------------------
% Shout to all rooms
%-------------------------------------------------------------------------------
handle_call( { shout, Message }, _From, State ) ->
	#state{ rooms = Rooms } = State,
	lists:foreach( fun( { _, Room } ) ->
		simplechat_room:say( Room, Message )
	end, Rooms ),
	{ reply, pending, State };
%-------------------------------------------------------------------------------
% Quit command
%-------------------------------------------------------------------------------
handle_call( quit, _From, State = #state{ rooms = Rooms } ) ->
	part_all( Rooms ),
	gen_event:sync_notify( State#state.event, quit ),
	{ stop, shutdown, ok, State#state{ rooms = [] } };
%-------------------------------------------------------------------------------
% Catch-all
%-------------------------------------------------------------------------------
handle_call( _Msg, _From, State ) ->
	{ reply, { error, unknown_call }, State }.

%===============================================================================
% handle_info/2
%===============================================================================
% A room has been created
%-------------------------------------------------------------------------------
handle_info( E = { server_event, _ }, State ) ->
	fire( State, E ),
	{ noreply, State };
%-------------------------------------------------------------------------------
% A room has accepted the client's join request
%-------------------------------------------------------------------------------
handle_info( { room, { RoomName, RoomPid }, joined, RoomInfo }, State ) ->
	fire( State, { joined, RoomInfo } ),
	{ noreply, State#state{ rooms = [ { RoomName, RoomPid } | State#state.rooms ] } };

%-------------------------------------------------------------------------------
% A room has denied the client's join request
%-------------------------------------------------------------------------------
handle_info( { room, { RoomName, RoomPid }, denied }, State ) ->
	fire( State, { denied, { RoomName, RoomPid } } ),
	{ noreply, State };
%-------------------------------------------------------------------------------
% A room has been parted
%-------------------------------------------------------------------------------
handle_info( { room, { RoomName, RoomPid }, parted }, State ) ->
	fire( State, { parted, { RoomName, RoomPid } } ),
	{ noreply, State#state{ rooms = lists:delete( { RoomName, RoomPid }, State#state.rooms ) } };
%-------------------------------------------------------------------------------
% Room info
%-------------------------------------------------------------------------------
handle_info( { room, _, info, RoomInfo }, State ) ->
	fire( State, { room_info, RoomInfo } ),
	{ noreply, State };
%-------------------------------------------------------------------------------
% A room error
%-------------------------------------------------------------------------------
handle_info( Error = { room, _, { error, _ } }, State ) ->
	fire( State, Error ),
	{ noreply, State };
%-------------------------------------------------------------------------------
% All Room Events
%-------------------------------------------------------------------------------
handle_info( Event = { room_event, _, _ }, State ) ->
	fire( State, Event ),
	{ noreply, State };
%-------------------------------------------------------------------------------
% Catch All
%-------------------------------------------------------------------------------
handle_info( _Msg, State ) ->
	{ noreply, State }.

terminate( _Reason, _State ) ->
	ok.

code_change( _Vsn, State, _Opts ) ->
	{ ok, State }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%===============================================================================
% Fire event to room event manager
%===============================================================================
fire( S, E ) ->
	gen_event:notify( S#state.event, E ).

%===============================================================================
% Validate a nickname
%===============================================================================
% Nick is blank (empty binary)
%-------------------------------------------------------------------------------
valid_nick( <<>> ) -> false;
%-------------------------------------------------------------------------------
% Everything else is fine
%-------------------------------------------------------------------------------
valid_nick( _ ) -> true.
	
%===============================================================================
% Part all joined rooms
%===============================================================================
part_all( [ { _, Pid } | T ] ) ->
	simplechat_room:part( Pid ),
	part_all( T );
part_all( [] ) ->
	ok.
	
%===============================================================================
% Room Call
%===============================================================================
room_call( Room, Function, State ) when is_atom( Function ) ->
	room_call( Room, { Function, [] }, State );
room_call( Room, { Function, Args }, State ) when is_atom( Function ), is_list( Args ) ->
	case proplists:lookup( Room, State#state.rooms ) of
		
		{ Room, RoomPid } ->
			apply( simplechat_room, Function, [ RoomPid | Args ] );
			
		none ->
			{ error, not_present_in_room }
			
	end.
