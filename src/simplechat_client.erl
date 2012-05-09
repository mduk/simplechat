-module( simplechat_client ).

% This module is a chat client. A new client process
% is spawned for every client that connects.

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [ start_link/1, add_handler/3, nick/1, nick/2, active_rooms/1, joined_rooms/1, join/2, part/2, quit/1 ] ).

-record( state, { 
	connection,	% deprecated

	event,		% Instead of passing a "connection" pid, just fire events. 
				% The connection can handle it's end on it's own.
	
	nick,		% The nickname of the user
	
	rooms = []	% Propertylist of rooms the client has joined
} ).

start_link( ConnectionPid ) ->
	gen_server:start_link( ?MODULE, ConnectionPid, [] ).
	
add_handler( Client, Module, Args ) ->
	gen_server:call( Client, { add_handler, Module, Args } ).

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

quit( Client ) ->
	gen_server:cast( Client, quit ). 
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Behaviour: gen_server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init( ConnectionPid ) ->
	{ ok, EventPid } = gen_event:start_link(),
	gen_event:add_handler( EventPid, ehandler, "Client Event" ),
	{ ok, #state{
		connection = ConnectionPid,
		event = EventPid
	} }.

% Quit command
handle_cast( quit, State = #state{ rooms = Rooms } ) ->
	lists:map( fun( Room ) ->
		simplechat_room:part( Room )
	end, Rooms ),
	{ stop, quit, State };
% A room event, pass it straight on to the client event manager
handle_cast( Event = { room_event, _ }, State ) ->
	gen_event:notify( State#state.event, Event ),
	{ noreply, State };
handle_cast( { room_event, Msg = { message, _RoomName, _ClientName, _Message } }, State ) ->
	State#state.connection ! { send, Msg },
	{ noreply, State }.

% Add Handler
handle_call( { add_handler, Module, Args }, _From, State ) ->
	Result = gen_event:add_handler( State#state.event, Module, Args ),
	{ reply, Result, State };
% Ident
handle_call( { ident, Nick }, _From, State ) ->
	{ reply, ok, State#state{ nick = Nick } };
% Set Nick
handle_call( { nick, Nick }, _From, State ) ->
	{ reply, ok, State#state{ nick = Nick } };
% Get Nick 
handle_call( nick, _From, State ) ->
	{ reply, State#state.nick, State };
% Active rooms (equiv to irc /list, all server rooms)
handle_call( active_rooms, _From, State ) ->
	Rooms = lists:map( fun( { _Name, Pid } ) ->
		simplechat_room:info( Pid )
	end, simplechat_room_sup:rooms() ),
	{ reply, { ok, { active_rooms, Rooms } }, State };
% List of rooms the client has joined
handle_call( joined_rooms, _From, State = #state{ rooms = Rooms } ) ->
	{ reply, { ok, { joined_rooms, Rooms } }, State };
% Join Room
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
			Result = simplechat_room:join( RoomPid ),
			
			% Fire event
			gen_event:notify( State#state.event, { joined, { Room, RoomPid } } ),
			
			% Return join result and update state
			{ reply, Result, State#state{ rooms = [ { Room, RoomPid } | State#state.rooms ] } }
	end;
% Part Room
handle_call( { part, Room }, _From, State ) ->
	case proplists:lookup( Room, State#state.rooms ) of
	
		% Present in room, part.
		{ Room, RoomPid } ->
		
			% Part room
			Result = simplechat_room:part( RoomPid ),
			
			% Fire event
			gen_event:notify( State#state.event, { parted, { Room, RoomPid } } ),
			
			% Return part result and update state
			{ reply, Result, State#state{ rooms = proplists:delete( Room, State#state.rooms ) } };
	
		% Not present in room, ok.
		none ->
			{ reply, ok, State }
	end;
% Say something in a room
handle_call( { say, Room, Message }, _From, State ) ->
	{ ok, RoomPid } = simplechat_room_sup:room( Room ),
	Result = simplechat_room:say( RoomPid, State#state.nick, Message ),
	{ reply, Result, State };
% Catch-all
handle_call( _Msg, _From, State ) ->
	{ reply, error, State }.

handle_info( _Msg, State ) ->
	{ noreply, State }.

terminate( _Reason, _State ) ->
	ok.

code_change( _Vsn, State, _Opts ) ->
	{ ok, State }.