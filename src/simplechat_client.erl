-module( simplechat_client ).

% This module is a chat client. A new client process
% is spawned for every client that connects.

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [ start_link/0, add_handler/3, nick/1, nick/2, active_rooms/1, joined_rooms/1, join/2, part/2, quit/1 ] ).

-record( state, { 
	pid,		% Client pid
	
	event,		% Client event handler
	
	nick,		% The nickname of the user
	
	rooms = []	% Propertylist of rooms the client has joined
} ).

start_link() ->
	gen_server:start_link( ?MODULE, [], [] ).
	
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
	gen_server:call( Client, quit ). 
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Behaviour: gen_server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init( _ ) ->
	{ ok, EventPid } = gen_event:start_link(),
	gen_event:add_handler( EventPid, simplechat_echohandler, "Client Event" ),
	{ ok, #state{
		pid = self(),
		event = EventPid
	} }.

%===============================================================================
% handle_cast/2
%===============================================================================

% A room event, pass it straight on to the client event manager
handle_cast( Event = { room_event, _ }, State ) ->
	gen_event:notify( State#state.event, Event ),
	{ noreply, State }.

%===============================================================================
% handle_call/3
%===============================================================================

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
			simplechat_room:join( RoomPid, State#state.nick ),
			
			% Return join result and update state
			{ reply, pending, State }
	end;
% Part Room
handle_call( { part, Room }, _From, State ) ->
	case proplists:lookup( Room, State#state.rooms ) of
	
		% Present in room, part.
		{ Room, RoomPid } ->
		
			% Part room
			simplechat_room:part( RoomPid ),
			
			{ reply, pending, State };
				
		% Not present in room, ok.
		none ->
			{ reply, ok, State }
	end;
% Say something in a room
handle_call( { say, Room, Message }, _From, State ) ->
	case proplists:lookup( Room, State#state.rooms ) of
		
		{ Room, RoomPid } ->
			Result = simplechat_room:say( RoomPid, State#state.nick, Message ),
			{ reply, Result, State };
			
		none ->
			{ reply, { error, not_present_in_room }, State }
			
	end;
% Quit command
handle_call( quit, _From, State = #state{ rooms = Rooms } ) ->
	
	part_all( Rooms ),
	gen_event:sync_notify( State#state.event, quit ),
	
	io:format( "Client stopping~n" ),
	{ stop, shutdown, ok, State#state{ rooms = [] } };
% Catch-all
handle_call( _Msg, _From, State ) ->
	{ reply, error, State }.

%===============================================================================
% handle_info/2
%===============================================================================

% A room has accepted the client's join request
handle_info( { room, { RoomName, RoomPid }, joined }, State ) ->
	
	% Fire event
	gen_event:notify( State#state.event, { joined, { RoomName, RoomPid } } ),
	
	% Return join result and update state
	{ noreply, State#state{ rooms = [ { RoomName, RoomPid } | State#state.rooms ] } };

% A room has denied the client's join request
handle_info( { room, { _RoomName, _RoomPid }, denied }, State ) ->
	{ noreply, State };

% A room has been parted
handle_info( { room, { RoomName, RoomPid }, parted }, State ) ->
	
	% Fire event
	gen_event:notify( State#state.event, { parted, { RoomName, RoomPid } } ),
	
	% Remove the room from the state
	{ noreply, State#state{ rooms = lists:delete( { RoomName, RoomPid }, State#state.rooms ) } };

handle_info( _Msg, State ) ->
	{ noreply, State }.

terminate( _Reason, _State ) ->
	ok.

code_change( _Vsn, State, _Opts ) ->
	{ ok, State }.
	
part_all( [] ) -> ok;
part_all( [ { Name, Pid } | T ] ) ->
	io:format( "Parting ~s ~p~n", [ Name, Pid ] ),
	simplechat_room:part( Pid ),
	part_all( T ).