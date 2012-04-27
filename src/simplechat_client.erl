-module( simplechat_client ).

% This module is a chat client. A new client process
% is spawned for every client that connects.

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [ start_link/1, nick/1, nick/2, active_rooms/1, join/2, part/2, quit/1 ] ).

-record( state, { connection, nick } ).

start_link( ConnectionPid ) ->
	gen_server:start_link( ?MODULE, ConnectionPid, [] ).

nick( Client ) ->
	gen_server:call( Client, nick ).

nick( Client, Nick ) ->
	gen_server:call( Client, { nick, Nick } ).
	
active_rooms( Client ) ->
	gen_server:call( Client, active_rooms ).

join( Client, Room ) ->
	gen_server:call( Client, { join, Room } ).

part( Client, Room ) ->
	gen_server:call( Client, { part, Room } ).

quit( Client ) ->
	gen_server:cast( Client, quit ). 

% Behaviour: gen_server
init( ConnectionPid ) ->
	{ ok, #state{ connection = ConnectionPid } }.

handle_cast( quit, State ) ->
	{ stop, quit, State };
handle_cast( { Action, Room, User }, State ) when Action =:= joined; Action =:= parted ->
	State#state.connection ! { send, { Action, User, Room } },
	{ noreply, State };
handle_cast( Msg = { message, _, _ }, State ) ->
	State#state.connection ! { send, Msg },
	{ noreply, State }.

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
	Rooms = lists:map( fun( { Name, Pid } ) ->
		simplechat_room:info( Pid )
	end, simplechat_room_sup:rooms() ),
	{ reply, { ok, { active_rooms, Rooms } }, State };
% Join Room
handle_call( { join, Room }, _From, State ) ->
	{ ok, RoomPid } = simplechat_room_sup:room( Room ),
	Result = simplechat_room:join( RoomPid ),
	{ reply, Result, State };
% Part Room
handle_call( { part, Room }, _From, State ) ->
	{ ok, RoomPid } = simplechat_room_sup:room( Room ),
	Result = simplechat_room:part( RoomPid ),
	{ reply, Result, State };
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
