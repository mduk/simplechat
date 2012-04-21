-module( simplechat_client ).

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [ start_link/1, nick/1, nick/2, join/2, part/2, quit/1 ] ).

-record( state, { connection, nick } ).

start_link( ConnectionPid ) ->
	gen_server:start_link( ?MODULE, ConnectionPid, [] ).

nick( Client ) ->
	gen_server:call( Client, nick ).

nick( Client, Nick ) ->
	gen_server:call( Client, { nick, Nick } ).

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
	io:format( "Client ~p quitting", [ self() ] ),
	{ stop, quit, State };
handle_cast( { Action, User, Room }, State ) when Action =:= joined; Action =:= parted ->
	State#state.connection ! { send, { Action, User, Room } },
	{ noreply, State };
handle_cast( Msg = { message, _, _ }, State ) ->
	State#state.connection ! { send, Msg },
	{ noreply, State }.

handle_call( { ident, Nick }, _From, State ) ->
	{ reply, ok, State#state{ nick = Nick } };
% Set Nick
handle_call( { nick, Nick }, _From, State ) ->
	{ reply, ok, State#state{ nick = Nick } };
% Get Nick 
handle_call( nick, _From, State ) ->
	{ reply, State#state.nick, State };
% Join Room
handle_call( { join, Room }, _From, State ) ->
	Result = simplechat_room:join( binary_to_atom( Room, utf8 ) ),
	{ reply, Result, State };
% Part Room
handle_call( { part, Room }, _From, State ) ->
	Result = simplechat_room:part( binary_to_atom( Room, utf8 ) ),
	{ reply, Result, State };
% Say something in a room
handle_call( { say, Room, Message }, _From, State ) ->
	Result = simplechat_room:say( binary_to_atom( Room, utf8 ), State#state.nick, Message ),
	{ reply, Result, State };
% Catch-all
handle_call( _Msg, _From, State ) ->
	{ reply, error, State }.

handle_info( Msg, State ) ->
	io:format( "Uncaught info: ~p~n", [ Msg ] ),
	{ noreply, State }.

terminate( _Reason, _State ) ->
	ok.

code_change( _Vsn, State, _Opts ) ->
	{ ok, State }.