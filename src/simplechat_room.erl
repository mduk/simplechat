-module( simplechat_room ).

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [ start_link/1, join/1, part/1, say/3 ] ).

-record( state, { name, event, clients = [] } ).

start_link( Name ) when is_binary( Name ) ->
	gen_server:start_link( ?MODULE, { Name }, [] ).

join( Room ) ->
	gen_server:call( Room, { join, self() } ).

part( Room ) ->
	gen_server:call( Room, { part, self() } ).

say( Room, Author, Message ) ->
	gen_server:cast( Room, { message, Author, Message } ).

% Behaviour: gen_server

init( { Name } ) ->
	{ ok, Pid } = gen_event:start_link(),
	gen_event:add_handler( Pid, ehandler, [] ),
	{ ok, #state{ 
		name = Name,
		event = Pid 
	} }.

% Client joins room
handle_call( { join, ClientPid }, _, State ) ->
	gen_event:add_handler( State#state.event, simplechat_room_handler, ClientPid ),
	spawn( fun() -> 
		gen_event:notify( State#state.event, { joined, State#state.name, simplechat_client:nick( ClientPid ) } ) 
	end ),
	{ reply, ok, State#state{ clients = [ ClientPid | State#state.clients ] } };
% Client parts room
handle_call( { part, ClientPid }, _, State ) ->
	spawn( fun() -> 
		gen_event:notify( State#state.event, { parted, State#state.name, simplechat_client:nick( ClientPid ) } )
	end ),
	
	gen_event:delete_handler( State#state.event, simplechat_room_handler, ClientPid ),

	case lists:delete( ClientPid, State#state.clients ) of
		[] -> 
			{ stop, room_empty, ok, State#state{ clients = [] } };
		RemainingClients -> 
			{ reply, ok, State#state{ clients = RemainingClients } }
	end;
handle_call( _Msg, _From, State ) ->
	{ reply, unknown_call, State }.

handle_cast( Msg = { message, _, _ }, State ) ->
	gen_event:notify( State#state.event, Msg ),
	{ noreply, State };
handle_cast( _Msg, State ) ->
	{ noreply, State }.

handle_info( _Msg, State ) ->
	{ noreply, State }.

terminate( room_empty, State ) ->
	spawn( fun() ->
		gen_event:notify( State#state.event, { closed, room_empty } )
	end );
terminate( _Reason, _State ) ->
	ok.

code_change( _OldVsn, _State, _Extra ) ->
	ok.
