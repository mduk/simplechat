-module( simplechat_room ).

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [ start_link/1, info/1, join/2, part/1, say/3 ] ).

-record( state, { name, event, clients = [], topic } ).

-record( member, { pid, nick } ).

start_link( Name ) when is_binary( Name ) ->
	gen_server:start_link( ?MODULE, { Name }, [] ).

join( Room, Nick ) ->
	gen_server:cast( Room, { join, self(), Nick } ).

part( Room ) ->
	gen_server:cast( Room, { part, self() } ).

say( Room, Author, Message ) ->
	gen_server:cast( Room, { message, Author, Message } ).

info( Room ) ->
	gen_server:call( Room, info ).

% Behaviour: gen_server

init( { Name } ) ->
	{ ok, Pid } = gen_event:start_link(),
	gen_event:add_handler( Pid, simplechat_echohandler, io_lib:format( "Room (~s) Event", [ Name ] ) ),
	{ ok, #state{ 
		name = Name,
		event = Pid 
	} }.

% Get room info
handle_call( info, _, State ) ->
	{ reply, [
		{ name, State#state.name },
		{ members, lists:flatlength( State#state.clients ) },
		{ topic, State#state.topic }
	], State };
handle_call( _Msg, _From, State ) ->
	{ reply, unknown_call, State }.


% Client requests to join room
handle_cast( { join, ClientPid, Nick }, State ) ->
	
	% Check to see if the client is allowed in the room
	NewState = case client_allowed( State, ClientPid ) of
		
		granted ->
			
			% Add client-room handler
			gen_event:add_handler( State#state.event, simplechat_client_room_handler, ClientPid ),
			
			% Fire joined event
			gen_event:notify( State#state.event, 
				{ joined, State#state.name, Nick } 
			),
			
			% Notify client that it joined the room
			ClientPid ! { room, { State#state.name, self() }, joined },
			
			% Create the member record
			Member = #member{ pid = ClientPid, nick = Nick },
			
			% Add the member to the room state
			State#state{ clients = [ Member | State#state.clients ] };
		
		denied ->
			% Notify the client that it was denied access to the room
			ClientPid ! { room, { State#state.name, self() }, denied },
			State
	end,
	
	{ noreply, NewState };
% Client requests to part room
handle_cast( { part, ClientPid }, State ) ->
		NewState = case client_present( State, ClientPid ) of
			
			% Client not joined, fine
			false -> ok;
			
			% Client present, part
			Member ->
				
				% Fire parted event - done first so the parting client also gets the event
				gen_event:notify( State#state.event,
					{ parted, State#state.name, Member#member.nick } 
				),
				
				% Delete client-room handler
				gen_event:delete_handler( State#state.event, simplechat_client_room_handler, ClientPid ),
				
				% Notify the client that it has parted the room
				ClientPid ! { room, { State#state.name, self() }, parted },
				
				% Remove member
				Members = lists:delete( Member, State#state.clients ),
				
				% Update state
				State#state{ clients = Members }
			end,
				
	{ noreply, NewState };
handle_cast( { message, Author, Body }, State ) ->
	gen_event:notify( State#state.event, { message, State#state.name, Author, Body } ),
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

% Private functions

% Everyone is alowed!
client_allowed( _, _ ) -> granted.

client_present( State, Pid ) ->
	lists:keyfind( Pid, #member.pid, State#state.clients ).
