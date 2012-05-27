-module( simplechat_room ).

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [ start_link/1, info/1, join/2, part/1, say/2, topic/1, topic/2 ] ).

-record( state, { 
	name, 
	event, 
	clients = [], 
	topic = { open, <<"">> }
} ).

-record( member, { pid, nick } ).

start_link( Name ) when is_binary( Name ) ->
	gen_server:start_link( ?MODULE, { Name }, [] ).

join( Room, Nick ) ->
	gen_server:cast( Room, { join, self(), Nick } ).

part( Room ) ->
	gen_server:cast( Room, { part, self() } ).

say( Room, Message ) when is_binary( Message ) ->
	gen_server:cast( Room, { say, self(), Message } ).

topic( Room ) ->
	gen_server:call( Room, topic ).

topic( Room, Topic ) when is_binary( Topic ) ->
	gen_server:cast( Room, { set_topic, self(), Topic } );
topic( Room, lock ) ->
	gen_server:cast( Room, { lock_topic, self() } );
topic( Room, unlock ) ->
	gen_server:cast( Room, { unlock_topic, self() } ).
	
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
	{ reply, gather_room_info( State ), State };

% Get topic
handle_call( topic, _, State = #state{ topic = { _, Topic } } ) ->
	{ reply, Topic, State };
	
% Catch all
handle_call( _Msg, _From, State ) ->
	{ reply, unknown_call, State }.


% Client requests to join room
handle_cast( { join, ClientPid, Nick }, State ) ->
	
	% Check to see if the client is allowed in the room
	NewState = case client_allowed( State, ClientPid ) of
		
		granted ->
			
			% Add client-room handler
			Args = { ClientPid, { State#state.name, self() }, all },
			gen_event:add_handler( State#state.event, simplechat_client_room_handler, Args ),
			
			% Fire joined event
			fire( State, { joined, State#state.name, Nick } ),
			
			% Notify client that it joined the room
			ClientPid ! { room, { State#state.name, self() }, joined, gather_room_info( State ) },
			
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
				fire( State, { parted, State#state.name, Member#member.nick } ),
				
				% Delete client-room handler
				Args = { ClientPid, { State#state.name, self() }, all },
				gen_event:delete_handler( State#state.event, simplechat_client_room_handler, Args ),
				
				% Notify the client that it has parted the room
				ClientPid ! { room, { State#state.name, self() }, parted },
				
				% Remove member
				Members = lists:delete( Member, State#state.clients ),
				
				% Update state
				State#state{ clients = Members }
			end,
				
	{ noreply, NewState };
% Client says something to the room
handle_cast( { say, ClientPid, Message }, State ) ->
	case client_present( State, ClientPid ) of
		false -> 
			ClientPid ! { error, not_joined };
		#member{ nick=Nick } -> 
			fire( State, { message, { Nick, ClientPid }, Message } )
	end,	
	{ noreply, State };

% Set topic when open
handle_cast( { set_topic, _, Topic }, State = #state{ topic = { open, _ } } ) ->
	
	% Fire the topic_changed event
	fire( State, { topic_changed, Topic } ),
	
	{ noreply, State#state{ topic = { open, Topic } } };

% Set topic when locked
handle_cast( { set_topic, ClientPid, _ }, State = #state{ topic = { { locked, KeyHolder }, _ } } ) ->
	
	% Tell the client the topic is locked
	ClientPid ! { room, { State#state.name, self() }, { error, { topic_locked, KeyHolder } } },
	
	{ noreply, State };

% Lock topic when open
handle_cast( { lock_topic, KeyHolder }, State = #state{ topic = { open, Topic } } ) ->

	% Monitor the client process, if it dies, the topic lock should be released

	% Fire the topic_locked event
	fire( State, { topic_locked, Topic } ),
	
	{ noreply, State#state{ topic = { { locked, KeyHolder }, Topic } } };

% Lock topic when locked by caller
handle_cast( { lock_topic, KeyHolder }, State = #state{ topic = { { locked, KeyHolder }, _ } } ) ->
	{ noreply, State };

% Lock topic when locked
handle_cast( { lock_topic, ClientPid }, State = #state{ topic = { { locked, KeyHolder }, _ } } ) ->

	% Tell the client the topic is already locked
	ClientPid ! { room, { State#state.name, self() }, { error, { topic_locked, KeyHolder } } },
	
	{ noreply, State };

% Unlock topic when open
handle_cast( { unlock_topic, _ }, State = #state{ topic = { open, _ } } ) ->
	{ noreply, State };

% Unlock topic when locked by caller
handle_cast( { unlock_topic, KeyHolder }, State = #state{ topic = { { locked, KeyHolder }, Topic } } ) ->
	
	% Fire the topic_unlocked event
	fire( State, { topic_unlocked, Topic } ),
	
	{ noreply, State#state{ topic = { open, Topic } } };

% Unlock topic when locked
handle_cast( { unlock_topic, ClientPid }, State = #state{ topic = { { locked, KeyHolder }, _ } } ) ->

	% Tell the client that it doesn't have permission
	ClientPid ! { room, { State#state.name, self() }, { error, { topic_locked, KeyHolder } } },
	
	{ noreply, State };

% Catch All
handle_cast( _Msg, State ) ->
	{ noreply, State }.

handle_info( _Msg, State ) ->
	{ noreply, State }.

terminate( _Reason, _State ) ->
	ok.

code_change( _OldVsn, _State, _Extra ) ->
	ok.

% Private functions

gather_room_info( #state{ name = Name, clients = Clients, topic = { _, Topic } } ) -> 
	[
		{ pid, self() },
		{ name, Name },
		{ members, lists:flatlength( Clients ) },
		{ topic, Topic }
	].

% fire/2
%
% Fires an event, or list of events to the room event manager
fire( State, Event ) when is_tuple( Event ) ->
	gen_event:notify( State#state.event, Event );
fire( State, [ Event | Events ] ) ->
	fire( State, Event ),
	fire( State, Events );
fire( _, [] ) ->
	ok.

% client_allowed/2
% 
% Everyone is alowed!
client_allowed( _State, _ClientPid ) -> granted.

% client_present/2
%
% Determine whether or not a client is present in the room
client_present( State, Pid ) ->
	lists:keyfind( Pid, #member.pid, State#state.clients ).
