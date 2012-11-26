-module( simplechat_wshandler ).

-include("simplechat.hrl").

% This module mediates between the Websocket connection
% and the client processes. It handles translating the
% Json messages into client calls and vice versa.

-behaviour( cowboy_http_handler ).
-export( [ init/3, handle/2, terminate/2 ] ).

-behaviour( cowboy_http_websocket_handler ).
-export( [ websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3 ] ).

-record( state, { pid, name, client_pid=undefined } ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Behaviour: cowboy_http_handler
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ==============================================================================
% init/3
% ==============================================================================
init( { _Any, http }, Req, [] ) ->
	case cowboy_http_req:header( 'Upgrade', Req ) of
		{ undefined, Req2 } -> { ok, Req2, undefined };
		{ <<"websocket">>, _Req2 } -> { upgrade, protocol, cowboy_http_websocket };
		{ <<"WebSocket">>, _Req2 } -> { upgrade, protocol, cowboy_http_websocket }
	end.

% ==============================================================================
% handle/2
% ==============================================================================
handle( Req, S ) ->
	HttpPath = case cowboy_http_req:path( Req ) of
		{ [], _ }   -> <<"/index.html">>;
		{ Path, _ } -> convert_path( Path )
	end,
	{ ok, Req2 } = serve_file( Req, <<"www", HttpPath/binary>> ),
	{ ok, Req2, S }.

% ==============================================================================
% terminate/2
% ==============================================================================
terminate( _, _ ) ->
	ok.

% ==============================================================================
% serve_file/2
%
% Serves a file specified by Path
% ==============================================================================
serve_file( Req, Path ) ->
	{ Code, Headers, Body } = case filelib:is_regular( Path ) of 
		true ->
			{ ok, Bin } = file:read_file( Path ),
			{ 200, [ 
				{ <<"Content-Type">>, mime_type( filename:extension( Path ) ) } 
			], Bin };
		false ->
			{ 404, [
				{ <<"Content-Type">>, <<"text/html">> }
			], <<"<html><head><title>File Not Found</title></head><body><h1>File Not Found</h1></body></html>">> }
	end,
	
	cowboy_http_req:reply( Code, Headers, Body, Req ).

% ==============================================================================
% mime_type/1
%
% Return a mime type for a given file extension
% ==============================================================================
mime_type( <<".css">>  ) -> <<"text/css">>;
mime_type( <<".js">>   ) -> <<"application/x-javascript">>;
mime_type( <<".html">> ) -> <<"text/html">>;
mime_type( <<".png">>  ) -> <<"image/png">>;
mime_type( <<".gif">>  ) -> <<"image/gif">>;
mime_type( <<".ico">>  ) -> <<"image/x-icon">>;
mime_type( Unknown     ) ->
	io:format( "Unknown extension! ~p~n", [ Unknown ] ),
	<<"text/plain">>.

% ==============================================================================
% convert_path/1
% 
% Take the parsed path list from cowboy and return a single binary of the path
% ==============================================================================
convert_path( { Path, _ } ) -> convert_path( Path );
convert_path( Path )        -> convert_path( Path, [] ).

% ==============================================================================
% convert_path/2
%
% Glue a list of binary path segments together
% ==============================================================================
convert_path( [], Acc ) ->
	erlang:iolist_to_binary( lists:reverse( Acc ) );
convert_path( [ H | T ], Acc ) ->
	convert_path( T, [ H, <<"/">> | Acc ] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Behaviour: cowboy_http_websocket_handler
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ==============================================================================
% Initialise websocket handler
% ==============================================================================
websocket_init( _, Req, [] ) ->
	process_flag( trap_exit, true ),
	{ ok, cowboy_http_req:compact( Req ), #state{ 
		pid = self()
	}, hibernate }.

% ==============================================================================
% Received a message over the websocket
% ==============================================================================
websocket_handle( { text, Msg }, Req, State ) ->
	Packet = simplechat_protocol:decode( Msg ),
	
	case Packet of 
	
		% Ident to a client process
		{ ident, Nick, Password } ->
			case simplechat_auth:ident( Nick, Password ) of
				{ ok, Pid } ->
					simplechat_client:add_handler( Pid, simplechat_websocket_client_handler, self() ),
					Reply = simplechat_protocol:encode( welcome ),
					{ reply, { text, Reply }, Req, State#state{ client_pid = Pid } };
				{ denied, Reason } ->
					Reply = simplechat_protocol:encode( { error, Reason } ),
					{ reply, { text, Reply }, Req, State }
			end;
		
		% Retrieve the room list	
		room_list ->
			Ws = self(),
			lists:foreach( fun( Info ) ->
				spawn_link( fun() ->
					Message = simplechat_protocol:encode( Info ),
					Ws ! { send, list_to_binary( Message ) }
				end )
			end, simplechat_roomlist:get_list() ),
			{ ok, Req, State, hibernate };
		
		% Subscribe to the room list
		{ subscribe, <<"room_list">> } ->
			simplechat_roomlist:subscribe(),
			{ ok, Req, State, hibernate };
		
		% Unsubscribe from the room list
		{ unsubscribe, <<"room_list">> } ->
			simplechat_roomlist:unsubscribe(),
			{ ok, Req, State, hibernate };
			
		Packet ->
			% Parse the JSON payload into a tuple and call it on the client
			io:format( "Proxy calling: ~p~n", [ Packet ] ),
			case simplechat_client:proxy_call( State#state.client_pid, Packet ) of
				
				% Any successful result
				{ _, ok } -> 
					{ ok, Req, State, hibernate };
				
				% Any result pending
				{ _, pending } -> 
					{ ok, Req, State, hibernate };
				
				% Joined a room the client is already a member of
				{ { join, _ }, { ok, _ } } ->
					Reply = simplechat_protocol:encode( { error, "Already in room" } ),
					{ reply, { text, Reply }, Req, State, hibernate };
				
				% Any successful with a result term
				{ _, { ok, Result } } ->
					Ws = self(),
					spawn_link( fun() ->
						Reply = simplechat_protocol:encode( Result ),
						Ws ! { send, list_to_binary( Reply ) }
					end ),
					{ ok, Req, State, hibernate };
				
				% An explicit error occured
				{ Packet, { error, Reason } } -> 
					Reply = simplechat_protocol:encode( { error, io_lib:format(
						"A client error occured.~n"
						"Call was: ~p~n"
						"Error was: ~p", [ Packet, Reason ]
					) } ),
					{ reply, { text, Reply }, Req, State, hibernate };
				
				% Catch all
				{ Packet, Result } ->
					Reply = simplechat_protocol:encode( { error, io_lib:format(
						"Unknown client response.~n"
						"Call was: ~p~n"
						"Client returned: ~p", [ Packet, Result ]
					) } ),
					{ reply, { text, Reply }, Req, State, hibernate }
			end
	end;
% ==============================================================================
% Catch all websocket messages
% ==============================================================================
websocket_handle( _, Req, S ) ->
	{ ok, Req, S }.

%===============================================================================
% websocket_info/3
%===============================================================================
% Helper process termination
%-------------------------------------------------------------------------------
websocket_info( { 'EXIT', _, normal }, Req, State ) -> 
	{ ok, Req, State };
websocket_info( { 'EXIT', Pid, Reason }, Req, State ) ->
	error_logger:warning_msg( 
		"** Websocket handler ~p helper ~p crashed!~n"
		"** Reason: ~p~n", 
		[ self(), Pid, Reason ] 
	),
	self() ! { send, simplechat_protocol:encode( { error, io_lib:format(
		"Helper process crashed!~n~n~p", [ Reason ]
	) } ) },
	{ ok, Req, State };
%-------------------------------------------------------------------------------
% Encode and send a server event
%-------------------------------------------------------------------------------
websocket_info( Msg = { server_event, _ }, Req, State ) ->
	send( State, Msg ),
	{ ok, Req, State, hibernate };
%-------------------------------------------------------------------------------
% Encode and send a client event
%-------------------------------------------------------------------------------
websocket_info( Msg = { client_event, _ }, Req, State ) ->
	send( State, Msg ),
	{ ok, Req, State, hibernate };
%-------------------------------------------------------------------------------
% Encode and send a room event
%-------------------------------------------------------------------------------
websocket_info( Msg = { room_event, _, _ }, Req, State ) ->
	send( State, Msg ),
	{ ok, Req, State, hibernate };
%-------------------------------------------------------------------------------
% Encode and send a room info record
%-------------------------------------------------------------------------------
websocket_info( Msg, Req, State ) when is_record( Msg, room_info ) ->
	send( State, Msg ),
	{ ok, Req, State, hibernate };
%-------------------------------------------------------------------------------
% Send data down the websocket
%-------------------------------------------------------------------------------
websocket_info( { send, Data }, Req, State ) ->
	{ reply, { text, Data }, Req, State, hibernate };
%-------------------------------------------------------------------------------
% Close the socket
%-------------------------------------------------------------------------------
websocket_info( close, Req, State ) ->
	{ shutdown, Req, State };
%-------------------------------------------------------------------------------
% Catch all messages
%-------------------------------------------------------------------------------
websocket_info( Msg, Req, State ) ->
	io:format( "Wshandler Unknown info: ~p~n", [ Msg ] ),
	{ ok, Req, State, hibernate }.

%===============================================================================
% websocket_terminate/3
%===============================================================================
% Connection closed
%-------------------------------------------------------------------------------
websocket_terminate( _Reason, _Req, #state{ client_pid=Pid } ) ->
	simplechat_client:quit( Pid ),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%===============================================================================
% send/2
%
% Encode and queue the message for sending in a seperate helper process
%===============================================================================
send( #state{ pid = Pid }, Msg ) ->
	spawn_link( fun() ->
		Pid ! { send, simplechat_protocol:encode( Msg ) }
	end ).
