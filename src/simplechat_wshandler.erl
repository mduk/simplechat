-module( simplechat_wshandler ).

% This module mediates between the Websocket connection
% and the client processes. It handles translating the
% Json messages into client calls and vice versa.

-behaviour( cowboy_http_handler ).
-export( [ init/3, handle/2, terminate/2 ] ).

-behaviour( cowboy_http_websocket_handler ).
-export( [ websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3 ] ).

-record( state, { pid, name, client_id, client_pid } ).

% Behaviour: cowboy_http_handler

init( { _Any, http }, Req, [] ) ->
        case cowboy_http_req:header( 'Upgrade', Req ) of
                { undefined, Req2 } -> { ok, Req2, undefined };
                { <<"websocket">>, _Req2 } -> { upgrade, protocol, cowboy_http_websocket };
                { <<"WebSocket">>, _Req2 } -> { upgrade, protocol, cowboy_http_websocket }
        end.

handle( Req, S ) ->
	HttpPath = case cowboy_http_req:path( Req ) of
		{ [], _ }   -> <<"/index.html">>;
		{ Path, _ } -> convert_path( Path )
	end,
	{ ok, Req2 } = serve_file( Req, <<"www", HttpPath/binary>> ),
	{ ok, Req2, S }.

terminate( _, _ ) ->
	ok.

% serve_file/2
%
% Serves a file specified by Path
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

% mime_type/1
%
% Return a mime type for a given file extension
mime_type( <<".css">>  ) -> <<"text/css">>;
mime_type( <<".js">>   ) -> <<"application/x-javascript">>;
mime_type( <<".html">> ) -> <<"text/html">>;
mime_type( <<".png">>  ) -> <<"image/png">>;
mime_type( <<".gif">>  ) -> <<"image/gif">>;
mime_type( <<".ico">>  ) -> <<"image/x-icon">>;
mime_type( Unknown     ) ->
	io:format( "Unknown extension! ~p~n", [ Unknown ] ),
	<<"text/plain">>.

% convert_path/1
% 
% Take the parsed path list from cowboy and return a single binary of the path
convert_path( { Path, _ } ) -> convert_path( Path );
convert_path( Path )        -> convert_path( Path, [] ).

% convert_path/2
%
% Glue a list of binary path segments together
convert_path( [], Acc ) ->
	erlang:iolist_to_binary( lists:reverse( Acc ) );
convert_path( [ H | T ], Acc ) ->
	convert_path( T, [ H, <<"/">> | Acc ] ).

% Behaviour: cowboy_http_websocket_handler

% Initialise websocket handler
websocket_init( _, Req, [] ) ->
	
	% Make up a ClientId
	{ PeerIp, _ } = cowboy_http_req:peer_addr( Req ),
	ClientId = { PeerIp, calendar:local_time() },
	
	% Start the client
	{ ok, ClientPid } = simplechat_client_sup:start_client( ClientId ),
	
	% Register the client event handler
	simplechat_client:add_handler( ClientPid, simplechat_websocket_client_handler, self() ),
	
	{ ok, cowboy_http_req:compact( Req ), #state{ 
		pid = self(),
		client_id = ClientId,
		client_pid = ClientPid
	}, hibernate }.

% Received a message over the websocket
websocket_handle( { text, Msg }, Req, State ) ->
	% Parse the JSON payload into a tuple and call it on the client
	case simplechat_client:proxy_call( State#state.client_pid, simplechat_protocol:decode( Msg ) ) of
		
		% Ident error
		{ { ident, _ }, { error, invalid_nick } } ->
			self() ! close,
			{ reply, { text, simplechat_protocol:encode( { error, "Invalid Nick" } ) }, Req, State };
		
		% Ident successful
		{ { ident, _ }, ok } ->
			{ reply, { text, simplechat_protocol:encode( welcome ) }, Req, State };
		
		% Call successful, no result to return
		{ _, ok } -> 
			{ ok, Req, State, hibernate };
		
		% Call successful with a result term
		{ _, { ok, Result } } ->
			Reply = simplechat_protocol:encode( Result ),
			{ reply, { text, list_to_binary( Reply ) }, Req, State, hibernate };
		
		% Call result pending
		{ _, pending } -> 
			{ ok, Req, State, hibernate };
		
		{ _, { error, Reason } } -> 
			Reply = simplechat_protocol:encode( { error, io_lib:format(
				"Client Error: ~p", [ Reason ]
			) } ),
			{ reply, { text, Reply }, Req, State, hibernate }
	end;
% Catch all websocket messages
websocket_handle( _, Req, S ) ->
	{ ok, Req, S }.

%===============================================================================
% websocket_info/3
%===============================================================================
% Encode and send a server event
%-------------------------------------------------------------------------------
websocket_info( Msg = { server_event, _ }, Req, State ) ->
	{ reply, { text, simplechat_protocol:encode( Msg ) }, Req, State, hibernate };
%-------------------------------------------------------------------------------
% Encode and send a client event
%-------------------------------------------------------------------------------
websocket_info( Msg = { client_event, _ }, Req, State ) ->
	{ reply, { text, simplechat_protocol:encode( Msg ) }, Req, State, hibernate };
%-------------------------------------------------------------------------------
% Encode and send a room event
%-------------------------------------------------------------------------------
websocket_info( Msg = { room_event, _, _ }, Req, State ) ->
	{ reply, { text, simplechat_protocol:encode( Msg ) }, Req, State, hibernate };
%-------------------------------------------------------------------------------
% Send data down the websocket
%-------------------------------------------------------------------------------
websocket_info( { send, Data }, Req, State ) when is_binary( Data ) ->
	{ reply, { text, Data }, Req, State, hibernate };
%-------------------------------------------------------------------------------
% Encode and send a message down the websocket
%-------------------------------------------------------------------------------
websocket_info( { send, Message }, Req, State ) when is_tuple( Message ) ->
	{ reply, { text, simplechat_protocol:encode( Message ) }, Req, State, hibernate };
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
websocket_terminate( _Reason, _Req, #state{ client_pid = ClientPid } ) ->
	% Tell The client process to quit
	simplechat_client:quit( ClientPid ),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

