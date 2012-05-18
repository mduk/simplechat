-module( simplechat_wshandler ).

% This module mediates between the Websocket connection
% and the client processes. It handles translating the
% Json messages into client calls and vice versa.

-behaviour( cowboy_http_handler ).
-export( [ init/3, handle/2, terminate/2 ] ).

-behaviour( cowboy_http_websocket_handler ).
-export( [ websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3 ] ).

-record( state, { name, client_id, client_pid } ).

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
		client_id = ClientId,
		client_pid = ClientPid
	}, hibernate }.

% Received a message over the websocket
websocket_handle( { text, Msg }, Req, State ) ->
	% Parse the JSON payload into a tuple and call it on the client
	case simplechat_client:proxy_call( State#state.client_pid, parse_message( Msg ) ) of
		
		% Ident error
		{ { ident, _ }, { error, invalid_nick } } ->
			self() ! close,
			{ reply, { text, encode_message( { error, "Invalid Nick" } ) }, Req, State };
		
		{ { ident, _ }, ok } ->
			{ reply, { text, encode_message( welcome ) }, Req, State };
		
		% Call successful, no result to return
		{ _, ok } -> 
			{ ok, Req, State, hibernate };
		
		% Call successful with a result term
		{ _, { ok, Result } } ->
			Reply = encode_message( Result ),
			{ reply, { text, list_to_binary( Reply ) }, Req, State, hibernate };
		
		% Call result pending
		{ _, pending } -> 
			{ ok, Req, State, hibernate };
		
		{ _, { error, Reason } } -> 
			Reply = encode_message( { error, io_lib:format(
				"Client Error: ~p", [ Reason ]
			) } ),
			{ reply, { text, Reply }, Req, State, hibernate }
	end;
% Catch all websocket messages
websocket_handle( _, Req, S ) ->
	{ ok, Req, S }.

% Client Events
websocket_info( Msg = { client_event, _ }, Req, State ) ->
	{ reply, { text, encode_message( Msg ) }, Req, State, hibernate };
% Room Events
websocket_info( Msg = { room_event, _ }, Req, State ) ->
	{ reply, { text, encode_message( Msg ) }, Req, State, hibernate };
% Send raw data down the websocket
websocket_info( { send, Data }, Req, State ) when is_binary( Data ) ->
	{ reply, { text, Data }, Req, State, hibernate };
% Encode and send a message down the websocket
websocket_info( { send, Message }, Req, State ) when is_tuple( Message ) ->
	{ reply, { text, encode_message( Message ) }, Req, State, hibernate };
% Close the socket
websocket_info( close, Req, State ) ->
	{ shutdown, Req, State };
% Catch all messages
websocket_info( Msg, Req, State ) ->
	io:format( "Wshandler Unknown info: ~p~n", [ Msg ] ),
	{ ok, Req, State, hibernate }.

% Connection closed
websocket_terminate( _Reason, _Req, #state{ client_pid = ClientPid } ) ->
	% Tell The client process to quit
	simplechat_client:quit( ClientPid ),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Parse an 'ident' message
parse_message( { ident, Props } ) ->
	{ _, Name } = proplists:lookup( <<"name">>, Props ),
	{ ident, Name };
% Parse a 'active_rooms' message
parse_message( { active_rooms, _ } ) ->
	active_rooms;
% Parse a 'quit' message
parse_message( { quit, _ } ) ->
	quit;
% Parse a 'join' message
parse_message( { join, Props } ) ->
	{ _, Room } = proplists:lookup( <<"room">>, Props ),
	{ join, Room };
% Parse a 'part' mssage
parse_message( { part, Props } ) ->
	{ _, Room } = proplists:lookup( <<"room">>, Props ),
	{ part, Room };
% Parse a 'say' message
parse_message( { say, Props } ) ->
	{ _, Room } = proplists:lookup( <<"room">>, Props ),
    { _, Body } = proplists:lookup( <<"body">>, Props ),
	{ say, Room, Body };
% Parse a message from it's decoded json representation
parse_message( { struct, Props } ) ->
	Type = case proplists:lookup( <<"type">>, Props ) of
		none -> throw( json_lacks_type );
		{ _, TypeBin } -> binary_to_atom( TypeBin, utf8 )
	end,
	parse_message( { Type, Props } );
% Parse json payload
parse_message( JsonBin ) ->
        parse_message( mochijson2:decode( JsonBin ) ).

% ==============================================================================
% encode_message/1
% ==============================================================================
% Room Events
% ------------------------------------------------------------------------------
encode_message( { room_event, { message, Room, Client, Message } } ) ->
	mochijson2:encode( { struct, [
		{ <<"source">>, <<"room">> },
		{ <<"type">>, <<"message">> },
		{ <<"room">>, Room },
		{ <<"client">>, Client },
		{ <<"body">>, Message }
	] } );
encode_message( { room_event, { Motion, Room, Client } } ) when Motion =:= joined; Motion =:= parted ->
	mochijson2:encode( { struct, [
		{ <<"source">>, <<"room">> },
		{ <<"type">>, atom_to_binary( Motion, utf8 ) },
		{ <<"room">>, Room },
		{ <<"client">>, Client }
	] } );
% ------------------------------------------------------------------------------
% Client Events
% ------------------------------------------------------------------------------
encode_message( { client_event, { Motion, { RoomName, _ } } } ) when Motion =:= joined; Motion =:= parted ->
	mochijson2:encode( { struct, [
		{ <<"source">>, <<"client">> },
		{ <<"type">>, atom_to_binary( Motion, utf8 ) },
		{ <<"room">>, RoomName }
	] } );
encode_message( { client_event, { denied, { RoomName, _ } } } ) ->
	encode_message( { error, <<"Access to room \"", RoomName/binary, "\" denied.">> } );
% ------------------------------------------------------------------------------
% Misc
% ------------------------------------------------------------------------------
% Encode a 'welcome' message
encode_message( welcome ) ->
	mochijson2:encode( { struct, [
		{ <<"type">>, <<"welcome">> }
	] } );
% Encode an 'active_rooms' message
encode_message( { active_rooms, Rooms } ) ->
	RoomStructs = lists:map( fun( RoomProps ) ->
		{ struct, RoomProps }
	end, Rooms ),
	mochijson2:encode( { struct, [
		{ <<"type">>, <<"active_rooms">> },
		{ <<"rooms">>, RoomStructs }
	] } );
% Encode an 'error' message
encode_message( { error, Message } ) ->
	mochijson2:encode( { struct, [
		{ <<"type">>, <<"error">> },
		{ <<"title">>, <<"Server Error">> },
		{ <<"message">>, list_to_binary( Message ) }
	] } ). 

