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
	Body = case cowboy_http_req:path( Req ) of
		{ [], _ } ->
			Rooms = simplechat_room_sup:rooms(),
			RoomsHtml = case Rooms of
				[] ->
					<<"<p>No active rooms</p>">>;
				_ ->
					ListItems = list_to_binary( lists:map( fun( { Name, _Pid } ) ->
						<<"<li><a href=\"", Name/binary, "\">", Name/binary, "</a></li>">>
					end, Rooms ) ),
					<<"<ul>", ListItems/binary, "</ul>">>
			end,
			<<"<html><head><title>Simplechat</title></head><body>", RoomsHtml/binary, "</body></html>">>;
		_ ->
			{ ok, ClientHtml } = file:read_file( "./client.html" ),
			ClientHtml
	end,
	
	Headers = [
		{ <<"Content-Type">>, <<"text/html">> }
	],
	
	{ ok, Req2 } = cowboy_http_req:reply( 200, Headers, Body, Req ),
	{ ok, Req2, S }.

terminate( _, _ ) ->
	ok.

% Behaviour: cowboy_http_websocket_handler

% Initialise websocket handler
websocket_init( _, Req, [] ) ->
	{ PeerIp, _ } = cowboy_http_req:peer_addr( Req ),
	ClientId = { PeerIp, calendar:local_time() },
	
	{ ok, ClientPid } = simplechat_client_sup:start_client( ClientId ),
	
	{ ok, cowboy_http_req:compact( Req ), #state{ 
		client_id = ClientId,
		client_pid = ClientPid
	}, hibernate }.

% Received a message over the websocket
websocket_handle( { text, Msg }, Req, State ) ->
	% Parse the JSON payload into a tuple and call it on the client
	case gen_server:call( State#state.client_pid, parse_message( Msg ) ) of
		unknown_call ->
			Reply = encode_message( { error, "Unknown client command" } ),
			{ reply, { text, list_to_binary( Reply ) }, Req, State, hibernate };
		ok ->
			{ ok, Req, State, hibernate };
		{ ok, Result } ->
			Reply = encode_message( Result ),
			{ reply, { text, list_to_binary( Reply ) }, Req, State, hibernate };
		Any ->
			Reply = encode_message( { error, io_lib:format( 
				"Unknown error occured: ~p", [ Any ] 
			) } ),
			{ reply, { text, list_to_binary( Reply ) }, Req, State, hibernate }
	end;
% Catch all websocket messages
websocket_handle( _, Req, S ) ->
	{ ok, Req, S }.

% Send raw data down the websocket
websocket_info( { send, Data }, Req, State ) when is_binary( Data ) ->
	{ reply, { text, Data }, Req, State, hibernate };
% Encode and send a message down the websocket
websocket_info( { send, Message }, Req, State ) ->
	{ reply, { text, encode_message( Message ) }, Req, State, hibernate };
% Catch all messages
websocket_info( _Msg, Req, State ) ->
	{ ok, Req, State, hibernate }.

% Connection closed
websocket_terminate( _Reason, _Req, State ) ->
	% Kill the client process
	simplechat_client_sup:terminate_client( State#state.client_id ),
	ok.

% Private functions

% Parse an 'ident' message
parse_message( { ident, Props } ) ->
	{ _, Name } = proplists:lookup( <<"name">>, Props ),
	{ ident, Name };
% Parse a 'active_rooms' message
parse_message( { active_rooms, _ } ) ->
	active_rooms;
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

% Encode an 'active_rooms' message
encode_message( { active_rooms, Rooms } ) ->
	RoomStructs = lists:map( fun( RoomProps ) ->
		{ struct, RoomProps }
	end, Rooms ),
	mochijson2:encode( { struct, [
		{ <<"type">>, <<"active_rooms">> },
		{ <<"rooms">>, RoomStructs }
	] } );
% Encode a 'joined' message
encode_message( { joined, User, Room } ) ->
	mochijson2:encode( { struct, [
		{ <<"type">>, <<"joined">> },
		{ <<"user">>, User },
		{ <<"room">>, Room }
	] } );
% Encode a 'parted' message
encode_message( { parted, User, Room } ) ->
	mochijson2:encode( { struct, [
		{ <<"type">>, <<"parted">> },
		{ <<"user">>, User },
		{ <<"room">>, Room }
	] } );
% Encode a 'message' message
encode_message( { message, Author, Body } ) ->
	mochijson2:encode( { struct, [
		{ <<"type">>, <<"message">> },
		{ <<"author">>, Author },
		{ <<"body">>, Body }
	] } );
% Encode an 'error' message
encode_message( { error, Message } ) ->
	mochijson2:encode( { struct, [
		{ <<"type">>, <<"message">> },
		{ <<"author">>, <<"Server Error">> },
		{ <<"body">>, Message }
	] } ). 

