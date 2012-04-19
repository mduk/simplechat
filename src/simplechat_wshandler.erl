-module( simplechat_wshandler ).

-behaviour( cowboy_http_handler ).
-export( [ init/3, handle/2, terminate/2 ] ).

-behaviour( cowboy_http_websocket_handler ).
-export( [ websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3 ] ).

-record( state, { } ).

% Behaviour: cowboy_http_handler

init( { _Any, http }, Req, [] ) ->
        case cowboy_http_req:header( 'Upgrade', Req ) of
                { undefined, Req2 } -> { ok, Req2, undefined };
                { <<"websocket">>, _Req2 } -> { upgrade, protocol, cowboy_http_websocket };
                { <<"WebSocket">>, _Req2 } -> { upgrade, protocol, cowboy_http_websocket }
        end.

handle( Req, S ) ->
	{ PeerIp, _ } = cowboy_http_req:peer_addr( Req ),
	io:format( "Serving client HTML to ~p~n", [ PeerIp ] ),
	
	{ ok, Html } = file:read_file( "./client.html" ),
	
	Headers = [
		{ <<"Content-Type">>, <<"text/html">> }
	],
	
	{ ok, Req2 } = cowboy_http_req:reply( 200, Headers, Html, Req ),
	{ ok, Req2, S }.

terminate( _, _ ) ->
	ok.

% Behaviour: cowboy_http_websocket_handler

websocket_init( _, Req, [] ) ->
	{ PeerIp, _ } = cowboy_http_req:peer_addr( Req ),
	io:format( "User ~p connecting via websocket ~p~n", [ PeerIp, self() ] ),
	simplechat_room:join( default_room ),
	{ ok, cowboy_http_req:compact( Req ), #state{}, hibernate }.

% Send messages received straight back to the websocket client
websocket_handle( { text, Msg }, Req, State ) ->
	
	{ PeerIp, _ } = cowboy_http_req:peer_addr( Req ),
	io:format( "User ~p sent ~p over websocket~n", [ PeerIp, Msg ] ),
	
	Parsed = parse_message( Msg ),
	io:format( "Casting to room ~p: ~p~n", [ whereis( default_room ), Parsed ] ),
	gen_event:notify( default_room, Parsed ),
	
	{ ok, Req, State, hibernate };
websocket_handle( _, Req, S ) ->
	{ ok, Req, S }.

websocket_info( ping, Req, State ) ->
	{ reply, { text, <<"{ \"author\":\"Server\", \"body\":\"Ping!\" }">> }, Req, State, hibernate };
websocket_info( { send, Data }, Req, State ) when is_binary( Data ) ->
	{ reply, { text, Data }, Req, State, hibernate };
websocket_info( { send, Message }, Req, State ) ->
	{ reply, { text, encode_message( Message ) }, Req, State, hibernate };
websocket_info( _Msg, Req, State ) ->
	{ ok, Req, State, hibernate }.

websocket_terminate( _Reason, Req, #state{} ) ->
	{ PeerIp, _ } = cowboy_http_req:peer_addr( Req ),
	io:format( "User ~p closed websocket connection~n", [ PeerIp ] ),
	simplechat_room:part( default_room ),
	ok.

% Private functions

% Parse a message from it's json representation
parse_message( { struct, Props } ) ->
	Type = case proplists:lookup( <<"type">>, Props ) of
		none -> throw( json_lacks_type );
		{ _, TypeBin } -> binary_to_atom( TypeBin, utf8 )
	end,
	parse_message( { Type, Props } );
parse_message( { join, Props } ) ->
	{ _, Room } = proplists:lookup( <<"room">>, Props ),
	{ join, Room };
parse_message( { part, Props } ) ->
	{ _, Room } = proplists:lookup( <<"room">>, Props ),
	{ part, Room };
parse_message( { message, Props } ) ->
	{ _, Author } = proplists:lookup( <<"author">>, Props ),
        { _, Body } = proplists:lookup( <<"body">>, Props ),
	{ message, Author, Body };
parse_message( JsonBin ) ->
        parse_message( mochijson2:decode( JsonBin ) ).

% Encode a message into it's json representation
encode_message( { joined, User, Room } ) ->
	mochijson2:encode( { struct, [
		{ <<"type">>, <<"joined">> },
		{ <<"user">>, User },
		{ <<"room">>, Room }
	] } );
encode_message( { parted, User, Room } ) ->
	mochijson2:encode( { struct, [
		{ <<"type">>, <<"parted">> },
		{ <<"user">>, User },
		{ <<"room">>, Room }
	] } );
encode_message( { message, Author, Body } ) ->
	mochijson2:encode( { struct, [
		{ <<"type">>, <<"message">> },
		{ <<"author">>, Author },
                { <<"body">>, Body }
	] } ).

