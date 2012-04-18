-module( wshandler ).

-behaviour( cowboy_http_handler ).
-export( [ init/3, handle/2, terminate/2 ] ).

-behaviour( cowboy_http_websocket_handler ).
-export( [ websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3 ] ).

% Behaviour: cowboy_http_handler

init( { _Any, http }, Req, [] ) ->
        case cowboy_http_req:header( 'Upgrade', Req ) of
                { undefined, Req2 } -> { ok, Req2, undefined };
                { <<"websocket">>, _Req2 } -> { upgrade, protocol, cowboy_http_websocket };
                { <<"WebSocket">>, _Req2 } -> { upgrade, protocol, cowboy_http_websocket }
        end.

handle( Req, S ) ->
	{ ok, Html } = file:read_file( "./test.html" ),
	
	Headers = [
		{ <<"Content-Type">>, <<"text/html">> }
	],
	
	{ ok, Req2 } = cowboy_http_req:reply( 200, Headers, Html, Req ),
	{ ok, Req2, S }.

terminate( _, _ ) ->
	ok.

% Behaviour: cowboy_http_websocket_handler

websocket_init( _, Req, [] ) ->
	timer:send_interval( 5000, ping ),
	{ ok, cowboy_http_req:compact( Req ), undefined, hibernate }.

websocket_handle( { text, Msg }, Req, S ) ->
	{ reply, { text, Msg }, Req, S, hibernate };
websocket_handle( _, Req, S ) ->
	{ ok, Req, S }.

websocket_info( ping, Req, State ) ->
	{ reply, { text, <<"{ \"author\":\"Server\", \"body\":\"Ping!\" }">> }, Req, State, hibernate };

websocket_info( _Msg, Req, State ) ->
	{ ok, Req, State, hibernate }.

websocket_terminate( _Reason, _Req, _State ) ->
	ok.
