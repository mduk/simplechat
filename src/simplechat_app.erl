-module( simplechat_app ).

-behaviour( application ).
-export( [ start/2, stop/1 ] ).

start( _StartType, _StartArgs ) ->
	start_http(),
	{ ok, Sup } = simplechat_sup:start_link(),
	{ ok, RoomPid } = simplechat_room_sup:start_room( <<"default_room">> ),
	register( default_room, RoomPid ),
	{ ok, Sup }.

stop( _State ) ->
	ok.

start_http() ->
	HttpDispatchRules = [
		{ '_', [ % Any Host
			{ '_', simplechat_wshandler, [] } % / -> simplechat_wshandler
		] }
	],

	cowboy:start_listener( http, 10, 
		cowboy_tcp_transport, [ { port, 8080 } ],
		cowboy_http_protocol, [ { dispatch, HttpDispatchRules } ] 
	).
