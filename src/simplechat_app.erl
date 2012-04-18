-module( simplechat_app ).

-behaviour( application ).
-export( [ start/2, stop/1 ] ).

start( _StartType, _StartArgs ) ->
	DispatchRules = [
		{ '_', [
			{ [ <<"websocket">> ], wshandler, [] },
			{ '_', handler, [] }
		] }
	],

	{ ok, Pid } = cowboy:start_listener( http, 10, 
		cowboy_tcp_transport, [ { port, 8080 } ],
		cowboy_http_protocol, [ { dispatch, DispatchRules } ]
	),
	
	ok.

stop( _State ) ->
	ok.
