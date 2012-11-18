-module( simplechat_app ).

-behaviour( application ).
-export( [ start/2, stop/1 ] ).

start( _StartType, _StartArgs ) ->
	start_http(),
	simplechat_sup:start_link().

stop( _State ) ->
	ok.

start_http() ->
	HttpDispatchRules = [
		{ '_', [ % Any Host
			{ '_', simplechat_wshandler, [] } % / -> simplechat_wshandler
		] }
	],

	cowboy:start_listener( http, 10, 
		cowboy_tcp_transport, [ { port, env( port, 8000 ) } ],
		cowboy_http_protocol, [ { dispatch, HttpDispatchRules } ] 
	).

env( Param, Default ) ->
	case application:get_env( Param ) of
		undefined -> Default;
		{ ok, Value } -> Value
	end.