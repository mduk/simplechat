-module( scirc_server ).

-export( [ start_link/0 ] ).

start_link() -> 
	{ ok, _ } = simplechat_sup:start_supervisor( scirc_client_sup, start_link, [] ),
	{ ok, Sock } = gen_tcp:listen( 6667, [
		binary,
		{ packet, line },
		{ nodelay, true },
		{ keepalive, true },
		{ active, true },
		{ reuseaddr, true }
	] ),
	spawn_link( fun() -> loop( Sock ) end ).

loop( Sock ) ->
	{ ok, ClientSock } = gen_tcp:accept( Sock ),
	{ ok, Pid } = scirc_client_sup:start_client( ClientSock ),
	ok = gen_tcp:controlling_process( ClientSock, Pid ),
	?MODULE:loop( Sock ).