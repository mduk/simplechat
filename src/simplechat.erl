-module( simplechat ).

-export( [ start/0 ] ).

start() ->
	application:start( sasl ),
	application:start( cowboy ),
	application:start( simplechat ).