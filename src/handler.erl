-module( handler ).

-behaviour( cowboy_http_handler ).
-export( [ init/3, handle/2, terminate/2 ] ).

init( { _, http }, R, [] ) ->
	{ ok, R, undefined }.

handle( R, S ) ->
	{ ok, R2 } = cowboy_http_req:reply( 200, [], <<"Hello World!">>, R ),
	{ ok, R2, S }.

terminate( _R, _S ) ->
	ok.
