% A HTTP notification plugin
%
% Simply a process that receives room events, encodes
% them to json, and sends it as an HTTP POST request
% to the url specified.

-module( simplechat_httpnotify ).

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [ start_link/1 ] ).
-record( state, { url } ).

start_link( Url ) ->
	gen_server:start_link( ?MODULE, Url, [] ).

init( Url ) ->
	{ ok, #state{
		url = Url
	} }.

handle_call( _, _, S ) ->
	{ reply, { error, unknown_call }, S }.

handle_cast( _, S ) ->
	{ noreply, S }.

handle_info( Event, State ) ->
	#state{ url = Url } = State,
	Body = list_to_binary( simplechat_protocol:encode( Event ) ),
	io:format( "Pinging ~p~n",  [ Body ] ),
	httpc:request( post, { Url, [], "application/json", Body }, [], [] ),
	{ noreply, State }.

terminate( _, _ ) ->
	ok.

code_change( _, _, _ ) ->
	ok.