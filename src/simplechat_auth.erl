-module( simplechat_auth ).

-export( [
	init/0,
	ident/2
] ).

-define( TABLE, simplechat_auth ).

init() ->
	ets:new( ?TABLE, [ public, named_table ] ).

ident( Nick, Password ) ->
	Id = { Nick, Password },
	case ets:lookup( ?TABLE, Id ) of
		[] ->
			start_client( Id, Nick );
		[ { Id, Pid } ] ->
			case is_alive( Pid ) of
				false ->
					ets:delete( ?TABLE, Id ),
					start_client( Id, Nick );
				true -> 
					{ ok, Pid }
			end
	end.

start_client( Id, Nick ) ->
	{ ok, Pid } = simplechat_client_sup:start_client( Id ),
	simplechat_client:nick( Pid, Nick ),
	ets:insert( ?TABLE, { Id, Pid } ),
	{ ok, Pid }.
	

is_alive( Pid ) when node( Pid ) =:= node() ->
	is_process_alive( Pid ).