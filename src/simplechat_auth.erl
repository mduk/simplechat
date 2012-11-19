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
			{ ok, Pid } = simplechat_client_sup:start_client( Id ),
			simplechat_client:nick( Pid, Nick ),
			ets:insert( ?TABLE, { Id, Pid } ),
			{ ok, Pid };
		[ { Id, Pid } ] ->
			{ ok, Pid }
	end.