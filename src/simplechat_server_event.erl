-module( simplechat_server_event ).

-export( [ 
	init/0,
	subscribe/0,
	unsubscribe/0,
	notify/1
] ).

init() ->
	{ ok, Pid } = simplechat_sup:start_worker( gen_event, start_link, [] ),
	register( ?MODULE, Pid ),
	{ ok, Pid }.

subscribe() ->
	gen_event:add_handler( ?MODULE, simplechat_subscribe, { self(), server_event } ).

unsubscribe() ->
	gen_event:delete_handler( ?MODULE, simplechat_subscribe, { self(), server_event } ).

notify( Event ) ->
	gen_event:notify( ?MODULE, Event ).