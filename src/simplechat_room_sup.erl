-module( simplechat_room_sup ).

-behaviour( supervisor ).
-export( [ init/1 ] ).

-export( [ start_link/0, start_room/1, room/1, rooms/0 ] ).

start_link() ->
	supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

init( _ ) ->
	{ ok, { { one_for_one, 5, 10 }, [] } }.

% Start a room with the given name
start_room( Name ) ->
	io:format( "Creating room: ~s~n", [ Name ] ),
	Mfa = { simplechat_room, start_link, [] },
	ChildSpec = { Name, Mfa, permanent, 5000, worker, [ simplechat_room ] },
	supervisor:start_child( ?MODULE, ChildSpec ).

% Return a room pid by it's name, start the room if it isn't already
room( Name ) ->
	case supervisor:which_children( ?MODULE ) of
		{ Name, Pid, _, _ } -> { ok, Pid };
		_                   -> start_room( Name )
	end.

% Get a list of active rooms
rooms() ->
	lists:map( fun( { Name, Pid, _, _ } ) ->
		{ Name, Pid }
	end, supervisor:which_children( ?MODULE ) ).
