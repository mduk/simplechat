-module( simplechat_room ).

-export( [ start_link/0, join/1, part/1 ] ).

start_link() ->
	gen_event:start_link().

join( Room ) ->
	gen_event:add_handler( Room, simplechat_room_handler, self() ).

part( Room ) ->
	gen_event:delete_handler( Room, simplechat_room_handler, self() ).
