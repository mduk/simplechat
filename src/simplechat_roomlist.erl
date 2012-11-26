-module( simplechat_roomlist ).

-include("simplechat.hrl").

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [
	init/0,
	start_link/0,
	subscribe/0,
	unsubscribe/0,
	get_list/0
] ).

-record( state, { event_manager, room_list = [] }  ).

%===============================================================================
% init/0
%===============================================================================
init() ->
	{ ok, Pid } = simplechat_sup:start_worker( ?MODULE, start_link, [] ),
	register( ?MODULE, Pid ).
	
%===============================================================================
% start_link/0
%===============================================================================
start_link() ->
	gen_server:start_link( ?MODULE, [], [] ).
	
%===============================================================================
% subscribe/0
%===============================================================================
subscribe() ->
	gen_server:call( ?MODULE, { subscribe, self() } ).

%===============================================================================
% unsubscribe/0
%===============================================================================
unsubscribe() ->
	gen_server:call( ?MODULE, { unsubscribe, self() } ).

%===============================================================================
% get_list/0
%===============================================================================
get_list() ->
	gen_server:call( ?MODULE, get_list ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%===============================================================================
% init/1
%
% Subscribe to server-level events (room_opened/room_closed)
% Start room list event manager
%===============================================================================
init( _ ) -> 
	simplechat_server_event:subscribe(),
	{ ok, Pid } = gen_event:start_link(),
	{ ok, #state{ event_manager = Pid } }.

%===============================================================================
% handle_call/3
%===============================================================================
% Get the room list
%-------------------------------------------------------------------------------
handle_call( get_list, _, S ) ->
	List = lists:map( fun( { _, I } ) -> I end, S#state.room_list ),
	{ reply, List, S };
%-------------------------------------------------------------------------------
% Subcribe to the room list event manger
%-------------------------------------------------------------------------------
handle_call( { subscribe, Pid }, _, S ) ->
	R = gen_event:add_handler( S#state.event_manager, simplechat_subscribe, Pid ),
	{ reply, R, S };
%-------------------------------------------------------------------------------
% Unsubcribe from the room list event manger
%-------------------------------------------------------------------------------
handle_call( { unsubscribe, Pid }, _, S ) ->
	R = gen_event:delete_handler( S#state.event_manager, simplechat_subscribe, Pid ),
	{ reply, R, S };
%-------------------------------------------------------------------------------
% Catch All
%-------------------------------------------------------------------------------
handle_call( _, _, S ) -> 
	{ reply, { error, unknown_call }, S }.

%===============================================================================
% handle_call/3
%===============================================================================
% A room opened
%-------------------------------------------------------------------------------
handle_info( { server_event, { room_opened, Room } }, S ) -> 
	{ ok, Pid } = simplechat_room_sup:room( Room ),
	simplechat_room:watch( Pid ),
	simplechat_room:info( Pid ),
	Element = { Pid, #room_info{ pid = Pid } },
	NewS = S#state{ room_list = [ Element | S#state.room_list ] },
	{ noreply, NewS };
%-------------------------------------------------------------------------------
% Someone joined a room
%
% Add one to the member count
%-------------------------------------------------------------------------------
handle_info( { room_event, { _Room, Pid }, { joined, _Room, _Nick } }, S ) ->
	OldInfo = proplists:get_value( Pid, S#state.room_list ),
	List = proplists:delete( Pid, S#state.room_list ),
	NewInfo = OldInfo#room_info{
		members = OldInfo#room_info.members + 1
	},
	gen_event:notify( S#state.event_manager, NewInfo ),
	NewList = [ { Pid, NewInfo } | List ],
	NewS = S#state{ room_list = NewList },
	{ noreply, NewS };
%-------------------------------------------------------------------------------
% Someone parted a room
% 
% Subtract one from the member count
%-------------------------------------------------------------------------------
handle_info( { room_event, { _Room, Pid }, { parted, _Room, _Nick } }, S ) ->
	OldInfo = proplists:get_value( Pid, S#state.room_list ),
	List = proplists:delete( Pid, S#state.room_list ),
	NewInfo = OldInfo#room_info{
		members = OldInfo#room_info.members - 1
	},
	gen_event:notify( S#state.event_manager, NewInfo ),
	NewList = [ { Pid, NewInfo } | List ],
	NewS = S#state{ room_list = NewList },
	{ noreply, NewS };
%-------------------------------------------------------------------------------
% Received room info proplist
%
% Update info with information supplied by room
%-------------------------------------------------------------------------------
handle_info( { room, { _, Pid }, info, Proplist }, S ) ->
	OldInfo = proplists:get_value( Pid, S#state.room_list ),
	List = proplists:delete( Pid, S#state.room_list ),
	NewInfo = OldInfo#room_info{
		name = proplists:get_value( name, Proplist ),
		topic = proplists:get_value( topic, Proplist ),
		members = proplists:get_value( members, Proplist )
	},
	gen_event:notify( S#state.event_manager, NewInfo ),
	NewList = [ { Pid, NewInfo } | List ],
	NewS = S#state{ room_list = NewList },
	{ noreply, NewS };
%-------------------------------------------------------------------------------
% Catch All
%-------------------------------------------------------------------------------
handle_info( Msg, S ) -> 
	error_logger:warning_msg( 
		"** Room List received an unexpected message~n"
		"** Message: ~p~n", 
		[ Msg ] 
	),
	{ noreply, S }.



handle_cast( _, S ) -> { noreply, S }.

terminate( _, _ ) -> ok.

code_change( _, _, S ) -> { ok, S }.

	