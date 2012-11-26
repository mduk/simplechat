-module( simplechat_protocol ).

-include("simplechat.hrl").

-export( [ encode/1, decode/1 ] ).

%===============================================================================
% decode/1
%===============================================================================
% Parse an 'ident' message
%-------------------------------------------------------------------------------
decode( { ident, Props } ) ->
	{ _, Name } = proplists:lookup( <<"name">>, Props ),
	{ _, Password } = proplists:lookup( <<"password">>, Props ),
	{ ident, Name, Password };
%-------------------------------------------------------------------------------
% Subscribe to a stream
%-------------------------------------------------------------------------------
decode( { subscribe, Props } ) ->
	Stream = proplists:get_value( <<"stream">>, Props ),
	{ subscribe, Stream };
%-------------------------------------------------------------------------------
% Unsubscribe from a stream
%-------------------------------------------------------------------------------
decode( { unsubscribe, Props } ) ->
	Stream = proplists:get_value( <<"stream">>, Props ),
	{ unsubscribe, Stream };
%-------------------------------------------------------------------------------
% Parse a 'joined_rooms' message
%-------------------------------------------------------------------------------
decode( { joined_rooms, _ } ) ->
	joined_rooms;
%-------------------------------------------------------------------------------
% Parse a 'room_list' message
%-------------------------------------------------------------------------------
decode( { room_list, _ } ) ->
	room_list;
%-------------------------------------------------------------------------------
% Parse a 'member_list' message
%-------------------------------------------------------------------------------
decode( { member_list, Props } ) ->
	{ _, Room } = proplists:lookup( <<"room">>, Props ),
	{ member_list, Room };
%-------------------------------------------------------------------------------
% Parse a 'quit' message
%-------------------------------------------------------------------------------
decode( { quit, _ } ) ->
	quit;
%-------------------------------------------------------------------------------
% Parse a 'join' message
%-------------------------------------------------------------------------------
decode( { join, Props } ) ->
	{ _, Room } = proplists:lookup( <<"room">>, Props ),
	{ join, Room };
%-------------------------------------------------------------------------------
% Parse a 'part' mssage
%-------------------------------------------------------------------------------
decode( { part, Props } ) ->
	{ _, Room } = proplists:lookup( <<"room">>, Props ),
	{ part, Room };
%-------------------------------------------------------------------------------
% Parse a 'set_topic' message
%-------------------------------------------------------------------------------
decode( { set_topic, Props } ) ->
	{ _, Room } = proplists:lookup( <<"room">>, Props ),
	{ _, Topic } = proplists:lookup( <<"topic">>, Props ),
	{ set_topic, Room, Topic };
%-------------------------------------------------------------------------------
% Parse a 'lock_topic' message
%-------------------------------------------------------------------------------
decode( { lock_topic, Props } ) ->
	{ _, Room } = proplists:lookup( <<"room">>, Props ),
	{ lock_topic, Room };
%-------------------------------------------------------------------------------
% Parse a 'unlock_topic' message
%-------------------------------------------------------------------------------
decode( { unlock_topic, Props } ) ->
	{ _, Room } = proplists:lookup( <<"room">>, Props ),
	{ unlock_topic, Room };
%-------------------------------------------------------------------------------
% Parse a 'say' message
%-------------------------------------------------------------------------------
decode( { say, Props } ) ->
	{ _, Room } = proplists:lookup( <<"room">>, Props ),
	{ _, Message } = proplists:lookup( <<"message">>, Props ),
	{ say, Room, Message };
%-------------------------------------------------------------------------------
% Parse a 'shout' message
%-------------------------------------------------------------------------------
decode( { shout, Props } ) ->
	{ _, Message } = proplists:lookup( <<"message">>, Props ),
	{ shout, Message };
%-------------------------------------------------------------------------------
% Parse a message from it's decoded json representation
%-------------------------------------------------------------------------------
decode( { struct, Props } ) ->
	Type = case proplists:lookup( <<"type">>, Props ) of
		none -> throw( json_lacks_type );
		{ _, TypeBin } -> binary_to_atom( TypeBin, utf8 )
	end,
	decode( { Type, Props } );
%-------------------------------------------------------------------------------
% Parse json payload
%-------------------------------------------------------------------------------
decode( JsonBin ) ->
        decode( mochijson2:decode( JsonBin ) ).

% ==============================================================================
% encode/1
% ==============================================================================
% room_info
% ------------------------------------------------------------------------------
encode( #room_info{ name=Name, topic=Topic, members=Members } ) ->
	mochijson2:encode( { struct, [
		{ type, room_info },
		{ name, Name },
		{ topic, Topic },
		{ members, Members }
	] } );
% ------------------------------------------------------------------------------
% joined_rooms
% ------------------------------------------------------------------------------
encode( { joined_rooms, Rooms } ) ->
	RoomNames = lists:map( fun( { _, N } ) -> N end, Rooms ),
	mochijson2:encode( { struct, [
		{ type, joined_rooms },
		{ rooms, RoomNames }
	] } );
% ------------------------------------------------------------------------------
% Server Event: room_opened
% ------------------------------------------------------------------------------
encode( { server_event, { room_opened, RoomName } } ) ->
	mochijson2:encode( { struct, [
		{ source, server },
		{ type, room_opened },
		{ room, { struct, [
			{ name, RoomName }
		] } }
	] } );
% ------------------------------------------------------------------------------
% Room Event: message
% ------------------------------------------------------------------------------
encode( { room_event, { Room, _ }, { message, { Nick, _ }, Message } } ) ->
	mochijson2:encode( { struct, [
		{ source, room },
		{ type, message },
		{ room, Room },
		{ client, Nick },
		{ body, Message }
	] } );
% ------------------------------------------------------------------------------
% Room Event: topic_changed/topic_locked/topic_unlocked event
% ------------------------------------------------------------------------------
encode( { room_event, { Room, _ }, { Event, Topic } } ) 
when Event =:= topic_changed; Event =:= topic_locked; Event =:= topic_unlocked ->
	mochijson2:encode( { struct, [
		{ source, room },
		{ room, Room },
		{ type, Event },
		{ topic, Topic }
	] } );
% ------------------------------------------------------------------------------
% Room Event: joined/parted
% ------------------------------------------------------------------------------
encode( { room_event, { Room, _ }, { Motion, _, Client } } ) when Motion =:= joined; Motion =:= parted ->
	mochijson2:encode( { struct, [
		{ source, room },
		{ type, atom_to_binary( Motion, utf8 ) },
		{ room, Room },
		{ client, Client }
	] } );
% ------------------------------------------------------------------------------
% Room Event: plugin_started
% ------------------------------------------------------------------------------
encode( { room_event, _, { plugin_started, _ } } ) ->
	[];
% ------------------------------------------------------------------------------
% Relayed room event --- Deprecated?
% ------------------------------------------------------------------------------
encode( { client_event, Event = { room_event, _, _ } } ) ->
	io:format( "Room Event wrapped in a client event: ~p~n", [ Event ]  ),
	encode( Event );
% ------------------------------------------------------------------------------
% Client Event: joined
% ------------------------------------------------------------------------------
encode( { client_event, { joined, RoomInfo } } ) ->
	mochijson2:encode( { struct, [
		{ source, client },
		{ type, joined },
		{ room, encode( { room_info, RoomInfo } ) }
	] } );
% ------------------------------------------------------------------------------
% Client Event: Parted
% ------------------------------------------------------------------------------
encode( { client_event, { parted, { RoomName, _ } } } ) ->
	mochijson2:encode( { struct, [
		{ source, client },
		{ type, parted },
		{ room, RoomName }
	] } );
% ------------------------------------------------------------------------------
% Client Event: room_info
% ------------------------------------------------------------------------------
encode( { client_event, { room_info, RoomInfo } } ) ->
	mochijson2:encode( { struct, [
		{ source, client },
		{ type, room_info },
		{ room, encode( { room_info, RoomInfo } ) }
	] } );
% ------------------------------------------------------------------------------
% Client Event: room error
% ------------------------------------------------------------------------------
encode( { client_event, { room, { RoomName, _ }, { error, Reason } } } ) ->
	encode( { error, [ "Room Error: ", RoomName, ": ", io_lib:format( "~p", [ Reason ] ) ] } );
% ------------------------------------------------------------------------------
% Client Event: room denied
% ------------------------------------------------------------------------------
encode( { client_event, { denied, { RoomName, _ } } } ) ->
	encode( { error, [ "Access to room \"", RoomName, "\" denied." ] } );
% ------------------------------------------------------------------------------
% Encode a 'welcome' message
% ------------------------------------------------------------------------------
encode( welcome ) ->
	mochijson2:encode( { struct, [
		{ type, welcome }
	] } );
% ------------------------------------------------------------------------------
% Encode room info
% ------------------------------------------------------------------------------
encode( { room_info, RoomInfo } ) ->
	{ struct, [
		{ name, proplists:get_value( name, RoomInfo ) },
		{ topic, proplists:get_value( topic, RoomInfo ) },
		{ members, proplists:get_value( members, RoomInfo ) }
	] };
% ------------------------------------------------------------------------------
% Encode an 'active_rooms' message
% ------------------------------------------------------------------------------
encode( { active_rooms, Rooms } ) ->
	RoomStructs = lists:map( fun( RoomInfo ) ->
		encode( { room_info, RoomInfo } )
	end, Rooms ),
	mochijson2:encode( { struct, [
		{ type, active_rooms },
		{ rooms, RoomStructs }
	] } );
% ------------------------------------------------------------------------------
% Encode an 'error' message
% ------------------------------------------------------------------------------
encode( { error, Message } ) ->
	mochijson2:encode( { struct, [
		{ type, error },
		{ title, <<"Server Error">> },
		{ message, list_to_binary( Message ) }
	] } ). 
