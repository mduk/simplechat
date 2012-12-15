-module( scirc_client ).

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [ start_link/1 ] ).

-record( state, { socket, nick, password, client_pid } ).

start_link( Sock ) ->
	gen_server:start_link( ?MODULE, Sock, [] ).

init( Sock ) -> 
	{ ok, #state{ socket = Sock } }.

handle_call( _, _, S ) -> { reply, { error, unknown_call }, S }.
handle_cast( _, S ) -> { noreply, S }.

%% --------------------------------------------------------------------------------
%% Client Events
%% --------------------------------------------------------------------------------
%% --------------------------------------------------------------------------------
%% Room Events
%% --------------------------------------------------------------------------------
handle_info( { room_event, _, { message, { Nick, _ }, _ } }, S = #state{ nick = Nick } ) -> 
	{ noreply, S };
handle_info( { room_event, { Channel, _ }, { message, { Nick, _ }, Body } }, S ) ->
	#state{ socket = Sock } = S,
	gen_tcp:send( Sock, [ ":", Nick, "!", Nick, "@localhost PRIVMSG ", convert_name( Channel ), " :", Body, "\r\n" ] ),
	{ noreply, S };
handle_info( { room_event, { Channel, _ }, { joined, Channel, Nick } }, S ) ->
	#state{ socket = Sock } = S,
	gen_tcp:send( Sock, [ ":", Nick, "!", Nick, "@localhost JOIN :", convert_name( Channel ), "\r\n" ] ),
	{ noreply, S };
%% --------------------------------------------------------------------------------
%% IRC
%% --------------------------------------------------------------------------------
handle_info( { irc, { { _, "PASS", [ Password ] }, _ } }, S ) -> 
	{ noreply, S#state{ password = Password } };
handle_info( { irc, { { _, "NICK", [ Nick ] }, _ } }, S ) -> 
	{ noreply, S#state{ nick = Nick } };
handle_info( { irc, { { _, "USER", _ }, _ } }, S ) -> 
	case S of
		#state{ password = undefined, nick = Nick, socket = Sock } ->
			error_logger:warning_msg( 
				"** User tried to auth without a password!~n"
				"** Nick: ~p~n", 
				[ Nick ] 
			),
			gen_tcp:close( Sock ),
			{ stop, no_password, S };
		
		#state{ nick = Nick, password = Password, socket = Sock } ->
			case auth( Nick, Password ) of
				{ ok, ClientPid } ->
					gen_tcp:send( Sock, [ ":localhost 001 ", Nick, " :Welcome (SimpleChat IRC Interface)\r\n" ] ),
					{ noreply, S#state{ client_pid = ClientPid } };
				{ error, _ } ->
					gen_tcp:close( Sock ),
					{ noreply, S }
			end
	end;
handle_info( { irc, { { _, "JOIN", [ Channel ] } , _ } }, S ) ->
	#state{ client_pid = Client, nick = Nick, socket = Sock } = S,
	simplechat_client:join( Client, convert_name( Channel ) ),
	{ noreply, S };
handle_info( { irc, { { _, "PART", [ Channel ] } , _ } }, S ) ->
	#state{ client_pid = Client, socket = Sock } = S,
	simplechat_client:part( Client, convert_name( Channel ) ),
	{ noreply, S };
handle_info( { irc, { { _,"PRIVMSG", [ Channel ] }, Message } }, S ) ->
	#state{ client_pid = Client } = S,
	simplechat_client:say( Client, convert_name( Channel ), list_to_binary( Message ) ),
	{ noreply, S };
%% --------------------------------------------------------------------------------
%% TCP
%% --------------------------------------------------------------------------------
handle_info( { tcp, _, Msg }, S ) ->
	io:format( "Got: ~p~n", [ Msg ] ),
	Self = self(),
	spawn( fun() ->
		Self ! { irc, scirc_protocol:decode( Msg ) }
	end ),
	{ noreply, S };
handle_info( { tcp_closed, _ }, S ) ->
	{ stop, disconnected, S };
%% --------------------------------------------------------------------------------
%% Catch All
%% --------------------------------------------------------------------------------
handle_info( Msg, S ) -> 
	error_logger:warning_msg( 
		"** Received an unexpected message!~n"
		"** Message: ~p~n", 
		[ Msg ] 
	),
	{ noreply, S }.

terminate( _, _ ) -> ok.

code_change( _, _, S ) -> { ok, S }.

convert_name( Name ) when is_binary( Name ) ->
	[ "#" | binary_to_list( Name ) ];
convert_name( [ $# | Name ] ) ->
	list_to_binary( Name ).

auth( Nick, Password ) ->
	io:format( "> Authing user with [~s] [~s]~n", [ Nick, Password ] ),
	case simplechat_auth:ident( list_to_binary( Nick ), list_to_binary( Password ) ) of
		{ ok, Pid } ->
			simplechat_client:add_handler( Pid, simplechat_websocket_client_handler, self() ),
			{ ok, Pid };
		
		{ denied, Reason } ->
			error_logger:warning_msg( 
				"** Client auth denied!~n"
				"** Nick: ~p~n" 
				"** Password: ~p~n"
				"** Reason: ~p~n", 
				[ Nick, Password, Reason ] 
			),
			{ error, { denied, Reason } }
	end.
