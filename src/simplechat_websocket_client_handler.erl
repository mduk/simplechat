-module( simplechat_websocket_client_handler ).

-behaviour( gen_event ).
-export( [ init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3 ] ).

-record( state, { wshandler } ).

% Initialise the websocket client handler. Record the wshandler pid in the state.
init( Pid ) ->
    { ok, #state{
    	wshandler = Pid
    } }.

% Relay room event as-is
handle_event( Event = { room_event, _, _ }, S = #state{ wshandler = Pid } ) ->
	Pid ! Event,
	{ ok, S };
% Relay server event as-is
handle_event( Event = { server_event, _ }, S = #state{ wshandler = Pid } ) ->
	Pid ! Event,
	{ ok, S };
% Wrap client events and send to pid
handle_event( Event, S = #state{ wshandler = Pid } ) ->
	Pid ! { client_event, Event },
	{ ok, S }.

handle_call( _Msg, State ) ->
	{ ok, { error, bad_query }, State }.

handle_info( _Msg, State ) ->
	{ ok, State }.

terminate( _Reason, _State ) ->
	ok.

code_change( _OldVsn, _State, _Opts ) ->
	ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

