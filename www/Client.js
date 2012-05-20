/**
 * Chat client class
 */
var Client = function( wsUrl, messageCallback )
{
	/**
	 * The Websocket handle
	 */
	var connection;
	
	/**
	 * Send a command (object with type property) down the connection
	 */
	function cmd( Data )
	{
		var json = JSON.stringify( Data );
		console.log( "Sending packet: " + json );
		connection.send( json );
	}
	
	this.cmd = cmd;

	/**
	 * Connect to the simplechat websocket handler
	 */
	this.connect = function( nick, callback )
	{
		console.log( "Connecting... " );
	
		connection = new WebSocket( wsUrl );
		
		connection.onopen = function()
		{
			console.log( "Connected." );
			
			cmd( {
				type: "ident",
				name: nick
			} );
	
			callback();
		};
		
		connection.onerror = function( error )
		{
			console.log( 'WebSocket Error!' );
			console.log( error );
		};
		
		connection.onmessage = messageCallback;
		
		connection.onclose = function( event )
		{
			console.log( event );
		};
	};

	/**
	 * Disconnect from server
	 */
	this.disconnect = function()
	{
		cmd( {
			type: 'quit'
		} );
		connection.close();
	};
	
	/**
	 * Join a chat room.
	 */
	this.join = function( Room )
	{
		cmd( {
			type: 'join',
			room: Room
		} );
	};
	
	/**
	 * Part a chat room.
	 */
	this.part = function( Room )
	{
		cmd( {
			type: 'part',
			room: Room
		} );
	};
	
	/**
	 * Say something to a chat room
	 */
	this.say = function( Room, Message )
	{
		cmd( {
			type: 'say',
			room: Room,
			body: Message
		} );
	};
	
	/**
	 * Request a list of active rooms
	 */
	this.activeRooms = function()
	{
		cmd( {
			type: 'active_rooms'
		} );
	};
	
	/**
	 * Disconnect
	 */
	this.disconnect = function()
	{
		connection.close();
	};
};
