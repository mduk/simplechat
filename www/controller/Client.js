Ext.define( 'SimpleChat.controller.Client', {
	extend: 'Ext.app.Controller',
	
	/**
	 * Initialise the client object
	 */
	init: function()
	{
		this.wsUrl = 'ws://' + window.location.host + '/';
		
		this.control( {
			
			// Connect / Disconnect button
			'#room-list-win #connectionButton' : {
				// Click
				click: function( btn, event )
				{
					// Connect state
					if ( btn.getText() == "Connect" )
					{
						// Scope hack
						var controller = this;
						
						// Get a nickname from the user
						Ext.Msg.prompt( 
							"Nickname", 
							"Please specify a nickname:", 
							function( clicked, nick )
							{
								controller.connect( nick );
							} 
						);
						
						return;
					}
					
					this.disconnect();
				}
			},
			
			'#room-list-win #createRoomButton' : {
				click: function( btn, e )
				{
					var controller = this;
					
					Ext.Msg.prompt( 'Room Name', 'Specify room name:', function( clicked, name )
					{
						if ( clicked != 'ok' )
						{
							return;
						}
						
						controller.join( name );
						controller.activeRooms();
					} );
				}
			},
			
			'#room-list-win #refreshListButton' : {
				click: function( btn, e )
				{
					this.activeRooms();
				}
			},
			
			// Room list item
			'#room-list-win #room-list' : {
				// Double Click
				itemdblclick: function( grid, record )
				{
					this.join( record.data.name );
				}
			}
		} );
		
		// When the client has connected, switch the
		// state of the toolbar buttons
		this.addListener( 'connected', function()
		{
			var btn = Ext.ComponentQuery.query( '#room-list-win #connectionButton' )[0];
		
			btn.setText( "Disconnect" );
			btn.setIconCls( 'icon-disconnect' );
			
			var createRoomBtn = btn.nextSibling();
			createRoomBtn.setDisabled( false );
			
			var refreshListBtn = createRoomBtn.nextSibling();
			refreshListBtn.setDisabled( false );
		}, this );
		
		// When the client has disconnected, switch the
		// state of the toolbar buttons
		this.addListener( 'disconnected', function()
		{
			var btn = Ext.ComponentQuery.query( '#room-list-win #connectionButton' )[0];
		
			btn.setText( "Connect" );
			btn.setIconCls( 'icon-connect' );
			
			var createRoomBtn = btn.nextSibling();
			createRoomBtn.setDisabled( true )
			
			var refreshListBtn = createRoomBtn.nextSibling();
			refreshListBtn.setDisabled( true );
		}, this );
	},
	
	/**
	 * Get the room list window
	 *
	 * If the room list window hasn't already 
	 * been created and registered, then do so.
	 */
	roomListWindow: function()
	{
		this.roomList = Ext.WindowManager.get( 'room-list-win' );
		
		if ( this.roomList == undefined )
		{
			this.roomList = Ext.create( 'SimpleChat.view.room.ListWindow', {
				id: 'room-list-win'
			} );
			
			Ext.WindowManager.register( this.roomList );
		}
		
		return this.roomList;
	},
	
	/**
	 * Connect to the server.
	 *
	 * Open the websocket, fire the connected
	 * event and send the ident packet.
	 */
	connect: function( nick )
	{
		// Create the websocket
		this.ws = ws = new WebSocket( this.wsUrl );
		
		// This is for scoping into the websocket handlers
		var controller = this;
		
		// Websocket opened
		this.ws.onopen = function()
		{
			// Fire connected event
			controller.fireEvent( 'connected' );
			
			// Identify with the server
			controller.sendPacket( {
				type: 'ident',
				name: nick
			} );
		};
		
		// Received websocket message
		this.ws.onmessage = function( wsMsg )
		{
			console.log( "Recv: " + wsMsg.data );
			// Parse the json payload and pass it to the controller
			var msg = JSON.parse( wsMsg.data );
			controller.handle( msg );
		};
		
		// Websocket error occured
		this.ws.onerror = function( e )
		{
			// Log the error to the console
			console.log( "Websocket error!" );
			console.log( e );
		};
		
		// Websocket closed
		this.ws.onclose = function()
		{
			// Fire a disconnected event
			controller.fireEvent( 'disconnected' );
		};
	},
	
	/**
	 * Disconnect from the server
	 */
	disconnect: function()
	{
		this.ws.close();
	},
	
	/**
	 * Join a room
	 *
	 * @param string room The room name
	 */
	join: function( room )
	{
		this.sendPacket( {
			type: 'join',
			room: room
		} );
	},
	
	/**
	 * Part a room
	 *
	 * @param string room The room name
	 */
	part: function( room )
	{
		this.sendPacket( {
			type: 'part',
			room: room
		} );
	},
	
	/**
	 * Say something to a room
	 */
	say: function( room, message )
	{
		this.sendPacket( {
			type: 'say',
			room: room,
			body: message
		} );
	},
	
	activeRooms: function()
	{
		this.sendPacket( {
			type: 'active_rooms'
		} );
	},
	
	/**
	 * Sends a packet to the server
	 *
	 * Takes an object (with a 'type' property),
	 * encodes it to json and sends it down the
	 * websocket to the server.
	 *
	 * @param object packet The object to encode and send
	 */
	sendPacket: function( packet )
	{
		var packet = JSON.stringify( packet );
		console.log( "Send: " + packet );
		this.ws.send( packet );
	},
	
	/**
	 * Handle a decoded packet from the server
	 */
	handle: function( msg )
	{
		if ( "source" in msg )
		{
			return this.handleEvent( msg );
		}
		
		switch ( msg.type )
		{
			// = Welcome =
			// Request the room list
			case 'welcome':
				this.activeRooms();
				break;
			
			// = Active Room list =
			// Update the room list window
			case 'active_rooms':
				Ext.getCmp( 'room-list' ).updateList( msg.rooms );
				break;
			
			// = Message = <<< DEPRECATED
			// Get the room window and add the event
			case 'message':
				console.log( "Received a message msg" );
				console.log( msg );
				var roomWindow = Ext.WindowManager
					.get( 'room-' + msg.room + '-win' )
					.appendRoomEvent( msg );
				break;
			
			// = Error =
			// Display an alert box
			case 'error':
				Ext.Msg.alert( "Alert!", msg.message );
				break;
			
			// Catch all
			default:
				console.log( "Unknown packet type!" );
				console.log( msg );
				break;
		}
	},
	
	handleEvent: function( event )
	{
		switch ( event.source )
		{
			case 'client':
				this.handleClientEvent( event );
				break;
				
			case 'room':
				this.getController( 'Room' )
					.handleEvent( event );
				break;
			
			default:
				console.log( 'Unknown event type!' );
				console.log( event );
				break;
		}
	},
	
	handleClientEvent: function( event )
	{
		switch ( event.type )
		{
			case "joined":
				this.getController( 'Room' )
					.roomWindow( event.room )
					.show( this.roomListWindow() );
				break;
			
			case "parted":
				this.getController( 'Room' )
					.roomWindow( { name: event.room } )
					.hide( this.roomListWindow() );
				break;
				
			case "room_info":
				Ext.getCmp( 'room-list' ).updateRoom( event.room );
				break;
			
			default:
				console.log( 'Unknown client event!' );
				console.log( event );
				break;
		}
	}
} );
