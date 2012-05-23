Ext.define( 'SimpleChat.controller.Room', {
	extend: 'Ext.app.Controller',
	
	init: function()
	{
		//
	},
	
	/**
	 * Handle room events
	 *
	 * @param object event The room event object
	 */
	handleEvent: function( event )
	{
		switch ( event.type )
		{
			case "joined":
			case "parted":
			case "message":
				var windowId = this.roomWindowId( { 
					name: event.room
				} );
				Ext.getCmp( windowId )
					.appendRoomEvent( event );
				break;
			
			default:
				console.log( "Unknown room event!" );
				console.log( event );
				break;
		}
	},
	
	/**
	 * Get the room window id for a given room model.
	 *
	 * @param object room The room model
	 */
	roomWindowId: function( room )
	{
		return 'room-' + room.name + '-win';
	},
	
	/**
	 * Get a room window.
	 * 
	 * If the room window doesn't exist, then
	 * create it and register it with the
	 * window manager before returning it.
	 *
	 * @param object room The room model
	 */
	roomWindow: function( room )
	{
		var 
			roomWindowId = this.roomWindowId( room ),
			roomWindow = Ext.WindowManager.get( roomWindowId );
		
		if ( roomWindow == undefined )
		{
			roomWindow = Ext.create( 'SimpleChat.view.room.Chat', {
				id: roomWindowId,
				room: room
			} );
			
			Ext.WindowManager.register( roomWindow );
		}
		
		return roomWindow;
	},
} );
