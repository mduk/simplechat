Ext.define( 'SimpleChat.controller.Room', {
	extend: 'Ext.app.Controller',
	
	init: function()
	{
		//
	},
	
	handleEvent: function( event )
	{
		switch ( event.type )
		{
			case "joined":
				Ext.getCmp( this.roomWindowId( event ) ).appendRoomEvent( event );
				break;
			
			default:
				console.log( "Unknown room event!" );
				console.log( event );
				break;
		}
	},
	
	roomWindowId: function( room )
	{
		return 'room-' + room.name + '-win';
	},
	
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
