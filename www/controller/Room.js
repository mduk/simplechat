Ext.define( 'SimpleChat.controller.Room', {
	extend: 'Ext.app.Controller',
	
	init: function()
	{
		var controller = this;
		
		this.control( {
			'window' : {
				beforeclose: function( win, event )
				{
					controller.getController( 'Client' ).part( win.room.name );
					return false;
				}
			},
			'#setTopicButton' : {
				click: function( btn, event )
				{
					Ext.Msg.prompt( 'Set Topic', 'New topic:', function( clicked, topic )
					{
						if ( clicked != 'ok' )
						{
							return;
						}
						
						var room = btn.up( 'window' ).room;
						controller.setTopic( room.name, topic );
					} );
				}
			},
			'#lockTopicButton' : {
				click: function( btn, event )
				{
					var 
						btnText = btn.getText(),
						room = btn.up( 'window' ).room;
					
					
					if ( btnText == 'Lock Topic' )
					{
						controller.lockTopic( room.name );
					}
					else
					{
						controller.lockTopic( room.name );
					}
				}
			},
			'#textInput' : {
				specialkey: function( textfield, event )
				{
					if ( event.getKey() == event.ENTER )
					{
						var text = textfield.getValue();
						
						if ( text != "" )
						{
							controller.say(
								textfield.up( 'window' ).room.name,
								text
							);
							textfield.setValue( "" );
						}
					}
				}
			}
		} );
	},
	
	setTopic: function( room, topic )
	{
		this.getController( 'Client' ).sendPacket( {
			type: "set_topic",
			room: room,
			topic: topic
		} );
	},
	
	lockTopic: function( room )
	{
		this.getController( 'Client' ).sendPacket( {
			type: 'lock_topic',
			room: room
		} );
	},
	
	unlockTopic: function( room )
	{
		this.getController( 'Client' ).sendPacket( {
			type: 'unlock_topic',
			room: room
		} );
	},
	
	say: function( room, message )
	{
		this.getController( 'Client' ).sendPacket( {
			type: 'say',
			room: room,
			message: message
		} );
	},
	
	/**
	 * Handle room events
	 *
	 * @param object event The room event object
	 */
	handleEvent: function( event )
	{
		var roomWindow = this.roomWindow( { name: event.room } );
		
		switch ( event.type )
		{
			case "joined":
			case "parted":
			case "message":
			case "topic_locked":
			case "topic_unlocked":
				roomWindow.appendRoomEvent( event );
				break;
			
			case "topic_changed":
				roomWindow.setTitle( "Room: " + event.room + " (" + event.topic + ")" );
				roomWindow.appendRoomEvent( event );
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
