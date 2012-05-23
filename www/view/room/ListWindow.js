Ext.define( 'SimpleChat.view.room.ListWindow', {
	extend: 'Ext.window.Window',
	title: 'Chat Rooms',
	iconCls: 'icon-comments',
	width: 600,
	height: 600,
	scrollable: true,
	closable: false,
	
	tbar: [
		{
			text: "Connect",
			id: 'connectionButton',
			iconCls: 'icon-connect'	
		},
		{ 
			text: "Create Room",
			iconCls: 'icon-comment-add',
			disabled: true,
			handler: function( btn, e )
			{
				Ext.Msg.prompt( 'Room Name', 'Specify room name:', function( clicked, name )
				{
					if ( clicked != 'ok' )
					{
						return;
					}
					
					client.join( name );
					client.activeRooms();
				} );
			}
		},
		{
			text: "Refresh List",
			iconCls: 'icon-arrow-refresh',
			disabled: true,
			handler: function( btn, e )
			{
				client.activeRooms();
			}
		}
	],
	
	initComponent: function()
	{
		this.on( 'beforeclose', function()
		{
			return client.disconnect();
		} );
		
		this.callParent();
	},
	
	items: {
		id: 'room-list',
		xtype: 'roomList'
	}
} );
