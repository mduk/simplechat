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
			id: "createRoomButton",
			iconCls: 'icon-comment-add',
			disabled: true
		},
		{
			text: "Refresh List",
			id: "refreshList",
			iconCls: 'icon-arrow-refresh',
			disabled: true
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
