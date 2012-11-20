Ext.define( 'SimpleChat.view.room.ListWindow', {
	extend: 'Ext.window.Window',
	title: 'Chat Rooms',
	iconCls: 'icon-silk-comments',
	width: 600,
	height: 600,
	scrollable: true,
	closable: false,
	
	tbar: [
		{
			text: "Connect",
			id: 'connectionButton',
			iconCls: 'icon-silk-connect'	
		},
		{ 
			text: "Create Room",
			id: "createRoomButton",
			iconCls: 'icon-silk-comment-add',
			disabled: true
		},
		{
			text: "Refresh List",
			id: "refreshList",
			iconCls: 'icon-silk-arrow-refresh',
			disabled: true
		},
		{
			text: "Shout",
			id: "shoutButton"
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
