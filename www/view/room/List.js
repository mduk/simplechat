Ext.define( 'SimpleChat.view.room.List', {
	extend: 'Ext.grid.Panel',
	alias: 'widget.roomList',
	
	initComponent: function()
	{
		this.store = {
			fields: [ 'name', 'topic', 'members' ],
			data: []
		};
		
		this.columns = [
			{ header: 'Room Name', dataIndex: 'name', flex: 1 },
			{ header: 'Topic', dataIndex: 'topic', flex: 1 },
			{ header: 'Members', dataIndex: 'members' }
		];
		
		this.callParent();
	},
	
	/**
	 * Given a room object, update that room in the list
	 */
	updateRoom: function( room )
	{
		var store = this.getStore();
		var pos = store.findExact( 'name', room.name );
		
		if ( pos == -1 )
		{
			store.add( room );
		}
		else
		{
			store.removeAt( pos );
			store.insert( pos, room );
		}
	},
	
	/**
	 * Update all rooms in the list
	 */
	updateList: function( list )
	{
		for ( var i = 0; i < list.length; i++ )
		{
			this.updateRoom( list[ i ] );
		}
	}
} );
