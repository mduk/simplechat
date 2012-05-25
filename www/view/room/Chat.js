Ext.define( 'SimpleChat.view.room.Chat', {
	extend: 'Ext.window.Window',
	iconCls: 'icon-user-comment',
	maximizable: true,
	width: 400,
	height: 600,
	
	initComponent: function()
	{
		this.title = "Room: " + this.room.name + ' (' + this.room.topic + ')';
		this.callParent();
	},
	
	layout: 'border',
	
	tbar: [
		{
			text: "Set Topic",
			id: 'setTopicButton',
			iconCls: 'icon-application-edit'
		},
		{
			text: "Lock Topic",
			id: 'lockTopicButton',
			iconCls: 'icon-lock'	
		}
	],
	
	items: [
		{ 
			autoScroll: true,
			region: 'center', 
		},
		{ 
			region: 'south',
			
			xtype: 'form',
			
			url: '/',
			layout: 'anchor',
			defaults: {
				anchor: '100%'
			},
			
			defaultType: 'textfield',
			items: [
				{
					name: 'msg',
					enableKeyEvents: true,
					listeners: {
						specialkey: function( textfield, event )
						{
							if ( event.getKey() == event.ENTER )
							{
								var text = textfield.getValue();
								
								if ( text != "" )
								{
									textfield.up( 'window' ).say( text )
									textfield.setValue( "" );
								}
							}
						}
					}
				}
			]
		}
	],
	
	say: function( Message )
	{
		client.say( this.room, Message );
	},
	
	appendRoomEvent: function( data )
	{
		var timeline = this.getComponent( 0 );
		
		switch ( data.type )
		{
			case 'joined':
				timeline.add( {
					border: false,
					cls: 'room-event joined',
					html: new Ext.XTemplate( 
						'<span class="client">{client}</span> has joined the room.'
					).apply( data )
				} );
				break;
			
			case 'parted':
				timeline.add( {
					border: false,
					cls: 'room-event parted',
					html: new Ext.XTemplate(
						'<span class="client">{client}</span> has left the room.'
					).apply( data )
				} );
				break;
				
			case 'message':
				timeline.add( { 
					border: false,
					cls: 'room-event message',
					html: new Ext.XTemplate(
						'<span class="client">{client}</span><span class="body">{body}</span>'
					).apply( data ) 
				} );
				break;
			
			default:
				console.log( "Room Event not recognised:" );
				console.log( data );
				break;
		}
		
		// Scroll the panel down
		var d = timeline.body.dom;
		d.scrollTop = d.scrollHeight - d.offsetHeight;
	}
} );
