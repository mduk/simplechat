function handle( p ) {
	switch ( p.source ) {
		case 'server': handleServer( p ); break;
		case 'room': handleRoom( p ); break;
		case 'client': handleClient( p ); break;
		default: handleUnknown( p ); break;
	}
}

function handleServer( p ) {
	switch ( p.type ) {
		default:
			console.log( "*** Unknown server message type" );
			console.log( p );
			break;
	}
}

function handleClient( p ) {
	switch ( p.type ) {
		
		/**
		 * Joined a room
		 */
		case 'joined':
			var room = p.room.name;
			
			// Hide the join modal
			$('#modal-join').modal('hide');
			
			// Update room list button
			var joinBtn = $('#room-list button[data-room="' + room + '"]');
			joinBtn.addClass( 'btn-danger' );
			joinBtn.html( '<i class="icon-remove"></i> Part' );
			joinBtn.attr( 'data-type', 'part' );
			
			// Create a new tab
			newTab( mkId( room ), room );
			
			// Switch to the new tab
			switchTab( mkId( room ) );
			break;
		
		/**
		 * Parted a room
		 */
		case 'parted':
			var btn = $( '#room-list-' + mkId( p.room ) + ' button' );
			btn.toggleClass( 'btn-danger' );
			btn.html( 'Join' );
			btn.attr( 'data-type', 'join' );
			deleteTab( mkId( p.room ) );
			break;
		
		default:
			console.log( "*** Unknown client message type" );
			console.log( p );
			break;
	}
}

function handleRoom( p ) {
	console.log( JSON.stringify( p ) );
	switch ( p.type ) {
		
		/**
		 * Someone joined a room
		 */
		case 'joined':
			var html = $('<li class="event joined">' + p.client + ' joined</li>');
			$('#' + mkId( p.room ) + ' ul.stream').append( html );
			break;
		
		/**
		 * Someone parted a room
		 */
		 case 'parted':
			var html = $('<li class="event joined">' + p.client + ' parted</li>');
			$('#' + mkId( p.room ) + ' ul.stream').append( html );
		 	break;
		 
		 /**
		  * Someone said something in a room
		  */
		 case 'message':
		 	var tab = $('.nav_tabs a[href="#' + mkId( p.room ) + '"]');
			var pane = $('#' + mkId( p.room ) );
			
			var html = $('#template-room-message').children().clone();
			html.find( 'img' ).attr( { 
				src: 'http://www.gravatar.com/avatar/' 
					+ MD5( p.client ) 
					+ '?default=identicon&s=32'
			} );
			html.find( 'h4'  ).html( p.client );
			html.find( 'p'   ).html( p.body );
			pane.find( 'ul.stream' ).append( html );
			break;
		 
		default:
			console.log( "*** Unknown room message type" );
			console.log( p );
			break;
	}
}

function handleUnknown( p ) {
	console.log( "*** Unknown message source" );
	console.log( p );
	
	switch ( p.type ) {
		
		/**
		 * Welcome
		 *
		 * Ident was successful
		 */
		case 'welcome':
			// Close the connect modal
			$('#modal-connect').modal('hide');
			
			// Request the room list
			cmd( { type: 'room_list' } );
			
			// Subscribe to the room list
			cmd( { type: 'subscribe', stream: 'room_list' } );
			break;
		
		/**
		 * Room Info
		 */
		case 'room_info':
			updateRoomList( p );
			break;
		
		default:
			console.log( "*** Unknown server message type" );
			console.log( p );
			break;
	}
}
