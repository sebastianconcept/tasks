actions
processOn: anAPI from: socket
	"Everybody update the current total number of connected clients."
	
	self clients: anAPI clients size. 
	anAPI sendToAll: self