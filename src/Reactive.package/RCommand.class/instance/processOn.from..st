actions
processOn: anAPI from: socket
	"Broadcast to everyone except the sender."

	anAPI sendToAll: self but: socket 