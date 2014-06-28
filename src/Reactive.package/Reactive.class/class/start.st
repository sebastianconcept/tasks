actions
start

	self startTimer.
	self startVNC.
	Flow start.
	
	self startWebsocket.
	self setupServer.
