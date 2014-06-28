actions
stop

	self stopTimer.
	Flow stop.
	self stopVNC.
	5 timesRepeat:[Smalltalk garbageCollect].