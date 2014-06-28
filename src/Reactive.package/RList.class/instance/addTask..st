actions
addTask: aTask

	self tasks: (self tasks, (Array with: aTask id)).
	
	^ aTask 