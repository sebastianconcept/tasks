actions
removeTask: aTask

	self tasks: (self tasks asOrderedCollection remove: aTask id ifAbsent:[nil];yourself) asArray.
	
	^ aTask 