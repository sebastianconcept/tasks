testing
includesTask: aTask

	^ self tasks notNil and:[
	self tasks includes: aTask id]
