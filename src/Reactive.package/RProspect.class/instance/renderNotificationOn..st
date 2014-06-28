rendering
renderNotificationOn: aStream

	aStream
		cr;cr; 
		nextPutAll: 'A new prospect from http://tasks.flowingconcept.com';cr;cr;
		nextPutAll: self email;cr;cr;
		yourself.
	
	self feedback ifNotNil:[
		aStream 
			nextPutAll: 'Here is some feedback:'; cr;cr;
			nextPutAll: self feedback;cr;cr].
		
	aStream nextPutAll: 'May you be where the force is'