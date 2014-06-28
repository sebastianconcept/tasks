actions
getList: aRequest 

	| anId list |
	
	anId := aRequest uri queryAt: 'id'.
	
	self databaseDo:[
		list := RList findAt: anId.
		list ifNil:[
			list := RList new
					id: anId; 
					name: 'Things we''ll get done';
					save].
		list loadTasks].

	^ ZnResponse ok: (ZnStringEntity text: list asJSONString)