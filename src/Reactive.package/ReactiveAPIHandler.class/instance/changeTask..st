actions
changeTask: aRequest 

	| command |
	
	command := RChangeTask fromJSONString: aRequest contents keys first.

	self databaseDo:[ | task |
		task := command task.
		task save].

	^ ZnResponse ok: (ZnStringEntity text: ':)')