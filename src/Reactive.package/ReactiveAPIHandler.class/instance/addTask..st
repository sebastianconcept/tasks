actions
addTask: aRequest 

	| command |
	
	command := RAddTask fromJSONString: aRequest contents keys first.
	
	self databaseDo:[ | freshList task |
		task := command task.
		task save.
		freshList := RList findAt: command list id.
		(freshList includesTask: command task) ifFalse:[
			freshList addTask: command task; save]].

	^ ZnResponse ok: (ZnStringEntity text: ':)')