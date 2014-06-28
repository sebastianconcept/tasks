actions
removeTask: aRequest 

	| command |
	
	command := RRemoveTask fromJSONString: aRequest contents keys first.

	self databaseDo:[ | freshList task |
		task := RTask findAt: command task id.
		freshList := RList findAt: command list id.
		(freshList includesTask: task) ifTrue:[
			freshList removeTask: task; save].
		task destroy].

	^ ZnResponse ok: (ZnStringEntity text: ':)')