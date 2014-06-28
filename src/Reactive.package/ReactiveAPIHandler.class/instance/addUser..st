actions
addUser: aRequest 

	| command |

	command := RAddUser fromJSONString: aRequest contents keys first.
	
	self databaseDo:[ |user|
		user := command user.
		user save].

	^ ZnResponse ok: (ZnStringEntity text: ':)')