actions
addList: aRequest 

	| command |

	command := RAddList fromJSONString: aRequest contents keys first.
	
	self databaseDo:[ |list|
		list  := command list.
		list save].

	^ ZnResponse ok: (ZnStringEntity text: ':)')