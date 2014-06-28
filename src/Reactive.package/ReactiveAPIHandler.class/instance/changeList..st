actions
changeList: aRequest 

	| command |

	command := RChangeTask fromJSONString: aRequest contents keys first.
	
	self databaseDo:[ | list freshList |
		list := command list.
		freshList := RList findAt: list id.
		freshList resortUsing: list.
		freshList save].

	^ ZnResponse ok: (ZnStringEntity text: ':)')