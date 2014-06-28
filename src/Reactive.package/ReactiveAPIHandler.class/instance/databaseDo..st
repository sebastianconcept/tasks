actions
databaseDo: aBlock

 	MongoPool instance
		databaseAt: 'Reactive'
		do: [^ aBlock value]