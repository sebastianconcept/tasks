accessing
api

	^ (ZnServer managedServers 
		detect:[:srv| srv port = (ConfigurationFiles at: 'Flow' at: 'port')]
		ifNone:[nil]) ifNotNilDo:[:srv|
			srv delegate handlers 
				detect:[:e| e class = ReactiveAPIHandler ]
				ifNone:[nil]]
