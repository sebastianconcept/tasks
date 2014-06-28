accessing
webSocketAPI

	^ (ZnServer managedServers 
		detect:[:srv| srv port = (ConfigurationFiles at: 'Reactive' at: 'websocketOn')]
		ifNone:[nil])
			ifNotNilDo:[:srv| srv delegate websocketsHandler]