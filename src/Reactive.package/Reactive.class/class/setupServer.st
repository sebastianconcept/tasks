actions
setupServer

	Flow isProduction ifFalse:[ 
		ZnServer default delegate	
			addHandler: FlowJsCodeHandler new;
			addHandler: FlowStCodeHandler new;
			yourself].

	(ZnServer managedServers detect:[:e|
		e port = (ConfigurationFiles at: 'Reactive' at: 'websocketOn')]) 
			delegate: (FlowWebSocketDelegate new
						addHandler: ReactiveWebSocketsAPI new;
						yourself).
		
	ZnServer default delegate	
		addHandler: ReactiveAPIHandler new;
		addHandler: (FlowFilesHandler for: (FileLocator imageDirectory / 'service') resolve fullName base: 'service');
		yourself.
	