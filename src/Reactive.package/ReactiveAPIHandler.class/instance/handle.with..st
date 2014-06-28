actions
handle: aRequest with: aResponse
	
	| command |

	(self isValid: aRequest) ifFalse:[
		^ ZnResponse badRequest: aRequest].
	
	command := aRequest uri segments last.
	
	^ [[self perform: (command,':') asSymbol with: aRequest]
		on: ConnectionTimedOut
		do:[:x| ^ZnResponse ok: (ZnStringEntity text: 'backend offline') ]]
			on: Error
			do:[:x| 
				Flow isProduction ifFalse:[self halt].
				ZnResponse badRequest: aRequest]