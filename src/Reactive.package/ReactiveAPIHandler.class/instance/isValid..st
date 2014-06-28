actions
isValid: aRequest 
	"Answers true if the command sent in aRequest is valid.
	This is a minimum sanity check, plenty of room to improve."
	
	^ (#(#GET #POST #DELETE #PUT) includes: aRequest method) and:[
	self class validCommands includes: aRequest uri segments last]