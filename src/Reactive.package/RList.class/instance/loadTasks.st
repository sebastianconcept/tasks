actions
loadTasks

	| someTasks |	

	someTasks := self tasks collect:[:e| RTask findAt: e].
	self tasks: (someTasks copyWithout: nil)