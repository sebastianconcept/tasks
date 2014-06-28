Smalltalk current createPackage: 'Flow-Examples' properties: #{}!
PersistentModel subclass: #ToDo
	instanceVariableNames: 'name description'
	category: 'Flow-Examples'!

!ToDo methodsFor: 'accessing'!

name

	^ name
!

description

	^ description
!

description: aString

	description := aString
!

name: aString

	name := aString
! !

PersistentModel subclass: #ToDoList
	instanceVariableNames: 'toDos'
	category: 'Flow-Examples'!

!ToDoList methodsFor: 'accessing'!

toDos

	^ toDos ifNil:[self initializeToDos]
! !

!ToDoList methodsFor: 'actions'!

addToDo: aToDo

	^ self toDos add: aToDo
!

removeToDo: aToDo

	self toDos remove: aToDo
! !

!ToDoList methodsFor: 'initialization'!

initializeToDos

	^ toDos := Array new
! !

!ToDoList class methodsFor: 'actions'!

example

	^ self new 
		addToDo: (ToDo new 
					name: 'Start with the problem';
					yourself);
		addToDo: (ToDo new 
					name: 'Show how this problem hurts';
					yourself);
		addToDo: (ToDo new 
					name: 'Present the solution';
					yourself);
		addToDo: (ToDo new 
					name: 'Exaplain the benefits of using it';
					yourself);
		yourself
!

oneOfToDosFrom: aReifiedJSON

	ToDo reifyFrom: aReifiedJSON
! !

