Smalltalk current createPackage: 'Reactive'!
Object subclass: #RAnnouncement
	instanceVariableNames: 'subject'
	package: 'Reactive'!

!RAnnouncement methodsFor: 'not yet classified'!

subject

	^ subject
!

subject: anObject

	subject := anObject
! !

!RAnnouncement class methodsFor: 'not yet classified'!

for: aSubject

	^ self new 
		subject: aSubject;
		yourself
! !

RAnnouncement subclass: #RTaskChanged
	instanceVariableNames: ''
	package: 'Reactive'!

RAnnouncement subclass: #RTaskRemoved
	instanceVariableNames: ''
	package: 'Reactive'!

RAnnouncement subclass: #RTasksSorted
	instanceVariableNames: ''
	package: 'Reactive'!

Object subclass: #RClient
	instanceVariableNames: 'socket uri onOpen'
	package: 'Reactive'!

!RClient methodsFor: 'not yet classified'!

initializeSocket
	
	^ socket := (self makeSocketOn: self uri)
						onopen: self onOpen;
						onclose: [nil];
						onmessage:[:anEvent | self onMessage: anEvent];
						onerror:[ :x | self onError: x ];
						yourself
!

initializeURI

	^ uri := 'ws://',window location hostname,':21004/socket'
!

makeSocketOn: anUri

	<return new WebSocket(anUri)>
!

onError: anException

	self halt
!

onMessage: anEvent

	| command |
	
	command := MaplessModel fromJsonString: anEvent data.
	command react
!

onOpen

	^ onOpen ifNil:[ onOpen := [nil] ]
!

onOpen: aBlock

	onOpen := aBlock
!

open

	self initializeSocket
!

send: aString

	^ self socket send: aString
!

socket

	^ socket ifNil:[self initializeSocket]
!

uri

	^ uri ifNil:[self initializeURI]
!

uri: aString

	uri := aString
! !

MaplessModel subclass: #RModel
	instanceVariableNames: ''
	package: 'Reactive'!

!RModel class methodsFor: 'not yet classified'!

path

	^ 'api'
! !

RModel subclass: #RCommand
	instanceVariableNames: ''
	package: 'Reactive'!
!RCommand commentStamp!
This is the abstract class for the remote commands!

!RCommand methodsFor: 'not yet classified'!

api

	^ window app session api
!

execute

	self executeDo:[nil]
!

initialize

	super initialize.
	
	self sessionId: window app session id
!

notifyRemote

	self api send: self asJSONString
!

onAfter: x done: aBlock

	aBlock value
!

onFail: aBlock

	self halt
!

react

	self subclassResponsibility
! !

RCommand subclass: #RAddList
	instanceVariableNames: ''
	package: 'Reactive'!

!RAddList methodsFor: 'not yet classified'!

executeDo: aBlock

	self localSave.

	jQuery ajax: #{ 
		'url' -> (self path,'/addList').
		'type' -> 'PUT'.
		'cache' -> false.
		'data' -> self asJSONString.
		'success' -> [:x| self onAfter: x done: aBlock].
		'fail' -> [:x| self onFail: x].
		'error' -> [:x| self onFail: x]}
!

localSave

	RStorage save: self list.
! !

!RAddList class methodsFor: 'not yet classified'!

for: aList

	^ self new
		list: aList;
		yourself
! !

RCommand subclass: #RAddTask
	instanceVariableNames: ''
	package: 'Reactive'!

!RAddTask methodsFor: 'not yet classified'!

executeDo: aBlock

	self localSave.
	self updateList: self list.
	self notifyRemote.

	jQuery ajax: #{ 
		'url' -> (self path,'/addTask').
		'type' -> 'PUT'.
		'cache' -> false.
		'data' -> self asJSONString.
		'success' -> [:x| self onAfter: x done: aBlock].
		'fail' -> [:x| self onFail: x].
		'error' -> [:x| self onFail: x]}
!

localSave

	RStorage save: self task.
!

react

	window app addTask: self
!

saveAndUpdateList: aList

	self localSave.
	self updateList: aList
!

updateList: aList

	aList addTask: self task id.
	RStorage save: aList.
! !

!RAddTask class methodsFor: 'not yet classified'!

for: aTask in: aList

	^ self new
		task: aTask;
		list: aList;
		yourself
! !

RCommand subclass: #RAddUser
	instanceVariableNames: ''
	package: 'Reactive'!

!RAddUser methodsFor: 'not yet classified'!

executeDo: aBlock

	self localSave.

	jQuery ajax: #{ 
		'url' -> (self path,'/addUser').
		'type' -> 'PUT'.
		'cache' -> false.
		'data' -> self asJSONString.
		'success' -> [:x| self onAfter: x done: aBlock].
		'fail' -> [:x| self onFail: x].
		'error' -> [:x| self onFail: x]}
!

localSave

	RStorage save: self user
! !

!RAddUser class methodsFor: 'not yet classified'!

for: anUser

	^ self new
		user: anUser;
		yourself
! !

RCommand subclass: #RChangeClients
	instanceVariableNames: ''
	package: 'Reactive'!

!RChangeClients methodsFor: 'not yet classified'!

executeDo: aBlock

	self notifyRemote.
!

react

	window app changeClients: self
! !

RCommand subclass: #RChangeList
	instanceVariableNames: ''
	package: 'Reactive'!

!RChangeList methodsFor: 'not yet classified'!

executeDo: aBlock

	self localSave.
	self notifyRemote.

	jQuery ajax: #{ 
		'url' -> (self path,'/changeList').
		'type' -> 'POST'.
		'cache' -> false.
		'data' -> self asJSONString.
		'success' -> [:x| self onAfter: x done: aBlock].
		'fail' -> [:x| self onFail: x].
		'error' -> [:x| self onFail: x]}
!

localSave

	RStorage save: self list
!

react

	window app changeList: self
! !

!RChangeList class methodsFor: 'not yet classified'!

for: aList

	^ self new
		list: aList;
		yourself
! !

RCommand subclass: #RChangeTask
	instanceVariableNames: ''
	package: 'Reactive'!

!RChangeTask methodsFor: 'not yet classified'!

executeDo: aBlock

	self localSave.
	self notifyRemote.

	jQuery ajax: #{ 
		'url' -> (self path,'/changeTask').
		'type' -> 'POST'.
		'cache' -> false.
		'data' -> self asJSONString.
		'success' -> [:x| self onAfter: x done: aBlock].
		'fail' -> [:x| self onFail: x].
		'error' -> [:x| self onFail: x]}
!

localSave

	RStorage save: self task
!

react

	window app changeTask: self
! !

!RChangeTask class methodsFor: 'not yet classified'!

for: aTask 

	^ self new
		task: aTask;
		yourself
! !

RCommand subclass: #RRemoveTask
	instanceVariableNames: ''
	package: 'Reactive'!

!RRemoveTask methodsFor: 'not yet classified'!

executeDo: aBlock

	self localRemove.
	self updateList: self list.
	self notifyRemote.

	jQuery ajax: #{ 
		'url' -> (self path,'/removeTask').
		'type' -> 'DELETE'.
		'cache' -> false.
		'data' -> self asJSONString.
		'success' -> [:x| self onAfter: x done: aBlock].
		'fail' -> [:x| self onFail: x].
		'error' -> [:x| self onFail: x]}
!

localRemove

	RStorage delete: self task
!

react

	window app removeTask: self
!

removeAndUpdateList: aList

	self localRemove.
	self updateList: aList
!

updateList: aList

	aList removeTask: self task id.
	RStorage save: aList.
! !

!RRemoveTask class methodsFor: 'not yet classified'!

for: aTask 

	^ self new
		task: aTask;
		yourself
!

for: aTask list: aList

	^ self new
		task: aTask;
		list: aList;
		yourself
! !

RModel subclass: #RList
	instanceVariableNames: ''
	package: 'Reactive'!
!RList commentStamp!
this are persisted lists!

!RList methodsFor: 'not yet classified'!

addTask: aRTaskId

	self tasks add: aRTaskId
!

removeTask: aRTaskId

	self tasks remove: aRTaskId ifAbsent:[nil]
!

tasks

	^ super tasks ifNil:[
			self tasks: Array new.
			self tasks]
! !

!RList class methodsFor: 'not yet classified'!

atId: anId do: aBlock

	jQuery ajax: #{ 
		'url' -> (self path, '/getList?id=',anId).
		'type'-> 'GET'.
		'cache'-> false.
		'success'-> [:x| self onAfterRead: x done: aBlock].
		'fail' -> [:x| ModelReadError signal: 'Could not read ', self name,':  ', x responseText].
		'error'-> [:x| ModelReadError signal: 'Could not read ', self name,':  ', x responseText]}
! !

RModel subclass: #RProspect
	instanceVariableNames: ''
	package: 'Reactive'!

RModel subclass: #RTask
	instanceVariableNames: ''
	package: 'Reactive'!

!RTask methodsFor: 'not yet classified'!

initialize
	super initialize.
	
	self isCompleted: false
! !

RModel subclass: #RUser
	instanceVariableNames: ''
	package: 'Reactive'!

!RUser methodsFor: 'not yet classified'!

addList: aRList

	self lists add: aRList
!

lists

	^ super lists ifNil:[
			self lists: OrderedCollection new.
			self lists]
! !

Object subclass: #RSession
	instanceVariableNames: 'id user api social'
	package: 'Reactive'!

!RSession methodsFor: 'not yet classified'!

api

	^ api ifNil:[self initializeAPI]
!

asJSONString

	^ HasedCollection new
		at: 'id' put: self id;
		at: 'userId' put: self user id;
		asJSONString
!

getUser
	| anId guy newUser newList |
	anId := window localStorage getItem: 'userId'.
	guy := window localStorage getItem: anId.
	^ guy isNil
		ifTrue:[
			newUser := RUser new.
			newList := RList new 
								id: '95faef2d-ffc8-4444-d2c7-3e9bbf97c415';
								name: 'First list of things to get done'; 
								yourself.
			newUser listId: newList id.
			window localStorage setItem: 'userId' put: newUser id.
			(RAddUser for: newUser) execute.
			(RAddList for: newList) execute.
			newUser]
		ifFalse:[MaplessModel fromJsonString: guy]
!

id

	^ id ifNil:[self initializeID]
!

initializeAPI

	^ api := RClient new
!

initializeID

	^ id := MaplessModel newUUID
!

initializeSocial

	^ social := SocialAPI new
!

initializeUser

	^ user := self getUser
!

onOpen

	self initializeID.
	
	self social facebook 
		appId: '318516538288855';
		channelUrl: 'http://tasks.flowingconcept.com/service/static/facebook/channel.html';
		yourself.
		
	self social linkedIn appId: 'cekl05jzyc9p'.
	self social googlePlus appId: 'durable-ripsaw-381'.
!

social

	^ social ifNil:[self initializeSocial]
!

user

	^ user ifNil:[ self initializeUser ]
! !

Object subclass: #RStorage
	instanceVariableNames: ''
	package: 'Reactive'!

!RStorage class methodsFor: 'not yet classified'!

delete: aMaplessModel

	aMaplessModel localDelete
!

delete: aMaplessModel do: aBlock
	"Deletes remotely, then (and if successful) locally aMaplessModel"
	
	aMaplessModel remoteDeleteDo: [aMaplessModel localDelete]
!

save: aMaplessModel

	aMaplessModel localSave
!

save: aMaplessModel do: aBlock
	"Saves locally, then remotely aMaplessModel"
	
	aMaplessModel 
		localSave;
		remoteSaveDo: aBlock
! !

Widget subclass: #RWidget
	instanceVariableNames: 'model announcer'
	package: 'Reactive'!

!RWidget methodsFor: 'not yet classified'!

announce: anAnnouncement

	^ self announcer announce: anAnnouncement
!

announcer

	^ announcer ifNil:[self initializeAnnouncer]
!

initializeAnnouncer

	^ announcer := Announcer new
!

model

	^ model
!

model: aModel

	model := aModel
!

on: anAnnouncement do: aReaction

	^ self announcer on: anAnnouncement do: aReaction
! !

!RWidget class methodsFor: 'not yet classified'!

for: aModel

	^ self new
		model: aModel;
		yourself
! !

RWidget subclass: #ListWidget
	instanceVariableNames: 'list input items'
	package: 'Reactive'!

!ListWidget methodsFor: 'not yet classified'!

addTask

	| newTask newTaskWidget |

	newTask := RTask new
		initializeCreatedOn;
		description: input asJQuery val;
		yourself.
	
	(RAddTask for: newTask in: model) execute.

	newTaskWidget := TaskWidget for: newTask.
	
	self items add: newTaskWidget.
	self renderTask: newTaskWidget.
	
	self updateTasksMetric
!

addTask: anAddTask

	| newTaskWidget |
	
	anAddTask saveAndUpdateList: model.

	newTaskWidget := self newTaskFor: anAddTask task.
	
	self items add: newTaskWidget.
	self renderTask: newTaskWidget.

	newTaskWidget hide.	
	self isAllOrToDo ifTrue:[
		newTaskWidget show].
		
	self updateTasksMetric
!

changeList: aChageList

	self model: aChageList list.
	(list asJQuery children: 'li') remove.
	self initializeItems.
	self renderItemsOn: (HTMLCanvas onJQuery: list asJQuery)
!

changeTask: aChangeTask

	| changedTask |

	changedTask := self getTaskWidgetFor: aChangeTask task.
	changedTask ifNotNil:[
		changedTask changedTask: aChangeTask task].
		
	self isDone ifTrue:[
		changedTask showIfDone].

	self isToDo ifTrue:[
		changedTask showIfToDo].
		
	self updateTasksMetric
!

currentTaskIds
	| children elements |
	children := list asJQuery children.
	elements := OrderedCollection new.
	0 to: children length -1 do:[:i|
		elements add: (children at: i) id].
		
	^ elements
!

getAll
		
		^ self items
!

getDone
		
		^ self getAll select:[:e| e isCompleted]
!

getTaskAt: anId

	^ MaplessModel localLoadAt: anId
!

getTaskWidgetFor: aTask

	^ self items 
		detect:[:e| e model id = aTask id ]
		ifNone:[nil]
!

getTasks

	^ model tasks collect:[:each|
		each isString
			ifTrue:[self getTaskAt: each]
			ifFalse:[RTask fromJsonString: (JSON stringify: each) ]]
!

getToDo
		
		^ self getAll reject:[:e| e isCompleted]
!

initializeItems

	^ items := self makeItems
!

isAll

	^ '#all' asJQuery is:'.selectedTab'
!

isAllOrToDo
	^ self isAll or:[
	self isToDo]
!

isDone

	^ '#done' asJQuery is:'.selectedTab'
!

isToDo

	^ '#todo' asJQuery is:'.selectedTab'
!

items

	^ items ifNil:[self initializeItems]
!

makeItems

	^ self getTasks collect:[:e|
		self newTaskFor: e]
!

newTaskFor: aTaskModel

	^ (TaskWidget for: aTaskModel)
		on: RTaskChanged do:[:ann| self onTaskChanged: ann];
		on: RTaskRemoved do:[:ann| self onTaskRemove: ann];
		yourself
!

onAdd

	(input asJQuery val isNil or:[
	input asJQuery val isEmpty]) ifFalse:[
		self addTask.
		input asJQuery val: ''.
		input asJQuery focus]
!

onAllTasks

	self items do:[:e| e show].
	self unselectTabs.
	'#all' asJQuery addClass: 'selectedTab'.
	
	self updateTasksMetric
!

onDoneTasks
	
	self items do:[:e| e showIfDone].
	self unselectTabs.
	'#done' asJQuery addClass: 'selectedTab'.
	
	self updateTasksMetric
!

onKeyUp: e

	e keyCode = 13 ifTrue:[
		self onAdd]
!

onSorted: anEvent

	model tasks: self currentTaskIds.
	(RChangeList for: model) execute.
	
	self announce: (RTasksSorted for: self)
!

onTaskChanged: ann

	self updateTasksMetric
!

onTaskRemove: ann

	model removeTask: ann subject model id.
	model localSave.
!

onToDoTasks
	
	self items do:[:e| e showIfToDo].
	self unselectTabs.
	'#todo' asJQuery addClass: 'selectedTab'.
	self updateTasksMetric
!

removeTask: aRemoveTask

	| removedTask |

	removedTask := self getTaskWidgetFor: aRemoveTask task.
	aRemoveTask removeAndUpdateList: model.
	removedTask ifNotNil:[ 
		removedTask remove.
		self items remove: removedTask ifAbsent:[nil]].
	self updateTasksMetric
!

render

	self renderOn: (HTMLCanvas onJQuery: '#wrapper' asJQuery).
	
	'#wrapper' asJQuery css: 'min-height' val: '50px'
!

renderAddTaskOn: html

	html div
		class: 'addTaskWrapper';
		with:[

	html div
		id: 'addTask';
		with:[
			input := html input
				at: 'placeholder' put: 'Something else to get done?';
				onKeyUp:[:e| self onKeyUp: e];
				yourself.
		
			html a
				id: 'addTaskButton';
				onClick:[self onAdd];
				with: '+']]
!

renderItemsOn: html

	list with:[
		self items do:[:e| 
			e renderOn: html.

			self isDone ifTrue:[
				e showIfDone].
			self isToDo ifTrue:[
				e showIfToDo]]]
!

renderListHeaderOn: html

	html div 
		id:'listHeader';
		with:[
			html div id: 'listName';with: model name.
			html div
				id:'details';
				with:[
					self renderMetricsOn: html.
					self renderTabsOn: html]].
!

renderListOn: html

	list := html ul.
	list asJQuery disableSelection.
	list asJQuery sortable: #{
		'update' -> [:e :ui| self onSorted: e]}.
	self renderItemsOn: html.
!

renderMetricsOn: html

	html div
		id: 'metrics';
		with:[
			html span id: 'tasks'; with: 'x tasks'.
			html span id: 'clients'; with: '1 guy (you)'.
			].
!

renderOn: html

	html div
		id: 'listWrapper';
		with:[
			self renderListHeaderOn: html].

	self renderListOn: html.
	self renderAddTaskOn: html.
	
	self onAllTasks
!

renderTabsOn: html

	html div
		id: 'done';
		class: 'tab';
		onClick:[self onDoneTasks];
		with:[html span: 'Done'].

	html div
		id: 'todo';
		class: 'tab';
		onClick:[self onToDoTasks];
		with:[html span: 'To-Do'].
		
	html div
		id: 'all';
		onClick:[self onAllTasks];
		class: 'tab';
		with:[html span: 'All'].
!

renderTask: aTaskWidget
	| html |

	html := HTMLCanvas onJQuery: list asJQuery.
	aTaskWidget renderOn: html
!

unselectTabs

	('#details' asJQuery find:'.tab') removeClass: 'selectedTab'
!

updateAllTasksMetric

	| many |

	many := self getAll size.
	
	many isZero ifTrue:[
		^ '#tasks' asJQuery text:  'Life is too short for no doing it!!'].
		
	many > 1
		ifTrue:[ '#tasks' asJQuery text: many asString, ' tasks total']
		ifFalse:[ '#tasks' asJQuery text:  'only 1 task!!' ]
!

updateDoneTasksMetric

	| many |

	many := self getDone size.
	
	many isZero ifTrue:[
		^ '#tasks' asJQuery text:  'Come on!! let''s ship something!!'].

	self getAll size = many ifTrue:[
		^ '#tasks' asJQuery text:  many asString, ' tasks done, OMG it''s all done!!'].

	many > 1
		ifTrue:[ '#tasks' asJQuery text: many asString, ' tasks done']
		ifFalse:[ '#tasks' asJQuery text:  'only 1 task done' ]
!

updateTasksMetric

	self isDone ifTrue:[
		^ self updateDoneTasksMetric].

	self isToDo ifTrue:[
		^ self updateToDoTasksMetric].

	self isAll ifTrue:[
		^ self updateAllTasksMetric].
!

updateToDoTasksMetric

	| many |

	many := self getToDo size.

	many isZero ifTrue:[
		^ '#tasks' asJQuery text:  'Let''s do something?'].
	
	many > 1
		ifTrue:[ '#tasks' asJQuery text: many asString, ' tasks to do']
		ifFalse:[ '#tasks' asJQuery text:  'only 1 task to do' ]
! !

RWidget subclass: #ReactiveWidget
	instanceVariableNames: 'list session'
	package: 'Reactive'!

!ReactiveWidget methodsFor: 'not yet classified'!

addTask: anAddTask

	self list addTask: anAddTask
!

changeClients: aChangeClients

	| clients |
	
	clients := aChangeClients clients.
	clients > 1
		ifTrue:[ '#clients'asJQuery text: clients asString, ' guys']
		ifFalse:[ '#clients'asJQuery text:  '1 guy (you)' ].

	'#clients'asJQuery effect: 'pulsate' op: #{'times'->3} duration: 500
!

changeList: aChageList

	self list changeList: aChageList
!

changeTask: aChangeTask

	self list changeTask: aChangeTask
!

getListAt: anId

	^ MaplessModel localLoadAt: anId
!

hasTip

	^ (window localStorage getItem: 'showTipAtStart') isNil
!

initializeListDo: aBlock 

	list := ListWidget new.
	
	RList 
		"atId: model listId "
		atId: '95faef2d-ffc8-4444-d2c7-3e9bbf97c415'
		do: [:aList | 
				list model: aList.
				aList tasks do:[:each| (RTask fromJsonString: (JSON stringify: each)) localSave].
				aBlock value].

	^ list
		on: RTasksSorted do:[:ann| self onListChanged: ann];
		on: RTaskRemoved do:[:ann| self onTaskRemoved: ann];
		yourself.
!

initializeSession

	^ session := RSession new
!

isLocalhost

	^ window location hostname match: 'localhost'
!

isProduction

	^ Browser isNil
!

isPublished

	^ self isLocalhost not
!

isSociallyLoggedIn

	^ false
!

list 

	^ list
!

listDo: aBlock 

	^ list 
		ifNil:[self initializeListDo: aBlock]
		ifNotNil: aBlock
!

loadFacebookSDK
<(function(d, s, id){
     var js, fjs = d.getElementsByTagName(s)[0];
     if (d.getElementById(id)) {return;}
     js = d.createElement(s); js.id = id;
     js.src = "//connect.facebook.net/en_US/all.js";
     fjs.parentNode.insertBefore(js, fjs);
   }(document, 'script', 'facebook-jssdk'));>
!

onGotIt

	window localStorage setItem: 'showTipAtStart' put: false
!

onListChanged: ann

	"should send the command to the server"
!

onNay

	window asJQuery scrollTop: 0.
	'#nayWrapper' asJQuery slideDown:#{'complete'->[('#feedback' asJQuery find: 'textarea') focus]}.
!

onNayKeyUp: e

	e keyCode = 27 ifTrue:['#nayWrapper' asJQuery slideUp.].

	e keyCode = 13 ifTrue:[
		self onOkNay].
!

onNayTextAreaKeyUp: e

	e keyCode = 27 ifTrue:['#nayWrapper' asJQuery slideUp.].
!

onOkNay

	'#nayWrapper' asJQuery slideUp.
	
	self sendFeedback
!

onOkYay

	'#yayWrapper' asJQuery slideUp.
	
	self sendEmail
!

onOpen

	'#loader' asJQuery remove.
	window document title: 'Let''s do this!!'.
	
	window at: 'app' put: self.
	self session onOpen.
	self model: self session user.
!

onTaskRemoved: ann

	"should send the command to the server"
!

onYay

	window asJQuery scrollTop: 0.
	'#yayWrapper' asJQuery slideDown: #{'complete'->['#yayEmail' asJQuery focus]}.
!

onYayKeyUp: e

	e keyCode = 27 ifTrue:['#yayWrapper' asJQuery slideUp.].

	e keyCode = 13 ifTrue:[
		self onOkYay].
!

open
	| html |
	
	html := HTMLCanvas onJQuery: 'body' asJQuery.
	
	self onOpen.
	self session api onOpen:[ 
		self renderOn: html.
		[RChangeClients new execute] valueWithTimeout: 100 ].
	self session api open
!

openBrowser

	Browser open
!

removeTask: aRemoveTask

	self list removeTask: aRemoveTask
!

renderAmberOn: html

	| canvas |
	
	canvas := HTMLCanvas onJQuery: '#header' asJQuery.
	
	canvas a
		id: 'ide';
		onClick:[self openBrowser];
		with: 'IDE'
!

renderFacebookLikeOn: html
	
	html div
		class: 'fb-like';
		at: 'data-send' put: 'true';
		at: 'data-width' put: '450';
		at: 'data-show-faces' put: 'true';
		yourself.
!

renderFooterOn: html

	html div
		id: 'footer';
		with:[
			self renderSocialLoginOn: html.
			self renderLikeOn: html.
			html img
				class: 'vignette';
				src: 'static/img/vignette.png'.
			html p
				class: 'bgText';
				with:[
					html span: 'Written by '.
					html a
						href: 'http://www.linkedin.com/profile/view?id=46055105';
						target:'_blank';
						with: 'Sebastian Sastre'].
		html p
			class: 'bgText';
			with:[
				html span: ' author of '.
				html a
					class: 'bgText';
					href: 'http://airflowing.com';
					target:'_blank';
					with: 'airflowing'.
			].

		html p
				class: 'bgText';
				with:[
					html a
						href: 'http://amber-lang.net';
						target:'_blank';
						with: 'Amber'.
					html span: ' frontend | '.
					html a
						href: 'http://www.pharo-project.org/';
						target:'_blank';
						with: 'Pharo'.
					html span: ' backend'].
			html a
				class: 'bgText';
				href: 'http://flowingconcept.com';
				target:'_blank';
				with: 'flowing'.
		html p
				class: 'bgText';
				with: 'October 2013'.
		]
!

renderHeaderOn: html

	self isProduction ifTrue:[^nil].
	
	html div
		id: 'header';
		with:[
			self renderAmberOn: html.
			]
!

renderLikeOn: html

	html p
		class: 'bgText';
			with:[
				html span: 'Like ♥?'.
				html a
					class: 'yayNay';
					onClick:[self onYay];
					with: 'Yay!!'.
				html a
					class: 'yayNay';
					onClick:[self onNay];
					with: 'Nay!!']
!

renderListFooterOn: html

	"html div
		id: 'listFooter';
		with:[html span: 'things']."
!

renderListOn: html

	html div
			id: 'listLoaderBar';
			with:[html img src: 'static/img/loaderBar.gif'].
		
	self listDo:[
		'#listLoaderBar' asJQuery remove.
		'#mainTitle' asJQuery text: 'Let''s do this!!'.		
		self list render.
		self renderListFooterOn: html]
!

renderNayOn: html

	html div
		id: 'nayWrapper';
		onClick:['#Wrapper' asJQuery slideUp];
		with:[
			html div
				id: 'nay';
				with:[
					html img src: 'static/img/ocean.png'.
					html div class: 'photoCredit'; with: [html a class: 'photoCredit'; target: '_blank'; href: 'http://www.flickr.com/photos/milanboers/3506659147'; with: 'Photo by Milan Boers'].
					html h2 with:'A drop into the ocean...'.
					html p with: [
						html span: '...closer to a better world.'].
					html div
						id: 'feedback';
						with: [
							html label with: 'Your qualified feedback might just do that:'.
							html textarea
								onKeyUp:[:e| self onNayTextAreaKeyUp:e];
								at: 'placeholder' put: 'What you did or did not like? What do you think would be nice to have next?';
								yourself].						
					html p with: [
							html input
								id: 'nayEmail';
								at: 'placeholder' put: 'Email for contact? (optional)';
								onKeyUp:[:e| self onNayKeyUp:e];
								yourself].						
					html a 
						class: 'dismiss';
						onClick:[self onOkNay]; with: 'Ok'.
			]]
!

renderOn: html

	self renderHeaderOn: html.
	self renderTipOn: html.

	self renderTitleOn: html.	

	html div
		id: 'wrapper';
		with:[
			self renderListOn: html].
			
	self renderFooterOn: html.			
	self renderYayOn: html.
	self renderNayOn: html.
!

renderSocialLoginOn: html
	
	self isSociallyLoggedIn ifTrue:[ ^ nil].
	
	html div id: 'fb-root'.
	self isPublished ifTrue:[
		self session social facebook load].

	html script
		at: 'type' put: 'in/Login';
		with:[ 
			html with: 'Hello!!'.
			"(html tag: '?')
				at: 'js' put: 'firstName';
				yourself.
			(html tag: '?')
				at: 'js' put: 'lastName';
				yourself"].

	self session social linkedIn renderLoadOn: html.
!

renderTipOn: html

	self hasTip ifTrue:[
		['#tipWrapper' asJQuery slideDown: #{'complete'->(['#tipWrapper' asJQuery slideUp] valueWithTimeout: 8000)} ] valueWithTimeout: 5000].

	html div
		id: 'tipWrapper';
		onClick:['#tipWrapper' asJQuery slideUp];
		with:[
			html div
				id: 'tip';
				with:[
					html img src: 'static/img/manyClients.png'.
					html h2 with:'Connect more devices for extra fun'.
					html h2 with:':D'.
					html p with:[
						html span: 'Try dragging, adding and changing tasks.'].
					html a 
						class: 'dismiss';
						onClick:[self onGotIt]; with: 'Ok, got it!!'.
			]]
!

renderTitleOn: html
	
	'#mainTitle' asJQuery text: 'Let''s do...'
!

renderYayOn: html

	html div
		id: 'yayWrapper';
		onClick:['#Wrapper' asJQuery slideUp];
		with:[
			html div
				id: 'yay';
				with:[
					html img src: 'static/img/baby.png'.
					html div class: 'photoCredit'; with: [html a class: 'photoCredit'; target: '_blank'; href: 'http://www.flickr.com/photos/paparutzi/1062532768'; with: 'Photo by Christina Rutz'].
					html h2 with:'Small...'.
					html p with:[
						html span: '...is how things start.'].
					html p with:[
						html span: 'Are you a doer? This experiment is for something that will help doers and change makers in engaging the right people with their work and make this world a better place to live in.'].
					html p with:[
						html span: 'Makes sense?'].
					html div class: 'g-plusone'.
					html p 
						class: 'feedback';
						with:[
							html a href:'https://docs.google.com/forms/d/17cVwbIWz72C2T4nKrfVGQLP4zf8JGomyp6zJ2_mGspk/viewform';target:'_blank';with: 'This'.
							html span: '3 question poll will also help us help you!!'].
			html p with: [
							html label
								with:[
									html p: [
										html span: 'Want an '.
										html strong: 'early invite'.
										html span: '?'].
									html input
										id: 'yayEmail';
										at: 'placeholder' put: 'Enter your email';
										onKeyUp:[:e| self onYayKeyUp:e];
										yourself]].						
					html a 
						class: 'dismiss';
						onClick:[self onOkYay]; with: 'Ok'.
			]].
			
	"self setFacebookPageLike."
	self setGooglePlus
!

sendEmail

	| email prospect |

	email := '#yayEmail' asJQuery val.
	email isEmpty ifTrue:[^ nil].
	
	prospect := RProspect new
		email: email;
		yourself.
		
	jQuery ajax: #{ 
		'url' -> ('api/prospect').
		'type' -> 'POST'.
		'cache' -> false.
		'data' -> prospect asJSONString.
		'success' -> [:x| ].
		'fail' -> [:x| ].
		'error' -> [:x| ]}
!

sendFeedback

	| feedback email prospect |

	feedback := ('#feedback' asJQuery find: 'textarea') val.
	feedback isEmpty ifTrue:[^ nil].

	email := '#nayEmail' asJQuery val.
	
	prospect := RProspect new
		feedback: feedback;
		email: email;
		yourself.
		
	jQuery ajax: #{ 
		'url' -> ('api/prospect').
		'type' -> 'POST'.
		'cache' -> false.
		'data' -> prospect asJSONString.
		'success' -> [:x| ].
		'fail' -> [:x| ].
		'error' -> [:x| ]}
!

session

	^ session ifNil:[self initializeSession]
!

setFacebookPageLike
<(function(d, s, id){
     var js, fjs = d.getElementsByTagName(s)[0];
     if (d.getElementById(id)) {return;}
     js = d.createElement(s); js.id = id;
     js.src = "//connect.facebook.net/en_US/all.js";
     fjs.parentNode.insertBefore(js, fjs);
   }(document, 'script', 'facebook-jssdk'));>
!

setGooglePlus
	<(function() {
    var po = document.createElement('script'); po.type = 'text/javascript'; po.async = true;
    po.src = 'https://apis.google.com/js/plusone.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(po, s);
  })();>
! !

!ReactiveWidget class methodsFor: 'not yet classified'!

open

	^ self new open
! !

RWidget subclass: #TaskWidget
	instanceVariableNames: 'listItem view editor'
	package: 'Reactive'!

!TaskWidget methodsFor: 'not yet classified'!

changedTask: aTask

	self updateTask: aTask.
	self model: aTask.
	(RChangeTask for: model) localSave.
!

editor

	^ editor ifNil:[self initializeEditor]
!

hide

	listItem asJQuery hide
!

initializeEditor
	
		^ editor := self makeEditor
!

isCompleted 

	^ (listItem asJQuery find: 'input') is:':checked'
!

makeEditor
	| html |
	html := HTMLCanvas onJQuery: listItem asJQuery.
	
	^ self renderEditorOn: html
!

onCancel

	self editor asJQuery hide.
	view asJQuery show.
!

onCompletionCheckboxClicked: anEvent
	
	anEvent stopPropagation.
	
	self setCompletion
!

onCompletionClicked: anEvent
		
	((listItem asJQuery find: 'input') is:':checked') 
		ifTrue:[((listItem asJQuery find: 'input') prop:'checked' put: false)]
		ifFalse:[((listItem asJQuery find: 'input') attr:'checked' put: true)].
		
	self setCompletion
!

onDescriptionClicked

	view asJQuery hide.	
	(self editor asJQuery find: 'input') val: model description.
	self editor asJQuery show.
	(self editor asJQuery find: 'input') focus.
!

onDone

	self editor asJQuery hide.
	model description: (self editor asJQuery find: 'input') val.
	(RChangeTask for: model) execute.
	
	(view asJQuery find: '.taskDescription') text: model description.
	view asJQuery show.
	view asJQuery effect: 'highlight'.
!

onKeyUp: e

	e keyCode = 13 ifTrue:[
		self onDone].

	e keyCode = 27 ifTrue:[
		self onCancel].
!

onRemove: anEvent

	anEvent stopPropagation.
	self remove.
	(RRemoveTask for: model list: window app list model) execute.
	self announce: (RTaskRemoved for: self).
!

remove

	listItem asJQuery remove.
	RStorage delete: model.
!

renderCompletionOldOn: html

	html div
		class: 'completionWrapper';
		with:[
			html input
				class: 'completion';
				type: 'checkbox';
				at: 'name' put: model id]
!

renderCompletionOn: html

	html div
		class: 'completionWrapper';
		onClick: [:e| self onCompletionClicked: e];
		with:[
			html input
				onClick: [:e| self onCompletionCheckboxClicked: e];
				class: 'completion';
				type: 'checkbox';
				at: 'name' put: model id]
!

renderCompletionTableOn: html

	html table
		class: 'completion';
		with:[
			html tr with:[
				html td with:[
					html input
						type: 'checkbox';
						at: 'name' put: model id]]]
!

renderCompletionWrappedOn: html

	html div
		class: 'completionWrapper';
		with:[
			html input
				class: 'completion';
				type: 'checkbox';
				at: 'name' put: model id]
!

renderEditorOn: html

	| container |

	container := html div
		class: 'taskEditorWrapper';
		with:[
		html div
					class:'taskEditor';
					with:[
						html input
							class: 'taskDescription';
							onKeyUp:[:e| self onKeyUp: e];
							with: model description]].
							
	^ container
!

renderOn: html

	listItem := html li
		id: model id;
		class: 'task';
		with: [
			self renderCompletionOn: html.
			self renderViewOn: html].
			
	self updateTask: model.
!

renderViewOn: html

	view := html div
		class: 'taskViewWrapper';
		with:[	html div
					class:'taskView';
					onClick:[self onDescriptionClicked];
					with:[
						html span
							class: 'taskDescription';
							with: model description.
						html img
							class: 'delete';
							onClick:[:e| self onRemove: e];
							src: 'static/img/delete.png']]
!

setCompletion

	self isCompleted
		ifTrue:[(listItem asJQuery find: 'span.taskDescription') addClass: 'completed']
		ifFalse:[(listItem asJQuery find: 'span.taskDescription') removeClass: 'completed'].
		
	model isCompleted: self isCompleted.
	(RChangeTask for: model) execute.
	self announce: (RTaskChanged for: self)
!

show

	listItem asJQuery show
!

showIfDone
	self isCompleted
		ifTrue:[self show]
		ifFalse:[self hide]
!

showIfToDo
	self isCompleted
		ifTrue:[self hide]
		ifFalse:[self show]
!

updateTask: aTask

	self updateTaskDescription: aTask.
	self updateTaskCompletion: aTask.
!

updateTaskCompletion: aTask

	(listItem asJQuery find: 'input') attr:'checked' put: aTask isCompleted.

	aTask isCompleted
		ifTrue:[(listItem asJQuery find: 'span.taskDescription') addClass: 'completed']
		ifFalse:[(listItem asJQuery find: 'span.taskDescription') removeClass: 'completed'].
!

updateTaskDescription: aTask

	model description: aTask description.
	view asJQuery show.
	view asJQuery effect: 'highlight'.
	
	(view asJQuery find: '.taskDescription') text: model description.
! !

