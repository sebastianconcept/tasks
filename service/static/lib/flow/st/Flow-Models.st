Smalltalk current createPackage: 'Flow-Models' properties: #{}!
Object subclass: #Model
	instanceVariableNames: 'announcer'
	package: 'Flow-Models'!
!Model commentStamp!
Models are meant to be used as local-only (at the DOM) objects that should be transident in nature (not-persistent)!

!Model methodsFor: 'accessing'!

announcer

	^ announcer ifNil:[self initializeAnnouncer]
! !

!Model methodsFor: 'actions'!

announce: anAnnouncement

	self announcer announce: anAnnouncement
!

on: anAnnouncementClass do: aReactionBlock

	^ self announcer on: anAnnouncementClass do: aReactionBlock
! !

!Model methodsFor: 'initialization'!

initializeAnnouncer

	^ announcer := Announcer new
! !

!Model class methodsFor: 'accessing'!

basePath
	"Answers the base path for the application's API.
	Subclasses should override if appropriate."

	"By default we use the AmberMP's default"
	^ 'api'
!

jsonIgnoreInstanceVariables

	^ super jsonIgnoreInstanceVariables, #('announcer')
!

modelPath
	"Answers the path for posting instances of this model.
	Subclasses should override if appropriate."

	^ self name
!

path
	^ self basePath,'/', self modelPath
! !

!Model class methodsFor: 'initialization'!

initialize

	"Set amber to commit using the Flow's API"
	Package defaultCommitPathSt: '/flow/commit'.
	Package defaultCommitPathJs: '/flow/commit'
! !

Model subclass: #JsonModel
	instanceVariableNames: 'data'
	package: 'Flow-Models'!

Model subclass: #PersistentModel
	instanceVariableNames: 'data'
	package: 'Flow-Models'!

!PersistentModel methodsFor: 'accessing'!

at: aKey

	^ data at: aKey
!

at: aKey put: anObject

	data at: aKey put: anObject.

	^ anObject
!

createdOn 

	| selector expects object|

	selector := #createdOn.
	expects := Date.

	object := self at: selector asString.
	object ifNil:[^nil].

	(object isKindOf: expects) ifTrue:[^object].

	^ self at: selector asString put: (self dateAndTimeAt: selector).
!

createdOn: aDate

	self at: #createdOn put: aDate
!

data

	^ data
!

dateAndTimeAt: aSelector

	^ Date fromString: (self at: aSelector)
!

id

	^ data at: #id
!

id: aString

	data at: #id put: aString
!

path 

	^ self class path
!

subModelAt: aSelector
	"Answers the reified submodel (instantiating if necessary)."
	
	| subModelData modelClass |

	subModelData := data at: aSelector.
	subModelData ifNil:[^nil].
	
	modelClass := subModelData at: 'modelClass'.
	modelClass ifNil:[^nil].
	modelClass := Smalltalk current at: modelClass.

	modelClass ifNil:[^ModelMetadataError signal: 'Cannot find ',aSelector asString,'''s class for this metadata'].
	
	^ modelClass fromReified: subModelData
!

url

	^ self path,'/',self id asString
! !

!PersistentModel methodsFor: 'actions'!

asJSONString

	self onAboutToJSON.

	^ JSON stringify: data
!

create

	^ self createDo:[nil]
!

createDo: aBlock

	jQuery ajax: #{
		url: self path.
		type: 'POST'.
		cache: false.
		data: self asJSONString.
		success: [:x| self onAfterCreate: x done: aBlock].
		fail: [:x| ModelCreateError signal: 'Could not create ', self class name,':  ', x responseText].
		error: [:x| ModelCreateError signal: 'Could not create ', self class name,':  ', x responseText]}.

	^ self
!

delete

	self deleteDo:[nil]
!

deleteDo: aBlock

	jQuery ajax: #{ 
		url: (self path, '?id=',self id asString).
		type: 'DELETE'.
		cache: false.
		data: self asJSONString.
		success: [:x| self onAfterDelete: x done: aBlock].
		fail: [:x| self onDeleteFail: x].
		error: [:x| self onDeleteFail: x]}
!

doesNotUnderstand: aMessage
	"The idea behind this DNU is to use the selector as setters or getter 
	delegating to data (aJsonObject)"
	
	| key part subModel isUndefined isObject obj keys |

	key := aMessage selector asSymbol.
	
	key isUnary ifTrue: [
		(data isKindOf: HashedCollection)
			ifTrue:[part := data at: key asString ifAbsent:[nil]]
			ifFalse:[part := data at: key asString].
		part ifNil:[^nil].
		isUndefined := <part=='undefined'>.
		isUndefined ifTrue:[^nil].
		isObject := <typeof part=='object'>.
		isObject ifTrue:[
			"is part an empty js object? (would be nil for JSON eyes)"
			obj := <Object>.
			obj := JSObjectProxy on: obj.
			(obj keys: part) isEmpty ifTrue:[
				data at: key asString put: nil.
				^nil]].

		subModel := <part['modelClass']>.
		"If there is no modelClass in it, then is a direct value in the property"
		subModel ifNil:[ ^part].

		subModel := Smalltalk current at: subModel.
		subModel ifNil:[part inspect. self error: 'this should have a ',subModel,' modelClass no?'].
		subModel := subModel fromReified: part.
		data at: key asString put: subModel.
		^ subModel].

	^ (key isKeyword and: [
	(key asString occurrencesOf: ':') = 1])
		ifTrue: [key := key allButLast.
				data at: key asString put: aMessage arguments first]
		ifFalse: [super doesNotUnderstand: aMessage]
!

refresh
	"Re-read this model's state."

	self refreshDo:[nil]
!

refreshDo: aBlock
	"Re-read this model's state."

	jQuery ajax: #{ 
		url: (self path, '?id=',self id asString).
		type: 'GET'.
		cache: false.
		success: [:x| self onAfterRefresh: x done: aBlock].
		fail: [:x| self onRefeshFail: x].
		error: [:x| self onRefreshFail: x]}
!

save
	^ self saveDo:[nil]
!

saveDo: aBlock

	jQuery ajax: #{ 
		url: self path.
		type: 'PUT'.
		cache: false.
		data: self asJSONString.
		success: [:x| self onAfterSave: x done: aBlock].
		fail: [:x| self onSaveFail: x].
		error: [:x| self onSaveFail: x]}
!

syncFrom: someJson

	^ self syncWith: (self class reify: someJson)
!

syncWith: aReifiedJSON
	"Sync the current values in this model 
	with the ones coming in aReifiedJSON."

	aReifiedJSON ifNil:[^ nil].

	((aReifiedJSON at: 'modelClass') isNil or:[
	(aReifiedJSON at: 'modelClass') ~= self class name]) ifTrue:[
		self error: 'this JSON does not fit here'].

	data := aReifiedJSON
! !

!PersistentModel methodsFor: 'initialization'!

initialize

	super initialize.

	data := HashedCollection new.
	self modelClass: self class name.
! !

!PersistentModel methodsFor: 'reactions'!

= anObject

	^ (anObject respondsTo: #id) and:[
	self id = anObject id]
!

onAboutToJSON
	"This model is about to be stringified in JSON.
	All inst var values that are expected to be model objects, need to be stringify friendly after this."
	| obj keys |

	obj := <Object>.
	obj := JSObjectProxy on: obj.
	keys := obj keys: data. 

	keys do:[:key| |value|
		value := data at: key.
		value := self perform: key asSymbol.

		(value isKindOf: PersistentModel) ifTrue:[
			value onAboutToJSON.
			value := value data].
		data at: key put: value].
!

onAfterCreate: x done: aBlock

	data := self class reify: x.

	self announce: (ModelCreated for: self).
	aBlock value: self
!

onAfterDelete: x done: aBlock
	
	self announce: (ModelDeleted for: self).

	aBlock value: self
!

onAfterRefresh: x done: aBlock

	self syncWith: (self class reify: x).
	self announce: (ModelRefreshed for: self).
	aBlock value: self
!

onAfterSave: x done: aBlock
	
	self announce: (ModelSaved for: self).

	aBlock value: self
!

onDeleteFail: x

	ModelUpdateError signal: 'Could not delete ', self class name
!

onRefreshFail: x

	ModelRefreshError signal: 'Could not refresh ', self class name
!

onSaveFail: x

	ModelUpdateError signal: 'Could not save ', self class name
! !

!PersistentModel class methodsFor: 'accessing'!

atId: anId do: onDone
	"Answers the instance of this model found at anId (or nil)
	using th answer in the onDone callback"
 
	^ self read: anId do: onDone
! !

!PersistentModel class methodsFor: 'actions'!

create

	^ self createDo: [nil]
!

createDo: aBlock

	| newInstance |

	newInstance := self basicNew initialize.

	jQuery ajax: #{
		url: self path.
		type: 'POST'.
		cache: false.
		data: newInstance asJSONString.
		success: [:x| newInstance onAfterCreate: x done: aBlock].
		fail: [:x| ModelCreateError signal: 'Could not create ', self name,':  ', x responseText].
		error: [:x| ModelCreateError signal: 'Could not create ', self name,':  ', x responseText]}.

	^ newInstance
!

createdOnFrom: aReifiedJSON

	^ Date fromString: aReifiedJSON
!

fromJson: someJson
	"Answers a new instance of this model and returns it
	in the state dictated by someJson."

	^ self new syncFrom: someJson
!

fromReified: aReifiedJSON
	"Answers a new instance of this model and returns it
	in sync with aReifiedJSON."

	^ self new syncWith: aReifiedJSON
!

manyFromJson: someJson

	^ (JSON parse: someJson) collect:[:each|
		 self fromReified: each ]
!

read: anId

	self read: anId do: [nil]
!

read: anId do: aBlock

	jQuery ajax: #{ 
		url: (self path, '?id=',anId).
		type: 'GET'.
		cache: false.
		success: [:x| self onAfterRead: x done: aBlock].
		fail: [:x| ModelReadError signal: 'Could not read ', self name,':  ', x responseText].
		error: [:x| ModelReadError signal: 'Could not read ', self name,':  ', x responseText]}
!

reify: someJson
	"Returns a simple javascript object with
	the attributes meant for the matching instance variable content of this model."

	^ JSON parse: someJson
! !

!PersistentModel class methodsFor: 'reactions'!

onAfterRead: someJson done: aBlock

	| reified |
	
	reified := self fromJson: someJson.
	aBlock value: reified.
	^ reified
! !

