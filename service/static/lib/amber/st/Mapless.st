Smalltalk current createPackage: 'Mapless'!
Object subclass: #MaplessModel
	instanceVariableNames: 'data'
	package: 'Mapless'!
!MaplessModel commentStamp!
This is the model parent.
alright?

yes?!

!MaplessModel methodsFor: 'accessing'!

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

!MaplessModel methodsFor: 'actions'!

asJSONString

	self onAboutToJSON.

	^ JSON stringify: data
!

at: aKey ifAbsent: aBlock

	^ data at: aKey ifAbsent: aBlock
!

create

	^ self createDo:[nil]
!

createDo: aBlock

	jQuery ajax: #{
		'url' -> self path.
		'type' -> 'POST'.
		'cache' -> false.
		'data' -> self asJSONString.
		'success' -> [:x| self onAfterCreate: x done: aBlock].
		'fai' -> [:x| ModelCreateError signal: 'Could not create ', self class name,':  ', x responseText].
		'error' -> [:x| ModelCreateError signal: 'Could not create ', self class name,':  ', x responseText]}.

	^ self
!

delete

	self deleteDo:[nil]
!

deleteDo: aBlock

	jQuery ajax: #{ 
		'url' -> (self path, '?id=',self id asString).
		'type' -> 'DELETE'.
		'cache' -> false.
		'data' -> self asJSONString.
		'success' -> [:x| self onAfterDelete: x done: aBlock].
		'fail' -> [:x| self onDeleteFail: x].
		'error' -> [:x| self onDeleteFail: x]}
!

doesNotUnderstand: aMessage
	"The idea behind this DNU is to use the selector as setters or getter 
	delegating to data (aJsonObject)"
	
	| key part subModel isUndefined isObject obj keys |

	key := aMessage selector asSymbol.

	(self isUnary: key) ifTrue: [
		(data isKindOf: HashedCollection)
			ifTrue:[part := data at: key asString ifAbsent:[nil]]
			ifFalse:[part := data at: key asString].
		part ifNil:[^nil].
		isUndefined := self isUndefinedPart: part.
		isUndefined ifTrue:[^nil].
		isObject := self isObjectPart: part.
		isObject ifTrue:[
			"is part an empty js object? (would be nil for JSON eyes)"
			obj := self newJSObject.
			obj := JSObjectProxy on: obj.
			(obj keys: part) isEmpty ifTrue:[
				data at: key asString put: nil.
				^nil]].

		subModel := self get: 'modelClass' from: part.
		"If there is no modelClass in it, then is a direct value in the property"
		subModel ifNil:[ ^part].

		subModel := Smalltalk current at: subModel.
		subModel ifNil:[part inspect. self error: 'this should have a ',subModel,' modelClass no?'].
		subModel := subModel fromReified: part.
		data at: key asString put: subModel.
		^ subModel].
 
	^ ((self isKeyword: key) and: [
	(key asString occurrencesOf: ':') = 1])
		ifTrue: [key := key allButLast.
				data at: key asString put: aMessage arguments first]
		ifFalse: [super doesNotUnderstand: aMessage]
!

get: anAttribute from: aPart
	
	<return aPart[anAttribute]>
!

localDelete

	self class localDelete: self
!

localSave

	self class localSave: self
!

newJSObject
	<return Object>
!

refresh
	"Re-read this model's state."

	self refreshDo:[nil]
!

refreshDo: aBlock
	"Re-read this model's state."

	jQuery ajax: #{ 
		'url' -> (self path, '?id=',self id asString).
		'type' -> 'GET'.
		'cache' -> false.
		'success' -> [:x| self onAfterRefresh: x done: aBlock].
		'fail' -> [:x| self onRefeshFail: x].
		'error' -> [:x| self onRefreshFail: x]}
!

remoteSaveDo: aBlock

	jQuery ajax: #{ 
		'url' -> self path.
		'type' -> 'PUT'.
		'cache' -> false.
		'data' -> self asJSONString.
		'success' -> [:x| self onAfterSave: x done: aBlock].
		'fail' -> [:x| self onSaveFail: x].
		'error' -> [:x| self onSaveFail: x]}
!

save
	^ self saveDo:[nil]
!

saveDo: aBlock

	self localSave.
	self remoteSaveDo: aBlock
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

!MaplessModel methodsFor: 'initialization'!

initialize

	super initialize.

	data := HashedCollection new.
	self modelClass: self class name.
	self initializeInstanceVersion.
	self id: self class newUUID.
!

initializeCreatedOn

	self createdOn: Date now
!

initializeInstanceVersion	

	self v: 1
! !

!MaplessModel methodsFor: 'reactions'!

= anObject

	^ (anObject respondsTo: #id) and:[
	self id = anObject id]
!

onAboutToJSON
	"This model is about to be stringified in JSON.
	All inst var values that are expected to be model objects, need to be stringify friendly after this."
	| obj keys |

	obj := self newJSObject.
	obj := JSObjectProxy on: obj.
	keys := obj keys: data. 

	keys do:[:key| |value|
		value := data at: key.
		value := self perform: key asSymbol.

		(value isKindOf: MaplessModel) ifTrue:[
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

!MaplessModel methodsFor: 'testing'!

isKeyword: aSelector

	^ aSelector asString includes: ':'
!

isObjectPart: aPart
	<return typeof part=='object'>
!

isUnary: aSelector

	^ (aSelector asString includes: ':') not
!

isUndefinedPart: aPart
	<return aPart=='undefined'>
! !

!MaplessModel class methodsFor: 'accessing'!

atId: anId do: onDone
	"Answers the instance of this model found at anId (or nil)
	using th answer in the onDone callback"
 
	^ self read: anId do: onDone
! !

!MaplessModel class methodsFor: 'actions'!

create

	^ self createDo: [nil]
!

createDo: aBlock

	| newInstance |

	newInstance := self basicNew initialize.

	jQuery ajax: #{
		'url'-> self path.
		'type' -> 'POST'.
		'cache'-> false.
		'data' -> newInstance asJSONString.
		'success' -> [:x| newInstance onAfterCreate: x done: aBlock].
		'fail' -> [:x| ModelCreateError signal: 'Could not create ', self name,':  ', x responseText].
		'error' -> [:x| ModelCreateError signal: 'Could not create ', self name,':  ', x responseText]}.

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

fromJsonString: someJsonString
	"Answers a new instance of this model and returns it
	in the state dictated by someJsonString."

	| json modelClass |
	json := self reify: someJsonString.
	modelClass := json at: 'modelClass' ifAbsent:[nil].
	
	^ modelClass isNil
		ifTrue:[nil]
		ifFalse:[(Smalltalk current at: modelClass) new syncFrom: someJsonString]
!

fromReified: aReifiedJSON
	"Answers a new instance of this model and returns it
	in sync with aReifiedJSON."

	^ self new syncWith: aReifiedJSON
!

localDelete: aMaplessModel

	window localStorage removeItem: aMaplessModel id
!

localLoadAt: anId

	^ self fromJsonString: (window localStorage getItem: anId)
!

localSave: aMaplessModel

	window localStorage
		setItem: aMaplessModel id
		value: aMaplessModel asJSONString
!

manyFromJson: someJson

	^ (JSON parse: someJson) collect:[:each|
		 self fromReified: each ]
!

newUUID
	^ self uuidGenerator value
!

read: anId

	self read: anId do: [nil]
!

read: anId do: aBlock

	jQuery ajax: #{ 
		'url' -> (self path, '?id=',anId).
		'type'-> 'GET'.
		'cache'-> false.
		'success'-> [:x| self onAfterRead: x done: aBlock].
		'fail' -> [:x| ModelReadError signal: 'Could not read ', self name,':  ', x responseText].
		'error'-> [:x| ModelReadError signal: 'Could not read ', self name,':  ', x responseText]}
!

reify: someJson
	"Returns a simple javascript object with
	the attributes meant for the matching instance variable content of this model."

	^ JSON parse: someJson
!

uuidGenerator
	"Taken from:
	http://stackoverflow.com/questions/105034/how-to-create-a-guid-uuid-in-javascript"

<return function guid() {
    function _p8(s) {
        var p = (Math.random().toString(16)+"000000000").substr(2,8);
        return s ? "-" + p.substr(0,4) + "-" + p.substr(4,4) : p ;
    }
    return _p8() + _p8(true) + _p8(true) + _p8();
}>
! !

!MaplessModel class methodsFor: 'reactions'!

onAfterRead: someJson done: aBlock

	| reified |
	
	reified := self fromJson: someJson.
	aBlock value: reified.
	^ reified
! !

