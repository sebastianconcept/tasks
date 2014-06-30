smalltalk.addPackage('Mapless');
smalltalk.addClass('MaplessModel', smalltalk.Object, ['data'], 'Mapless');
smalltalk.MaplessModel.comment="This is the model parent.\x0aalright?\x0a\x0ayes?"
smalltalk.addMethod(
smalltalk.method({
selector: "=",
category: 'reactions',
fn: function (anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(anObject)._respondsTo_(smalltalk.symbolFor("id")))._and_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(self)._id()).__eq(_st(anObject)._id());
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"=",{anObject:anObject},smalltalk.MaplessModel)})},
args: ["anObject"],
source: "= anObject\x0a\x0a\x09^ (anObject respondsTo: #id) and:[\x0a\x09self id = anObject id]\x0a",
messageSends: ["and:", "=", "id", "respondsTo:"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "asJSONString",
category: 'actions',
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(self)._onAboutToJSON();
$1=_st((smalltalk.JSON || JSON))._stringify_(self["@data"]);
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJSONString",{},smalltalk.MaplessModel)})},
args: [],
source: "asJSONString\x0a\x0a\x09self onAboutToJSON.\x0a\x0a\x09^ JSON stringify: data",
messageSends: ["onAboutToJSON", "stringify:"],
referencedClasses: ["JSON"]
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "at:",
category: 'accessing',
fn: function (aKey){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@data"])._at_(aKey);
return $1;
}, function($ctx1) {$ctx1.fill(self,"at:",{aKey:aKey},smalltalk.MaplessModel)})},
args: ["aKey"],
source: "at: aKey\x0a\x0a\x09^ data at: aKey",
messageSends: ["at:"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "at:ifAbsent:",
category: 'actions',
fn: function (aKey,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@data"])._at_ifAbsent_(aKey,aBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"at:ifAbsent:",{aKey:aKey,aBlock:aBlock},smalltalk.MaplessModel)})},
args: ["aKey", "aBlock"],
source: "at: aKey ifAbsent: aBlock\x0a\x0a\x09^ data at: aKey ifAbsent: aBlock",
messageSends: ["at:ifAbsent:"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "at:put:",
category: 'accessing',
fn: function (aKey,anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(self["@data"])._at_put_(aKey,anObject);
$1=anObject;
return $1;
}, function($ctx1) {$ctx1.fill(self,"at:put:",{aKey:aKey,anObject:anObject},smalltalk.MaplessModel)})},
args: ["aKey", "anObject"],
source: "at: aKey put: anObject\x0a\x0a\x09data at: aKey put: anObject.\x0a\x0a\x09^ anObject",
messageSends: ["at:put:"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "create",
category: 'actions',
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._createDo_((function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"create",{},smalltalk.MaplessModel)})},
args: [],
source: "create\x0a\x0a\x09^ self createDo:[nil]",
messageSends: ["createDo:"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "createDo:",
category: 'actions',
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(self)._path()),_st("type").__minus_gt("POST"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(self)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfterCreate_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fai").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st((smalltalk.ModelCreateError || ModelCreateError))._signal_(_st(_st(_st("Could not create ").__comma(_st(_st(self)._class())._name())).__comma(":  ")).__comma(_st(x)._responseText()));
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st((smalltalk.ModelCreateError || ModelCreateError))._signal_(_st(_st(_st("Could not create ").__comma(_st(_st(self)._class())._name())).__comma(":  ")).__comma(_st(x)._responseText()));
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
$1=self;
return $1;
}, function($ctx1) {$ctx1.fill(self,"createDo:",{aBlock:aBlock},smalltalk.MaplessModel)})},
args: ["aBlock"],
source: "createDo: aBlock\x0a\x0a\x09jQuery ajax: #{\x0a\x09\x09'url' -> self path.\x0a\x09\x09'type' -> 'POST'.\x0a\x09\x09'cache' -> false.\x0a\x09\x09'data' -> self asJSONString.\x0a\x09\x09'success' -> [:x| self onAfterCreate: x done: aBlock].\x0a\x09\x09'fai' -> [:x| ModelCreateError signal: 'Could not create ', self class name,':  ', x responseText].\x0a\x09\x09'error' -> [:x| ModelCreateError signal: 'Could not create ', self class name,':  ', x responseText]}.\x0a\x0a\x09^ self\x0a",
messageSends: ["ajax:", "->", "path", "asJSONString", "onAfterCreate:done:", "signal:", ",", "responseText", "name", "class"],
referencedClasses: ["ModelCreateError"]
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "createdOn",
category: 'accessing',
fn: function (){
var self=this;
var selector,expects,object;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4;
selector=smalltalk.symbolFor("createdOn");
expects=(smalltalk.Date || Date);
object=_st(self)._at_(_st(selector)._asString());
$1=object;
if(($receiver = $1) == nil || $receiver == undefined){
return nil;
} else {
$1;
};
$2=_st(object)._isKindOf_(expects);
if(smalltalk.assert($2)){
$3=object;
return $3;
};
$4=_st(self)._at_put_(_st(selector)._asString(),_st(self)._dateAndTimeAt_(selector));
return $4;
}, function($ctx1) {$ctx1.fill(self,"createdOn",{selector:selector,expects:expects,object:object},smalltalk.MaplessModel)})},
args: [],
source: "createdOn \x0a\x0a\x09| selector expects object|\x0a\x0a\x09selector := #createdOn.\x0a\x09expects := Date.\x0a\x0a\x09object := self at: selector asString.\x0a\x09object ifNil:[^nil].\x0a\x0a\x09(object isKindOf: expects) ifTrue:[^object].\x0a\x0a\x09^ self at: selector asString put: (self dateAndTimeAt: selector).",
messageSends: ["at:", "asString", "ifNil:", "ifTrue:", "isKindOf:", "at:put:", "dateAndTimeAt:"],
referencedClasses: ["Date"]
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "createdOn:",
category: 'accessing',
fn: function (aDate){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_(smalltalk.symbolFor("createdOn"),aDate);
return self}, function($ctx1) {$ctx1.fill(self,"createdOn:",{aDate:aDate},smalltalk.MaplessModel)})},
args: ["aDate"],
source: "createdOn: aDate\x0a\x0a\x09self at: #createdOn put: aDate",
messageSends: ["at:put:"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "data",
category: 'accessing',
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@data"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"data",{},smalltalk.MaplessModel)})},
args: [],
source: "data\x0a\x0a\x09^ data",
messageSends: [],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "dateAndTimeAt:",
category: 'accessing',
fn: function (aSelector){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st((smalltalk.Date || Date))._fromString_(_st(self)._at_(aSelector));
return $1;
}, function($ctx1) {$ctx1.fill(self,"dateAndTimeAt:",{aSelector:aSelector},smalltalk.MaplessModel)})},
args: ["aSelector"],
source: "dateAndTimeAt: aSelector\x0a\x0a\x09^ Date fromString: (self at: aSelector)",
messageSends: ["fromString:", "at:"],
referencedClasses: ["Date"]
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "delete",
category: 'actions',
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._deleteDo_((function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"delete",{},smalltalk.MaplessModel)})},
args: [],
source: "delete\x0a\x0a\x09self deleteDo:[nil]\x0a\x0a",
messageSends: ["deleteDo:"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "deleteDo:",
category: 'actions',
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(_st(self)._path()).__comma("?id=")).__comma(_st(_st(self)._id())._asString())),_st("type").__minus_gt("DELETE"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(self)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfterDelete_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onDeleteFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onDeleteFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"deleteDo:",{aBlock:aBlock},smalltalk.MaplessModel)})},
args: ["aBlock"],
source: "deleteDo: aBlock\x0a\x0a\x09jQuery ajax: #{ \x0a\x09\x09'url' -> (self path, '?id=',self id asString).\x0a\x09\x09'type' -> 'DELETE'.\x0a\x09\x09'cache' -> false.\x0a\x09\x09'data' -> self asJSONString.\x0a\x09\x09'success' -> [:x| self onAfterDelete: x done: aBlock].\x0a\x09\x09'fail' -> [:x| self onDeleteFail: x].\x0a\x09\x09'error' -> [:x| self onDeleteFail: x]}  \x0a",
messageSends: ["ajax:", "->", ",", "asString", "id", "path", "asJSONString", "onAfterDelete:done:", "onDeleteFail:"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "doesNotUnderstand:",
category: 'actions',
fn: function (aMessage){
var self=this;
var key,part,subModel,isUndefined,isObject,obj,keys;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$12,$11;
key=_st(_st(aMessage)._selector())._asSymbol();
$1=_st(self)._isUnary_(key);
if(smalltalk.assert($1)){
$2=_st(self["@data"])._isKindOf_((smalltalk.HashedCollection || HashedCollection));
if(smalltalk.assert($2)){
part=_st(self["@data"])._at_ifAbsent_(_st(key)._asString(),(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
part;
} else {
part=_st(self["@data"])._at_(_st(key)._asString());
part;
};
$3=part;
if(($receiver = $3) == nil || $receiver == undefined){
return nil;
} else {
$3;
};
isUndefined=_st(self)._isUndefinedPart_(part);
isUndefined;
$4=isUndefined;
if(smalltalk.assert($4)){
return nil;
};
isObject=_st(self)._isObjectPart_(part);
isObject;
$5=isObject;
if(smalltalk.assert($5)){
obj=_st(self)._newJSObject();
obj;
obj=_st((smalltalk.JSObjectProxy || JSObjectProxy))._on_(obj);
obj;
$6=_st(_st(obj)._keys_(part))._isEmpty();
if(smalltalk.assert($6)){
_st(self["@data"])._at_put_(_st(key)._asString(),nil);
return nil;
};
};
subModel=_st(self)._get_from_("modelClass",part);
subModel;
$7=subModel;
if(($receiver = $7) == nil || $receiver == undefined){
$8=part;
return $8;
} else {
$7;
};
subModel=_st(_st((smalltalk.Smalltalk || Smalltalk))._current())._at_(subModel);
subModel;
$9=subModel;
if(($receiver = $9) == nil || $receiver == undefined){
_st(part)._inspect();
_st(self)._error_(_st(_st("this should have a ").__comma(subModel)).__comma(" modelClass no?"));
} else {
$9;
};
subModel=_st(subModel)._fromReified_(part);
subModel;
_st(self["@data"])._at_put_(_st(key)._asString(),subModel);
$10=subModel;
return $10;
};
$12=_st(_st(self)._isKeyword_(key))._and_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(_st(key)._asString())._occurrencesOf_(":")).__eq((1));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
if(smalltalk.assert($12)){
key=_st(key)._allButLast();
key;
$11=_st(self["@data"])._at_put_(_st(key)._asString(),_st(_st(aMessage)._arguments())._first());
} else {
$11=smalltalk.Object.fn.prototype._doesNotUnderstand_.apply(_st(self), [aMessage]);
};
return $11;
}, function($ctx1) {$ctx1.fill(self,"doesNotUnderstand:",{aMessage:aMessage,key:key,part:part,subModel:subModel,isUndefined:isUndefined,isObject:isObject,obj:obj,keys:keys},smalltalk.MaplessModel)})},
args: ["aMessage"],
source: "doesNotUnderstand: aMessage\x0a\x09\x22The idea behind this DNU is to use the selector as setters or getter \x0a\x09delegating to data (aJsonObject)\x22\x0a\x09\x0a\x09| key part subModel isUndefined isObject obj keys |\x0a\x0a\x09key := aMessage selector asSymbol.\x0a\x0a\x09(self isUnary: key) ifTrue: [\x0a\x09\x09(data isKindOf: HashedCollection)\x0a\x09\x09\x09ifTrue:[part := data at: key asString ifAbsent:[nil]]\x0a\x09\x09\x09ifFalse:[part := data at: key asString].\x0a\x09\x09part ifNil:[^nil].\x0a\x09\x09isUndefined := self isUndefinedPart: part.\x0a\x09\x09isUndefined ifTrue:[^nil].\x0a\x09\x09isObject := self isObjectPart: part.\x0a\x09\x09isObject ifTrue:[\x0a\x09\x09\x09\x22is part an empty js object? (would be nil for JSON eyes)\x22\x0a\x09\x09\x09obj := self newJSObject.\x0a\x09\x09\x09obj := JSObjectProxy on: obj.\x0a\x09\x09\x09(obj keys: part) isEmpty ifTrue:[\x0a\x09\x09\x09\x09data at: key asString put: nil.\x0a\x09\x09\x09\x09^nil]].\x0a\x0a\x09\x09subModel := self get: 'modelClass' from: part.\x0a\x09\x09\x22If there is no modelClass in it, then is a direct value in the property\x22\x0a\x09\x09subModel ifNil:[ ^part].\x0a\x0a\x09\x09subModel := Smalltalk current at: subModel.\x0a\x09\x09subModel ifNil:[part inspect. self error: 'this should have a ',subModel,' modelClass no?'].\x0a\x09\x09subModel := subModel fromReified: part.\x0a\x09\x09data at: key asString put: subModel.\x0a\x09\x09^ subModel].\x0a \x0a\x09^ ((self isKeyword: key) and: [\x0a\x09(key asString occurrencesOf: ':') = 1])\x0a\x09\x09ifTrue: [key := key allButLast.\x0a\x09\x09\x09\x09data at: key asString put: aMessage arguments first]\x0a\x09\x09ifFalse: [super doesNotUnderstand: aMessage]",
messageSends: ["asSymbol", "selector", "ifTrue:", "ifTrue:ifFalse:", "at:ifAbsent:", "asString", "at:", "isKindOf:", "ifNil:", "isUndefinedPart:", "isObjectPart:", "newJSObject", "on:", "at:put:", "isEmpty", "keys:", "get:from:", "current", "inspect", "error:", ",", "fromReified:", "isUnary:", "allButLast", "first", "arguments", "doesNotUnderstand:", "and:", "=", "occurrencesOf:", "isKeyword:"],
referencedClasses: ["HashedCollection", "JSObjectProxy", "Smalltalk"]
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "get:from:",
category: 'actions',
fn: function (anAttribute,aPart){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return aPart[anAttribute];
return self}, function($ctx1) {$ctx1.fill(self,"get:from:",{anAttribute:anAttribute,aPart:aPart},smalltalk.MaplessModel)})},
args: ["anAttribute", "aPart"],
source: "get: anAttribute from: aPart\x0a\x09\x0a\x09<return aPart[anAttribute]>\x0a",
messageSends: [],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "id",
category: 'accessing',
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@data"])._at_(smalltalk.symbolFor("id"));
return $1;
}, function($ctx1) {$ctx1.fill(self,"id",{},smalltalk.MaplessModel)})},
args: [],
source: "id\x0a\x0a\x09^ data at: #id ",
messageSends: ["at:"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "id:",
category: 'accessing',
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self["@data"])._at_put_(smalltalk.symbolFor("id"),aString);
return self}, function($ctx1) {$ctx1.fill(self,"id:",{aString:aString},smalltalk.MaplessModel)})},
args: ["aString"],
source: "id: aString\x0a\x0a\x09data at: #id put: aString",
messageSends: ["at:put:"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
category: 'initialization',
fn: function (){
var self=this;
function $HashedCollection(){return smalltalk.HashedCollection||(typeof HashedCollection=="undefined"?nil:HashedCollection)}
return smalltalk.withContext(function($ctx1) { 
smalltalk.Object.fn.prototype._initialize.apply(_st(self), []);
self["@data"]=_st($HashedCollection())._new();
_st(self)._modelClass_(_st(_st(self)._class())._name());
_st(self)._initializeInstanceVersion();
_st(self)._id_(_st(_st(self)._class())._newUUID());
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.MaplessModel)})},
args: [],
source: "initialize\x0a\x0a\x09super initialize.\x0a\x0a\x09data := HashedCollection new.\x0a\x09self modelClass: self class name.\x0a\x09self initializeInstanceVersion.\x0a\x09self id: self class newUUID.",
messageSends: ["initialize", "new", "modelClass:", "name", "class", "initializeInstanceVersion", "id:", "newUUID"],
referencedClasses: ["HashedCollection"]
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeCreatedOn",
category: 'initialization',
fn: function (){
var self=this;
function $Date(){return smalltalk.Date||(typeof Date=="undefined"?nil:Date)}
return smalltalk.withContext(function($ctx1) { 
_st(self)._createdOn_(_st($Date())._now());
return self}, function($ctx1) {$ctx1.fill(self,"initializeCreatedOn",{},smalltalk.MaplessModel)})},
args: [],
source: "initializeCreatedOn\x0a\x0a\x09self createdOn: Date now",
messageSends: ["createdOn:", "now"],
referencedClasses: ["Date"]
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeInstanceVersion",
category: 'initialization',
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._v_((1));
return self}, function($ctx1) {$ctx1.fill(self,"initializeInstanceVersion",{},smalltalk.MaplessModel)})},
args: [],
source: "initializeInstanceVersion\x09\x0a\x0a\x09self v: 1",
messageSends: ["v:"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "isKeyword:",
category: 'testing',
fn: function (aSelector){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(aSelector)._asString())._includes_(":");
return $1;
}, function($ctx1) {$ctx1.fill(self,"isKeyword:",{aSelector:aSelector},smalltalk.MaplessModel)})},
args: ["aSelector"],
source: "isKeyword: aSelector\x0a\x0a\x09^ aSelector asString includes: ':'",
messageSends: ["includes:", "asString"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "isObjectPart:",
category: 'testing',
fn: function (aPart){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return typeof part=='object';
return self}, function($ctx1) {$ctx1.fill(self,"isObjectPart:",{aPart:aPart},smalltalk.MaplessModel)})},
args: ["aPart"],
source: "isObjectPart: aPart\x0a\x09<return typeof part=='object'>",
messageSends: [],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "isUnary:",
category: 'testing',
fn: function (aSelector){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(aSelector)._asString())._includes_(":"))._not();
return $1;
}, function($ctx1) {$ctx1.fill(self,"isUnary:",{aSelector:aSelector},smalltalk.MaplessModel)})},
args: ["aSelector"],
source: "isUnary: aSelector\x0a\x0a\x09^ (aSelector asString includes: ':') not",
messageSends: ["not", "includes:", "asString"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "isUndefinedPart:",
category: 'testing',
fn: function (aPart){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return aPart=='undefined';
return self}, function($ctx1) {$ctx1.fill(self,"isUndefinedPart:",{aPart:aPart},smalltalk.MaplessModel)})},
args: ["aPart"],
source: "isUndefinedPart: aPart\x0a\x09<return aPart=='undefined'>\x0a",
messageSends: [],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "localDelete",
category: 'actions',
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._class())._localDelete_(self);
return self}, function($ctx1) {$ctx1.fill(self,"localDelete",{},smalltalk.MaplessModel)})},
args: [],
source: "localDelete\x0a\x0a\x09self class localDelete: self ",
messageSends: ["localDelete:", "class"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave",
category: 'actions',
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._class())._localSave_(self);
return self}, function($ctx1) {$ctx1.fill(self,"localSave",{},smalltalk.MaplessModel)})},
args: [],
source: "localSave\x0a\x0a\x09self class localSave: self ",
messageSends: ["localSave:", "class"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "newJSObject",
category: 'actions',
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return Object;
return self}, function($ctx1) {$ctx1.fill(self,"newJSObject",{},smalltalk.MaplessModel)})},
args: [],
source: "newJSObject\x0a\x09<return Object>\x0a",
messageSends: [],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onAboutToJSON",
category: 'reactions',
fn: function (){
var self=this;
var obj,keys;
return smalltalk.withContext(function($ctx1) { 
var $1;
obj=_st(self)._newJSObject();
obj=_st((smalltalk.JSObjectProxy || JSObjectProxy))._on_(obj);
keys=_st(obj)._keys_(self["@data"]);
_st(keys)._do_((function(key){
var value;
return smalltalk.withContext(function($ctx2) {
value=_st(self["@data"])._at_(key);
value;
value=_st(self)._perform_(_st(key)._asSymbol());
value;
$1=_st(value)._isKindOf_((smalltalk.MaplessModel || MaplessModel));
if(smalltalk.assert($1)){
_st(value)._onAboutToJSON();
value=_st(value)._data();
value;
};
return _st(self["@data"])._at_put_(key,value);
}, function($ctx2) {$ctx2.fillBlock({key:key,value:value},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"onAboutToJSON",{obj:obj,keys:keys},smalltalk.MaplessModel)})},
args: [],
source: "onAboutToJSON\x0a\x09\x22This model is about to be stringified in JSON.\x0a\x09All inst var values that are expected to be model objects, need to be stringify friendly after this.\x22\x0a\x09| obj keys |\x0a\x0a\x09obj := self newJSObject.\x0a\x09obj := JSObjectProxy on: obj.\x0a\x09keys := obj keys: data. \x0a\x0a\x09keys do:[:key| |value|\x0a\x09\x09value := data at: key.\x0a\x09\x09value := self perform: key asSymbol.\x0a\x0a\x09\x09(value isKindOf: MaplessModel) ifTrue:[\x0a\x09\x09\x09value onAboutToJSON.\x0a\x09\x09\x09value := value data].\x0a\x09\x09data at: key put: value].",
messageSends: ["newJSObject", "on:", "keys:", "do:", "at:", "perform:", "asSymbol", "ifTrue:", "onAboutToJSON", "data", "isKindOf:", "at:put:"],
referencedClasses: ["JSObjectProxy", "MaplessModel"]
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onAfterCreate:done:",
category: 'reactions',
fn: function (x,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@data"]=_st(_st(self)._class())._reify_(x);
_st(self)._announce_(_st((smalltalk.ModelCreated || ModelCreated))._for_(self));
_st(aBlock)._value_(self);
return self}, function($ctx1) {$ctx1.fill(self,"onAfterCreate:done:",{x:x,aBlock:aBlock},smalltalk.MaplessModel)})},
args: ["x", "aBlock"],
source: "onAfterCreate: x done: aBlock\x0a\x0a\x09data := self class reify: x.\x0a\x0a\x09self announce: (ModelCreated for: self).\x0a\x09aBlock value: self",
messageSends: ["reify:", "class", "announce:", "for:", "value:"],
referencedClasses: ["ModelCreated"]
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onAfterDelete:done:",
category: 'reactions',
fn: function (x,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._announce_(_st((smalltalk.ModelDeleted || ModelDeleted))._for_(self));
_st(aBlock)._value_(self);
return self}, function($ctx1) {$ctx1.fill(self,"onAfterDelete:done:",{x:x,aBlock:aBlock},smalltalk.MaplessModel)})},
args: ["x", "aBlock"],
source: "onAfterDelete: x done: aBlock\x0a\x09\x0a\x09self announce: (ModelDeleted for: self).\x0a\x0a\x09aBlock value: self\x0a",
messageSends: ["announce:", "for:", "value:"],
referencedClasses: ["ModelDeleted"]
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onAfterRefresh:done:",
category: 'reactions',
fn: function (x,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._syncWith_(_st(_st(self)._class())._reify_(x));
_st(self)._announce_(_st((smalltalk.ModelRefreshed || ModelRefreshed))._for_(self));
_st(aBlock)._value_(self);
return self}, function($ctx1) {$ctx1.fill(self,"onAfterRefresh:done:",{x:x,aBlock:aBlock},smalltalk.MaplessModel)})},
args: ["x", "aBlock"],
source: "onAfterRefresh: x done: aBlock\x0a\x0a\x09self syncWith: (self class reify: x).\x0a\x09self announce: (ModelRefreshed for: self).\x0a\x09aBlock value: self\x0a",
messageSends: ["syncWith:", "reify:", "class", "announce:", "for:", "value:"],
referencedClasses: ["ModelRefreshed"]
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onAfterSave:done:",
category: 'reactions',
fn: function (x,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._announce_(_st((smalltalk.ModelSaved || ModelSaved))._for_(self));
_st(aBlock)._value_(self);
return self}, function($ctx1) {$ctx1.fill(self,"onAfterSave:done:",{x:x,aBlock:aBlock},smalltalk.MaplessModel)})},
args: ["x", "aBlock"],
source: "onAfterSave: x done: aBlock\x0a\x09\x0a\x09self announce: (ModelSaved for: self).\x0a\x0a\x09aBlock value: self",
messageSends: ["announce:", "for:", "value:"],
referencedClasses: ["ModelSaved"]
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onDeleteFail:",
category: 'reactions',
fn: function (x){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st((smalltalk.ModelUpdateError || ModelUpdateError))._signal_(_st("Could not delete ").__comma(_st(_st(self)._class())._name()));
return self}, function($ctx1) {$ctx1.fill(self,"onDeleteFail:",{x:x},smalltalk.MaplessModel)})},
args: ["x"],
source: "onDeleteFail: x\x0a\x0a\x09ModelUpdateError signal: 'Could not delete ', self class name\x09\x0a",
messageSends: ["signal:", ",", "name", "class"],
referencedClasses: ["ModelUpdateError"]
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onRefreshFail:",
category: 'reactions',
fn: function (x){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st((smalltalk.ModelRefreshError || ModelRefreshError))._signal_(_st("Could not refresh ").__comma(_st(_st(self)._class())._name()));
return self}, function($ctx1) {$ctx1.fill(self,"onRefreshFail:",{x:x},smalltalk.MaplessModel)})},
args: ["x"],
source: "onRefreshFail: x\x0a\x0a\x09ModelRefreshError signal: 'Could not refresh ', self class name\x09\x0a",
messageSends: ["signal:", ",", "name", "class"],
referencedClasses: ["ModelRefreshError"]
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onSaveFail:",
category: 'reactions',
fn: function (x){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st((smalltalk.ModelUpdateError || ModelUpdateError))._signal_(_st("Could not save ").__comma(_st(_st(self)._class())._name()));
return self}, function($ctx1) {$ctx1.fill(self,"onSaveFail:",{x:x},smalltalk.MaplessModel)})},
args: ["x"],
source: "onSaveFail: x\x0a\x0a\x09ModelUpdateError signal: 'Could not save ', self class name\x09\x0a",
messageSends: ["signal:", ",", "name", "class"],
referencedClasses: ["ModelUpdateError"]
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "path",
category: 'accessing',
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._class())._path();
return $1;
}, function($ctx1) {$ctx1.fill(self,"path",{},smalltalk.MaplessModel)})},
args: [],
source: "path \x0a\x0a\x09^ self class path",
messageSends: ["path", "class"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "refresh",
category: 'actions',
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._refreshDo_((function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"refresh",{},smalltalk.MaplessModel)})},
args: [],
source: "refresh\x0a\x09\x22Re-read this model's state.\x22\x0a\x0a\x09self refreshDo:[nil]",
messageSends: ["refreshDo:"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "refreshDo:",
category: 'actions',
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(_st(self)._path()).__comma("?id=")).__comma(_st(_st(self)._id())._asString())),_st("type").__minus_gt("GET"),_st("cache").__minus_gt(false),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfterRefresh_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onRefeshFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onRefreshFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"refreshDo:",{aBlock:aBlock},smalltalk.MaplessModel)})},
args: ["aBlock"],
source: "refreshDo: aBlock\x0a\x09\x22Re-read this model's state.\x22\x0a\x0a\x09jQuery ajax: #{ \x0a\x09\x09'url' -> (self path, '?id=',self id asString).\x0a\x09\x09'type' -> 'GET'.\x0a\x09\x09'cache' -> false.\x0a\x09\x09'success' -> [:x| self onAfterRefresh: x done: aBlock].\x0a\x09\x09'fail' -> [:x| self onRefeshFail: x].\x0a\x09\x09'error' -> [:x| self onRefreshFail: x]}\x09\x0a",
messageSends: ["ajax:", "->", ",", "asString", "id", "path", "onAfterRefresh:done:", "onRefeshFail:", "onRefreshFail:"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "remoteSaveDo:",
category: 'actions',
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(self)._path()),_st("type").__minus_gt("PUT"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(self)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfterSave_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onSaveFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onSaveFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"remoteSaveDo:",{aBlock:aBlock},smalltalk.MaplessModel)})},
args: ["aBlock"],
source: "remoteSaveDo: aBlock\x0a\x0a\x09jQuery ajax: #{ \x0a\x09\x09'url' -> self path.\x0a\x09\x09'type' -> 'PUT'.\x0a\x09\x09'cache' -> false.\x0a\x09\x09'data' -> self asJSONString.\x0a\x09\x09'success' -> [:x| self onAfterSave: x done: aBlock].\x0a\x09\x09'fail' -> [:x| self onSaveFail: x].\x0a\x09\x09'error' -> [:x| self onSaveFail: x]}\x0a",
messageSends: ["ajax:", "->", "path", "asJSONString", "onAfterSave:done:", "onSaveFail:"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "save",
category: 'actions',
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._saveDo_((function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"save",{},smalltalk.MaplessModel)})},
args: [],
source: "save\x0a\x09^ self saveDo:[nil]",
messageSends: ["saveDo:"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "saveDo:",
category: 'actions',
fn: function (aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localSave();
_st(self)._remoteSaveDo_(aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"saveDo:",{aBlock:aBlock},smalltalk.MaplessModel)})},
args: ["aBlock"],
source: "saveDo: aBlock\x0a\x0a\x09self localSave.\x0a\x09self remoteSaveDo: aBlock\x0a",
messageSends: ["localSave", "remoteSaveDo:"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "subModelAt:",
category: 'accessing',
fn: function (aSelector){
var self=this;
var subModelData,modelClass;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4,$5;
subModelData=_st(self["@data"])._at_(aSelector);
$1=subModelData;
if(($receiver = $1) == nil || $receiver == undefined){
return nil;
} else {
$1;
};
modelClass=_st(subModelData)._at_("modelClass");
$2=modelClass;
if(($receiver = $2) == nil || $receiver == undefined){
return nil;
} else {
$2;
};
modelClass=_st(_st((smalltalk.Smalltalk || Smalltalk))._current())._at_(modelClass);
$3=modelClass;
if(($receiver = $3) == nil || $receiver == undefined){
$4=_st((smalltalk.ModelMetadataError || ModelMetadataError))._signal_(_st(_st("Cannot find ").__comma(_st(aSelector)._asString())).__comma("'s class for this metadata"));
return $4;
} else {
$3;
};
$5=_st(modelClass)._fromReified_(subModelData);
return $5;
}, function($ctx1) {$ctx1.fill(self,"subModelAt:",{aSelector:aSelector,subModelData:subModelData,modelClass:modelClass},smalltalk.MaplessModel)})},
args: ["aSelector"],
source: "subModelAt: aSelector\x0a\x09\x22Answers the reified submodel (instantiating if necessary).\x22\x0a\x09\x0a\x09| subModelData modelClass |\x0a\x0a\x09subModelData := data at: aSelector.\x0a\x09subModelData ifNil:[^nil].\x0a\x09\x0a\x09modelClass := subModelData at: 'modelClass'.\x0a\x09modelClass ifNil:[^nil].\x0a\x09modelClass := Smalltalk current at: modelClass.\x0a\x0a\x09modelClass ifNil:[^ModelMetadataError signal: 'Cannot find ',aSelector asString,'''s class for this metadata'].\x0a\x09\x0a\x09^ modelClass fromReified: subModelData",
messageSends: ["at:", "ifNil:", "current", "signal:", ",", "asString", "fromReified:"],
referencedClasses: ["Smalltalk", "ModelMetadataError"]
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "syncFrom:",
category: 'actions',
fn: function (someJson){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._syncWith_(_st(_st(self)._class())._reify_(someJson));
return $1;
}, function($ctx1) {$ctx1.fill(self,"syncFrom:",{someJson:someJson},smalltalk.MaplessModel)})},
args: ["someJson"],
source: "syncFrom: someJson\x0a\x0a\x09^ self syncWith: (self class reify: someJson)",
messageSends: ["syncWith:", "reify:", "class"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "syncWith:",
category: 'actions',
fn: function (aReifiedJSON){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=aReifiedJSON;
if(($receiver = $1) == nil || $receiver == undefined){
return nil;
} else {
$1;
};
$2=_st(_st(_st(aReifiedJSON)._at_("modelClass"))._isNil())._or_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(aReifiedJSON)._at_("modelClass")).__tild_eq(_st(_st(self)._class())._name());
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
if(smalltalk.assert($2)){
_st(self)._error_("this JSON does not fit here");
};
self["@data"]=aReifiedJSON;
return self}, function($ctx1) {$ctx1.fill(self,"syncWith:",{aReifiedJSON:aReifiedJSON},smalltalk.MaplessModel)})},
args: ["aReifiedJSON"],
source: "syncWith: aReifiedJSON\x0a\x09\x22Sync the current values in this model \x0a\x09with the ones coming in aReifiedJSON.\x22\x0a\x0a\x09aReifiedJSON ifNil:[^ nil].\x0a\x0a\x09((aReifiedJSON at: 'modelClass') isNil or:[\x0a\x09(aReifiedJSON at: 'modelClass') ~= self class name]) ifTrue:[\x0a\x09\x09self error: 'this JSON does not fit here'].\x0a\x0a\x09data := aReifiedJSON",
messageSends: ["ifNil:", "ifTrue:", "error:", "or:", "~=", "name", "class", "at:", "isNil"],
referencedClasses: []
}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "url",
category: 'accessing',
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(self)._path()).__comma("/")).__comma(_st(_st(self)._id())._asString());
return $1;
}, function($ctx1) {$ctx1.fill(self,"url",{},smalltalk.MaplessModel)})},
args: [],
source: "url\x0a\x0a\x09^ self path,'/',self id asString\x0a",
messageSends: [",", "asString", "id", "path"],
referencedClasses: []
}),
smalltalk.MaplessModel);


smalltalk.addMethod(
smalltalk.method({
selector: "atId:do:",
category: 'accessing',
fn: function (anId,onDone){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._read_do_(anId,onDone);
return $1;
}, function($ctx1) {$ctx1.fill(self,"atId:do:",{anId:anId,onDone:onDone},smalltalk.MaplessModel.klass)})},
args: ["anId", "onDone"],
source: "atId: anId do: onDone\x0a\x09\x22Answers the instance of this model found at anId (or nil)\x0a\x09using th answer in the onDone callback\x22\x0a \x0a\x09^ self read: anId do: onDone\x0a",
messageSends: ["read:do:"],
referencedClasses: []
}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "create",
category: 'actions',
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._createDo_((function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"create",{},smalltalk.MaplessModel.klass)})},
args: [],
source: "create\x0a\x0a\x09^ self createDo: [nil]",
messageSends: ["createDo:"],
referencedClasses: []
}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "createDo:",
category: 'actions',
fn: function (aBlock){
var self=this;
var newInstance;
return smalltalk.withContext(function($ctx1) { 
var $1;
newInstance=_st(_st(self)._basicNew())._initialize();
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(self)._path()),_st("type").__minus_gt("POST"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(newInstance)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(newInstance)._onAfterCreate_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st((smalltalk.ModelCreateError || ModelCreateError))._signal_(_st(_st(_st("Could not create ").__comma(_st(self)._name())).__comma(":  ")).__comma(_st(x)._responseText()));
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st((smalltalk.ModelCreateError || ModelCreateError))._signal_(_st(_st(_st("Could not create ").__comma(_st(self)._name())).__comma(":  ")).__comma(_st(x)._responseText()));
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
$1=newInstance;
return $1;
}, function($ctx1) {$ctx1.fill(self,"createDo:",{aBlock:aBlock,newInstance:newInstance},smalltalk.MaplessModel.klass)})},
args: ["aBlock"],
source: "createDo: aBlock\x0a\x0a\x09| newInstance |\x0a\x0a\x09newInstance := self basicNew initialize.\x0a\x0a\x09jQuery ajax: #{\x0a\x09\x09'url'-> self path.\x0a\x09\x09'type' -> 'POST'.\x0a\x09\x09'cache'-> false.\x0a\x09\x09'data' -> newInstance asJSONString.\x0a\x09\x09'success' -> [:x| newInstance onAfterCreate: x done: aBlock].\x0a\x09\x09'fail' -> [:x| ModelCreateError signal: 'Could not create ', self name,':  ', x responseText].\x0a\x09\x09'error' -> [:x| ModelCreateError signal: 'Could not create ', self name,':  ', x responseText]}.\x0a\x0a\x09^ newInstance\x0a",
messageSends: ["initialize", "basicNew", "ajax:", "->", "path", "asJSONString", "onAfterCreate:done:", "signal:", ",", "responseText", "name"],
referencedClasses: ["ModelCreateError"]
}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "createdOnFrom:",
category: 'actions',
fn: function (aReifiedJSON){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st((smalltalk.Date || Date))._fromString_(aReifiedJSON);
return $1;
}, function($ctx1) {$ctx1.fill(self,"createdOnFrom:",{aReifiedJSON:aReifiedJSON},smalltalk.MaplessModel.klass)})},
args: ["aReifiedJSON"],
source: "createdOnFrom: aReifiedJSON\x0a\x0a\x09^ Date fromString: aReifiedJSON",
messageSends: ["fromString:"],
referencedClasses: ["Date"]
}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "fromJson:",
category: 'actions',
fn: function (someJson){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._new())._syncFrom_(someJson);
return $1;
}, function($ctx1) {$ctx1.fill(self,"fromJson:",{someJson:someJson},smalltalk.MaplessModel.klass)})},
args: ["someJson"],
source: "fromJson: someJson\x0a\x09\x22Answers a new instance of this model and returns it\x0a\x09in the state dictated by someJson.\x22\x0a\x0a\x09^ self new syncFrom: someJson",
messageSends: ["syncFrom:", "new"],
referencedClasses: []
}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "fromJsonString:",
category: 'actions',
fn: function (someJsonString){
var self=this;
var json,modelClass;
function $Smalltalk(){return smalltalk.Smalltalk||(typeof Smalltalk=="undefined"?nil:Smalltalk)}
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
json=_st(self)._reify_(someJsonString);
modelClass=_st(json)._at_ifAbsent_("modelClass",(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$2=_st(modelClass)._isNil();
if(smalltalk.assert($2)){
$1=nil;
} else {
$1=_st(_st(_st(_st($Smalltalk())._current())._at_(modelClass))._new())._syncFrom_(someJsonString);
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"fromJsonString:",{someJsonString:someJsonString,json:json,modelClass:modelClass},smalltalk.MaplessModel.klass)})},
args: ["someJsonString"],
source: "fromJsonString: someJsonString\x0a\x09\x22Answers a new instance of this model and returns it\x0a\x09in the state dictated by someJsonString.\x22\x0a\x0a\x09| json modelClass |\x0a\x09json := self reify: someJsonString.\x0a\x09modelClass := json at: 'modelClass' ifAbsent:[nil].\x0a\x09\x0a\x09^ modelClass isNil\x0a\x09\x09ifTrue:[nil]\x0a\x09\x09ifFalse:[(Smalltalk current at: modelClass) new syncFrom: someJsonString]",
messageSends: ["reify:", "at:ifAbsent:", "ifTrue:ifFalse:", "syncFrom:", "new", "at:", "current", "isNil"],
referencedClasses: ["Smalltalk"]
}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "fromReified:",
category: 'actions',
fn: function (aReifiedJSON){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._new())._syncWith_(aReifiedJSON);
return $1;
}, function($ctx1) {$ctx1.fill(self,"fromReified:",{aReifiedJSON:aReifiedJSON},smalltalk.MaplessModel.klass)})},
args: ["aReifiedJSON"],
source: " fromReified: aReifiedJSON\x0a\x09\x22Answers a new instance of this model and returns it\x0a\x09in sync with aReifiedJSON.\x22\x0a\x0a\x09^ self new syncWith: aReifiedJSON",
messageSends: ["syncWith:", "new"],
referencedClasses: []
}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "localDelete:",
category: 'actions',
fn: function (aMaplessModel){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._localStorage())._removeItem_(_st(aMaplessModel)._id());
return self}, function($ctx1) {$ctx1.fill(self,"localDelete:",{aMaplessModel:aMaplessModel},smalltalk.MaplessModel.klass)})},
args: ["aMaplessModel"],
source: "localDelete: aMaplessModel\x0a\x0a\x09window localStorage removeItem: aMaplessModel id",
messageSends: ["removeItem:", "id", "localStorage"],
referencedClasses: []
}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "localLoadAt:",
category: 'actions',
fn: function (anId){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._fromJsonString_(_st(_st(window)._localStorage())._getItem_(anId));
return $1;
}, function($ctx1) {$ctx1.fill(self,"localLoadAt:",{anId:anId},smalltalk.MaplessModel.klass)})},
args: ["anId"],
source: "localLoadAt: anId\x0a\x0a\x09^ self fromJsonString: (window localStorage getItem: anId)",
messageSends: ["fromJsonString:", "getItem:", "localStorage"],
referencedClasses: []
}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave:",
category: 'actions',
fn: function (aMaplessModel){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._localStorage())._setItem_value_(_st(aMaplessModel)._id(),_st(aMaplessModel)._asJSONString());
return self}, function($ctx1) {$ctx1.fill(self,"localSave:",{aMaplessModel:aMaplessModel},smalltalk.MaplessModel.klass)})},
args: ["aMaplessModel"],
source: "localSave: aMaplessModel\x0a\x0a\x09window localStorage\x0a\x09\x09setItem: aMaplessModel id\x0a\x09\x09value: aMaplessModel asJSONString",
messageSends: ["setItem:value:", "id", "asJSONString", "localStorage"],
referencedClasses: []
}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "manyFromJson:",
category: 'actions',
fn: function (someJson){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st((smalltalk.JSON || JSON))._parse_(someJson))._collect_((function(each){
return smalltalk.withContext(function($ctx2) {
return _st(self)._fromReified_(each);
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"manyFromJson:",{someJson:someJson},smalltalk.MaplessModel.klass)})},
args: ["someJson"],
source: "manyFromJson: someJson\x0a\x0a\x09^ (JSON parse: someJson) collect:[:each|\x0a\x09\x09 self fromReified: each ]",
messageSends: ["collect:", "fromReified:", "parse:"],
referencedClasses: ["JSON"]
}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "newUUID",
category: 'actions',
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._uuidGenerator())._value();
return $1;
}, function($ctx1) {$ctx1.fill(self,"newUUID",{},smalltalk.MaplessModel.klass)})},
args: [],
source: "newUUID\x0a\x09^ self uuidGenerator value",
messageSends: ["value", "uuidGenerator"],
referencedClasses: []
}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "onAfterRead:done:",
category: 'reactions',
fn: function (someJson,aBlock){
var self=this;
var reified;
return smalltalk.withContext(function($ctx1) { 
var $1;
reified=_st(self)._fromJson_(someJson);
_st(aBlock)._value_(reified);
$1=reified;
return $1;
}, function($ctx1) {$ctx1.fill(self,"onAfterRead:done:",{someJson:someJson,aBlock:aBlock,reified:reified},smalltalk.MaplessModel.klass)})},
args: ["someJson", "aBlock"],
source: "onAfterRead: someJson done: aBlock\x0a\x0a\x09| reified |\x0a\x09\x0a\x09reified := self fromJson: someJson.\x0a\x09aBlock value: reified.\x0a\x09^ reified",
messageSends: ["fromJson:", "value:"],
referencedClasses: []
}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "read:",
category: 'actions',
fn: function (anId){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._read_do_(anId,(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"read:",{anId:anId},smalltalk.MaplessModel.klass)})},
args: ["anId"],
source: "read: anId\x0a\x0a\x09self read: anId do: [nil]",
messageSends: ["read:do:"],
referencedClasses: []
}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "read:do:",
category: 'actions',
fn: function (anId,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(_st(self)._path()).__comma("?id=")).__comma(anId)),_st("type").__minus_gt("GET"),_st("cache").__minus_gt(false),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfterRead_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st((smalltalk.ModelReadError || ModelReadError))._signal_(_st(_st(_st("Could not read ").__comma(_st(self)._name())).__comma(":  ")).__comma(_st(x)._responseText()));
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st((smalltalk.ModelReadError || ModelReadError))._signal_(_st(_st(_st("Could not read ").__comma(_st(self)._name())).__comma(":  ")).__comma(_st(x)._responseText()));
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"read:do:",{anId:anId,aBlock:aBlock},smalltalk.MaplessModel.klass)})},
args: ["anId", "aBlock"],
source: "read: anId do: aBlock\x0a\x0a\x09jQuery ajax: #{ \x0a\x09\x09'url' -> (self path, '?id=',anId).\x0a\x09\x09'type'-> 'GET'.\x0a\x09\x09'cache'-> false.\x0a\x09\x09'success'-> [:x| self onAfterRead: x done: aBlock].\x0a\x09\x09'fail' -> [:x| ModelReadError signal: 'Could not read ', self name,':  ', x responseText].\x0a\x09\x09'error'-> [:x| ModelReadError signal: 'Could not read ', self name,':  ', x responseText]}",
messageSends: ["ajax:", "->", ",", "path", "onAfterRead:done:", "signal:", "responseText", "name"],
referencedClasses: ["ModelReadError"]
}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "reify:",
category: 'actions',
fn: function (someJson){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st((smalltalk.JSON || JSON))._parse_(someJson);
return $1;
}, function($ctx1) {$ctx1.fill(self,"reify:",{someJson:someJson},smalltalk.MaplessModel.klass)})},
args: ["someJson"],
source: "reify: someJson\x0a\x09\x22Returns a simple javascript object with\x0a\x09the attributes meant for the matching instance variable content of this model.\x22\x0a\x0a\x09^ JSON parse: someJson",
messageSends: ["parse:"],
referencedClasses: ["JSON"]
}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "uuidGenerator",
category: 'actions',
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return function guid() {
    function _p8(s) {
        var p = (Math.random().toString(16)+"000000000").substr(2,8);
        return s ? "-" + p.substr(0,4) + "-" + p.substr(4,4) : p ;
    }
    return _p8() + _p8(true) + _p8(true) + _p8();
};
return self}, function($ctx1) {$ctx1.fill(self,"uuidGenerator",{},smalltalk.MaplessModel.klass)})},
args: [],
source: "uuidGenerator\x0a\x09\x22Taken from:\x0a\x09http://stackoverflow.com/questions/105034/how-to-create-a-guid-uuid-in-javascript\x22\x0a\x0a<return function guid() {\x0a    function _p8(s) {\x0a        var p = (Math.random().toString(16)+\x22000000000\x22).substr(2,8);\x0a        return s ? \x22-\x22 + p.substr(0,4) + \x22-\x22 + p.substr(4,4) : p ;\x0a    }\x0a    return _p8() + _p8(true) + _p8(true) + _p8();\x0a}>",
messageSends: [],
referencedClasses: []
}),
smalltalk.MaplessModel.klass);


