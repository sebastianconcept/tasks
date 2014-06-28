smalltalk.addPackage('Reactive');
smalltalk.addClass('RAnnouncement', smalltalk.Object, ['subject'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "subject",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@subject"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"subject",{},smalltalk.RAnnouncement)});},
args: [],
source: "subject\x0a\x0a\x09^ subject",
messageSends: [],
referencedClasses: []
}),
smalltalk.RAnnouncement);

smalltalk.addMethod(
smalltalk.method({
selector: "subject:",
category: 'not yet classified',
fn: function (anObject) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@subject"]=anObject;
return self}, function($ctx1) {$ctx1.fill(self,"subject:",{anObject:anObject},smalltalk.RAnnouncement)});},
args: ["anObject"],
source: "subject: anObject\x0a\x0a\x09subject := anObject",
messageSends: [],
referencedClasses: []
}),
smalltalk.RAnnouncement);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
category: 'not yet classified',
fn: function (aSubject) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._subject_(aSubject);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"for:",{aSubject:aSubject},smalltalk.RAnnouncement.klass)});},
args: ["aSubject"],
source: "for: aSubject\x0a\x0a\x09^ self new \x0a\x09\x09subject: aSubject;\x0a\x09\x09yourself",
messageSends: ["subject:", "new", "yourself"],
referencedClasses: []
}),
smalltalk.RAnnouncement.klass);


smalltalk.addClass('RTaskChanged', smalltalk.RAnnouncement, [], 'Reactive');


smalltalk.addClass('RTaskRemoved', smalltalk.RAnnouncement, [], 'Reactive');


smalltalk.addClass('RTasksSorted', smalltalk.RAnnouncement, [], 'Reactive');


smalltalk.addClass('RClient', smalltalk.Object, ['socket', 'uri', 'onOpen'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "initializeSocket",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._makeSocketOn_(_st(self)._uri());
_st($2)._onopen_(_st(self)._onOpen());
_st($2)._onclose_((function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st($2)._onmessage_((function(anEvent){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onMessage_(anEvent);
}, function($ctx2) {$ctx2.fillBlock({anEvent:anEvent},$ctx1)})}));
_st($2)._onerror_((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onError_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}));
$3=_st($2)._yourself();
self["@socket"]=$3;
$1=self["@socket"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeSocket",{},smalltalk.RClient)});},
args: [],
source: "initializeSocket\x0a\x09\x0a\x09^ socket := (self makeSocketOn: self uri)\x0a\x09\x09\x09\x09\x09\x09onopen: self onOpen;\x0a\x09\x09\x09\x09\x09\x09onclose: [nil];\x0a\x09\x09\x09\x09\x09\x09onmessage:[:anEvent | self onMessage: anEvent];\x0a\x09\x09\x09\x09\x09\x09onerror:[ :x | self onError: x ];\x0a\x09\x09\x09\x09\x09\x09yourself\x09",
messageSends: ["onopen:", "onOpen", "makeSocketOn:", "uri", "onclose:", "onmessage:", "onMessage:", "onerror:", "onError:", "yourself"],
referencedClasses: []
}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeURI",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@uri"]=_st(_st("ws://").__comma(_st(_st(window)._location())._hostname())).__comma(":21004/socket");
$1=self["@uri"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeURI",{},smalltalk.RClient)});},
args: [],
source: "initializeURI\x0a\x0a\x09^ uri := 'ws://',window location hostname,':21004/socket'",
messageSends: [",", "hostname", "location"],
referencedClasses: []
}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "makeSocketOn:",
category: 'not yet classified',
fn: function (anUri) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
return new WebSocket(anUri);
return self}, function($ctx1) {$ctx1.fill(self,"makeSocketOn:",{anUri:anUri},smalltalk.RClient)});},
args: ["anUri"],
source: "makeSocketOn: anUri\x0a\x0a\x09<return new WebSocket(anUri)>\x0a\x09",
messageSends: [],
referencedClasses: []
}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "onError:",
category: 'not yet classified',
fn: function (anException) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._halt();
return self}, function($ctx1) {$ctx1.fill(self,"onError:",{anException:anException},smalltalk.RClient)});},
args: ["anException"],
source: "onError: anException\x0a\x0a\x09self halt",
messageSends: ["halt"],
referencedClasses: []
}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "onMessage:",
category: 'not yet classified',
fn: function (anEvent) {
var self=this;
var command;
function $MaplessModel(){return smalltalk.MaplessModel||(typeof MaplessModel=="undefined"?nil:MaplessModel)}
return smalltalk.withContext(function($ctx1) { 
command=_st($MaplessModel())._fromJsonString_(_st(anEvent)._data());
_st(command)._react();
return self}, function($ctx1) {$ctx1.fill(self,"onMessage:",{anEvent:anEvent,command:command},smalltalk.RClient)});},
args: ["anEvent"],
source: "onMessage: anEvent\x0a\x0a\x09| command |\x0a\x09\x0a\x09command := MaplessModel fromJsonString: anEvent data.\x0a\x09command react",
messageSends: ["fromJsonString:", "data", "react"],
referencedClasses: ["MaplessModel"]
}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "onOpen",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@onOpen"];
if(($receiver = $2) == nil || $receiver == undefined){
self["@onOpen"]=(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})});
$1=self["@onOpen"];
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"onOpen",{},smalltalk.RClient)});},
args: [],
source: "onOpen\x0a\x0a\x09^ onOpen ifNil:[ onOpen := [nil] ]",
messageSends: ["ifNil:"],
referencedClasses: []
}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "onOpen:",
category: 'not yet classified',
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@onOpen"]=aBlock;
return self}, function($ctx1) {$ctx1.fill(self,"onOpen:",{aBlock:aBlock},smalltalk.RClient)});},
args: ["aBlock"],
source: "onOpen: aBlock\x0a\x0a\x09onOpen := aBlock",
messageSends: [],
referencedClasses: []
}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "open",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._initializeSocket();
return self}, function($ctx1) {$ctx1.fill(self,"open",{},smalltalk.RClient)});},
args: [],
source: "open\x0a\x0a\x09self initializeSocket\x0a",
messageSends: ["initializeSocket"],
referencedClasses: []
}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "send:",
category: 'not yet classified',
fn: function (aString) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._socket())._send_(aString);
return $1;
}, function($ctx1) {$ctx1.fill(self,"send:",{aString:aString},smalltalk.RClient)});},
args: ["aString"],
source: "send: aString\x0a\x0a\x09^ self socket send: aString",
messageSends: ["send:", "socket"],
referencedClasses: []
}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "socket",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@socket"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeSocket();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"socket",{},smalltalk.RClient)});},
args: [],
source: "socket\x0a\x0a\x09^ socket ifNil:[self initializeSocket]",
messageSends: ["ifNil:", "initializeSocket"],
referencedClasses: []
}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "uri",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@uri"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeURI();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"uri",{},smalltalk.RClient)});},
args: [],
source: "uri\x0a\x0a\x09^ uri ifNil:[self initializeURI]",
messageSends: ["ifNil:", "initializeURI"],
referencedClasses: []
}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "uri:",
category: 'not yet classified',
fn: function (aString) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@uri"]=aString;
return self}, function($ctx1) {$ctx1.fill(self,"uri:",{aString:aString},smalltalk.RClient)});},
args: ["aString"],
source: "uri: aString\x0a\x0a\x09uri := aString",
messageSends: [],
referencedClasses: []
}),
smalltalk.RClient);



smalltalk.addClass('RModel', smalltalk.MaplessModel, [], 'Reactive');

smalltalk.addMethod(
smalltalk.method({
selector: "path",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
return "api";
}, function($ctx1) {$ctx1.fill(self,"path",{},smalltalk.RModel.klass)});},
args: [],
source: "path\x0a\x0a\x09^ 'api'",
messageSends: [],
referencedClasses: []
}),
smalltalk.RModel.klass);


smalltalk.addClass('RCommand', smalltalk.RModel, [], 'Reactive');
smalltalk.RCommand.comment="This is the abstract class for the remote commands"
smalltalk.addMethod(
smalltalk.method({
selector: "api",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(window)._app())._session())._api();
return $1;
}, function($ctx1) {$ctx1.fill(self,"api",{},smalltalk.RCommand)});},
args: [],
source: "api\x0a\x0a\x09^ window app session api",
messageSends: ["api", "session", "app"],
referencedClasses: []
}),
smalltalk.RCommand);

smalltalk.addMethod(
smalltalk.method({
selector: "execute",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._executeDo_((function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"execute",{},smalltalk.RCommand)});},
args: [],
source: "execute\x0a\x0a\x09self executeDo:[nil]",
messageSends: ["executeDo:"],
referencedClasses: []
}),
smalltalk.RCommand);

smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
smalltalk.RModel.fn.prototype._initialize.apply(_st(self), []);
_st(self)._sessionId_(_st(_st(_st(window)._app())._session())._id());
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.RCommand)});},
args: [],
source: "initialize\x0a\x0a\x09super initialize.\x0a\x09\x0a\x09self sessionId: window app session id",
messageSends: ["initialize", "sessionId:", "id", "session", "app"],
referencedClasses: []
}),
smalltalk.RCommand);

smalltalk.addMethod(
smalltalk.method({
selector: "notifyRemote",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._api())._send_(_st(self)._asJSONString());
return self}, function($ctx1) {$ctx1.fill(self,"notifyRemote",{},smalltalk.RCommand)});},
args: [],
source: "notifyRemote\x0a\x0a\x09self api send: self asJSONString",
messageSends: ["send:", "asJSONString", "api"],
referencedClasses: []
}),
smalltalk.RCommand);

smalltalk.addMethod(
smalltalk.method({
selector: "onAfter:done:",
category: 'not yet classified',
fn: function (x, aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aBlock)._value();
return self}, function($ctx1) {$ctx1.fill(self,"onAfter:done:",{x:x,aBlock:aBlock},smalltalk.RCommand)});},
args: ["x", "aBlock"],
source: "onAfter: x done: aBlock\x0a\x0a\x09aBlock value",
messageSends: ["value"],
referencedClasses: []
}),
smalltalk.RCommand);

smalltalk.addMethod(
smalltalk.method({
selector: "onFail:",
category: 'not yet classified',
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._halt();
return self}, function($ctx1) {$ctx1.fill(self,"onFail:",{aBlock:aBlock},smalltalk.RCommand)});},
args: ["aBlock"],
source: "onFail: aBlock\x0a\x0a\x09self halt",
messageSends: ["halt"],
referencedClasses: []
}),
smalltalk.RCommand);

smalltalk.addMethod(
smalltalk.method({
selector: "react",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._subclassResponsibility();
return self}, function($ctx1) {$ctx1.fill(self,"react",{},smalltalk.RCommand)});},
args: [],
source: "react\x0a\x0a\x09self subclassResponsibility",
messageSends: ["subclassResponsibility"],
referencedClasses: []
}),
smalltalk.RCommand);



smalltalk.addClass('RAddList', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
category: 'not yet classified',
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localSave();
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(self)._path()).__comma("/addList")),_st("type").__minus_gt("PUT"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(self)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfter_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"executeDo:",{aBlock:aBlock},smalltalk.RAddList)});},
args: ["aBlock"],
source: "executeDo: aBlock\x0a\x0a\x09self localSave.\x0a\x0a\x09jQuery ajax: #{ \x0a\x09\x09'url' -> (self path,'/addList').\x0a\x09\x09'type' -> 'PUT'.\x0a\x09\x09'cache' -> false.\x0a\x09\x09'data' -> self asJSONString.\x0a\x09\x09'success' -> [:x| self onAfter: x done: aBlock].\x0a\x09\x09'fail' -> [:x| self onFail: x].\x0a\x09\x09'error' -> [:x| self onFail: x]}",
messageSends: ["localSave", "ajax:", "->", ",", "path", "asJSONString", "onAfter:done:", "onFail:"],
referencedClasses: []
}),
smalltalk.RAddList);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave",
category: 'not yet classified',
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st($RStorage())._save_(_st(self)._list());
return self}, function($ctx1) {$ctx1.fill(self,"localSave",{},smalltalk.RAddList)});},
args: [],
source: "localSave\x0a\x0a\x09RStorage save: self list.",
messageSends: ["save:", "list"],
referencedClasses: ["RStorage"]
}),
smalltalk.RAddList);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
category: 'not yet classified',
fn: function (aList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._list_(aList);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"for:",{aList:aList},smalltalk.RAddList.klass)});},
args: ["aList"],
source: "for: aList\x0a\x0a\x09^ self new\x0a\x09\x09list: aList;\x0a\x09\x09yourself",
messageSends: ["list:", "new", "yourself"],
referencedClasses: []
}),
smalltalk.RAddList.klass);


smalltalk.addClass('RAddTask', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
category: 'not yet classified',
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localSave();
_st(self)._updateList_(_st(self)._list());
_st(self)._notifyRemote();
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(self)._path()).__comma("/addTask")),_st("type").__minus_gt("PUT"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(self)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfter_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"executeDo:",{aBlock:aBlock},smalltalk.RAddTask)});},
args: ["aBlock"],
source: "executeDo: aBlock\x0a\x0a\x09self localSave.\x0a\x09self updateList: self list.\x0a\x09self notifyRemote.\x0a\x0a\x09jQuery ajax: #{ \x0a\x09\x09'url' -> (self path,'/addTask').\x0a\x09\x09'type' -> 'PUT'.\x0a\x09\x09'cache' -> false.\x0a\x09\x09'data' -> self asJSONString.\x0a\x09\x09'success' -> [:x| self onAfter: x done: aBlock].\x0a\x09\x09'fail' -> [:x| self onFail: x].\x0a\x09\x09'error' -> [:x| self onFail: x]}",
messageSends: ["localSave", "updateList:", "list", "notifyRemote", "ajax:", "->", ",", "path", "asJSONString", "onAfter:done:", "onFail:"],
referencedClasses: []
}),
smalltalk.RAddTask);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave",
category: 'not yet classified',
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st($RStorage())._save_(_st(self)._task());
return self}, function($ctx1) {$ctx1.fill(self,"localSave",{},smalltalk.RAddTask)});},
args: [],
source: "localSave\x0a\x0a\x09RStorage save: self task.",
messageSends: ["save:", "task"],
referencedClasses: ["RStorage"]
}),
smalltalk.RAddTask);

smalltalk.addMethod(
smalltalk.method({
selector: "react",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._app())._addTask_(self);
return self}, function($ctx1) {$ctx1.fill(self,"react",{},smalltalk.RAddTask)});},
args: [],
source: "react\x0a\x0a\x09window app addTask: self",
messageSends: ["addTask:", "app"],
referencedClasses: []
}),
smalltalk.RAddTask);

smalltalk.addMethod(
smalltalk.method({
selector: "saveAndUpdateList:",
category: 'not yet classified',
fn: function (aList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localSave();
_st(self)._updateList_(aList);
return self}, function($ctx1) {$ctx1.fill(self,"saveAndUpdateList:",{aList:aList},smalltalk.RAddTask)});},
args: ["aList"],
source: "saveAndUpdateList: aList\x0a\x0a\x09self localSave.\x0a\x09self updateList: aList",
messageSends: ["localSave", "updateList:"],
referencedClasses: []
}),
smalltalk.RAddTask);

smalltalk.addMethod(
smalltalk.method({
selector: "updateList:",
category: 'not yet classified',
fn: function (aList) {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st(aList)._addTask_(_st(_st(self)._task())._id());
_st($RStorage())._save_(aList);
return self}, function($ctx1) {$ctx1.fill(self,"updateList:",{aList:aList},smalltalk.RAddTask)});},
args: ["aList"],
source: "updateList: aList\x0a\x0a\x09aList addTask: self task id.\x0a\x09RStorage save: aList.",
messageSends: ["addTask:", "id", "task", "save:"],
referencedClasses: ["RStorage"]
}),
smalltalk.RAddTask);


smalltalk.addMethod(
smalltalk.method({
selector: "for:in:",
category: 'not yet classified',
fn: function (aTask, aList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._task_(aTask);
_st($2)._list_(aList);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"for:in:",{aTask:aTask,aList:aList},smalltalk.RAddTask.klass)});},
args: ["aTask", "aList"],
source: "for: aTask in: aList\x0a\x0a\x09^ self new\x0a\x09\x09task: aTask;\x0a\x09\x09list: aList;\x0a\x09\x09yourself",
messageSends: ["task:", "new", "list:", "yourself"],
referencedClasses: []
}),
smalltalk.RAddTask.klass);


smalltalk.addClass('RAddUser', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
category: 'not yet classified',
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localSave();
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(self)._path()).__comma("/addUser")),_st("type").__minus_gt("PUT"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(self)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfter_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"executeDo:",{aBlock:aBlock},smalltalk.RAddUser)});},
args: ["aBlock"],
source: "executeDo: aBlock\x0a\x0a\x09self localSave.\x0a\x0a\x09jQuery ajax: #{ \x0a\x09\x09'url' -> (self path,'/addUser').\x0a\x09\x09'type' -> 'PUT'.\x0a\x09\x09'cache' -> false.\x0a\x09\x09'data' -> self asJSONString.\x0a\x09\x09'success' -> [:x| self onAfter: x done: aBlock].\x0a\x09\x09'fail' -> [:x| self onFail: x].\x0a\x09\x09'error' -> [:x| self onFail: x]}",
messageSends: ["localSave", "ajax:", "->", ",", "path", "asJSONString", "onAfter:done:", "onFail:"],
referencedClasses: []
}),
smalltalk.RAddUser);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave",
category: 'not yet classified',
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st($RStorage())._save_(_st(self)._user());
return self}, function($ctx1) {$ctx1.fill(self,"localSave",{},smalltalk.RAddUser)});},
args: [],
source: "localSave\x0a\x0a\x09RStorage save: self user\x0a\x09",
messageSends: ["save:", "user"],
referencedClasses: ["RStorage"]
}),
smalltalk.RAddUser);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
category: 'not yet classified',
fn: function (anUser) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._user_(anUser);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"for:",{anUser:anUser},smalltalk.RAddUser.klass)});},
args: ["anUser"],
source: "for: anUser\x0a\x0a\x09^ self new\x0a\x09\x09user: anUser;\x0a\x09\x09yourself",
messageSends: ["user:", "new", "yourself"],
referencedClasses: []
}),
smalltalk.RAddUser.klass);


smalltalk.addClass('RChangeClients', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
category: 'not yet classified',
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._notifyRemote();
return self}, function($ctx1) {$ctx1.fill(self,"executeDo:",{aBlock:aBlock},smalltalk.RChangeClients)});},
args: ["aBlock"],
source: "executeDo: aBlock\x0a\x0a\x09self notifyRemote.",
messageSends: ["notifyRemote"],
referencedClasses: []
}),
smalltalk.RChangeClients);

smalltalk.addMethod(
smalltalk.method({
selector: "react",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._app())._changeClients_(self);
return self}, function($ctx1) {$ctx1.fill(self,"react",{},smalltalk.RChangeClients)});},
args: [],
source: "react\x0a\x0a\x09window app changeClients: self",
messageSends: ["changeClients:", "app"],
referencedClasses: []
}),
smalltalk.RChangeClients);



smalltalk.addClass('RChangeList', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
category: 'not yet classified',
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localSave();
_st(self)._notifyRemote();
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(self)._path()).__comma("/changeList")),_st("type").__minus_gt("POST"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(self)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfter_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"executeDo:",{aBlock:aBlock},smalltalk.RChangeList)});},
args: ["aBlock"],
source: "executeDo: aBlock\x0a\x0a\x09self localSave.\x0a\x09self notifyRemote.\x0a\x0a\x09jQuery ajax: #{ \x0a\x09\x09'url' -> (self path,'/changeList').\x0a\x09\x09'type' -> 'POST'.\x0a\x09\x09'cache' -> false.\x0a\x09\x09'data' -> self asJSONString.\x0a\x09\x09'success' -> [:x| self onAfter: x done: aBlock].\x0a\x09\x09'fail' -> [:x| self onFail: x].\x0a\x09\x09'error' -> [:x| self onFail: x]}",
messageSends: ["localSave", "notifyRemote", "ajax:", "->", ",", "path", "asJSONString", "onAfter:done:", "onFail:"],
referencedClasses: []
}),
smalltalk.RChangeList);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave",
category: 'not yet classified',
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st($RStorage())._save_(_st(self)._list());
return self}, function($ctx1) {$ctx1.fill(self,"localSave",{},smalltalk.RChangeList)});},
args: [],
source: "localSave\x0a\x0a\x09RStorage save: self list",
messageSends: ["save:", "list"],
referencedClasses: ["RStorage"]
}),
smalltalk.RChangeList);

smalltalk.addMethod(
smalltalk.method({
selector: "react",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._app())._changeList_(self);
return self}, function($ctx1) {$ctx1.fill(self,"react",{},smalltalk.RChangeList)});},
args: [],
source: "react\x0a\x0a\x09window app changeList: self",
messageSends: ["changeList:", "app"],
referencedClasses: []
}),
smalltalk.RChangeList);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
category: 'not yet classified',
fn: function (aList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._list_(aList);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"for:",{aList:aList},smalltalk.RChangeList.klass)});},
args: ["aList"],
source: "for: aList\x0a\x0a\x09^ self new\x0a\x09\x09list: aList;\x0a\x09\x09yourself",
messageSends: ["list:", "new", "yourself"],
referencedClasses: []
}),
smalltalk.RChangeList.klass);


smalltalk.addClass('RChangeTask', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
category: 'not yet classified',
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localSave();
_st(self)._notifyRemote();
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(self)._path()).__comma("/changeTask")),_st("type").__minus_gt("POST"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(self)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfter_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"executeDo:",{aBlock:aBlock},smalltalk.RChangeTask)});},
args: ["aBlock"],
source: "executeDo: aBlock\x0a\x0a\x09self localSave.\x0a\x09self notifyRemote.\x0a\x0a\x09jQuery ajax: #{ \x0a\x09\x09'url' -> (self path,'/changeTask').\x0a\x09\x09'type' -> 'POST'.\x0a\x09\x09'cache' -> false.\x0a\x09\x09'data' -> self asJSONString.\x0a\x09\x09'success' -> [:x| self onAfter: x done: aBlock].\x0a\x09\x09'fail' -> [:x| self onFail: x].\x0a\x09\x09'error' -> [:x| self onFail: x]}",
messageSends: ["localSave", "notifyRemote", "ajax:", "->", ",", "path", "asJSONString", "onAfter:done:", "onFail:"],
referencedClasses: []
}),
smalltalk.RChangeTask);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave",
category: 'not yet classified',
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st($RStorage())._save_(_st(self)._task());
return self}, function($ctx1) {$ctx1.fill(self,"localSave",{},smalltalk.RChangeTask)});},
args: [],
source: "localSave\x0a\x0a\x09RStorage save: self task",
messageSends: ["save:", "task"],
referencedClasses: ["RStorage"]
}),
smalltalk.RChangeTask);

smalltalk.addMethod(
smalltalk.method({
selector: "react",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._app())._changeTask_(self);
return self}, function($ctx1) {$ctx1.fill(self,"react",{},smalltalk.RChangeTask)});},
args: [],
source: "react\x0a\x0a\x09window app changeTask: self",
messageSends: ["changeTask:", "app"],
referencedClasses: []
}),
smalltalk.RChangeTask);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
category: 'not yet classified',
fn: function (aTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._task_(aTask);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"for:",{aTask:aTask},smalltalk.RChangeTask.klass)});},
args: ["aTask"],
source: "for: aTask \x0a\x0a\x09^ self new\x0a\x09\x09task: aTask;\x0a\x09\x09yourself",
messageSends: ["task:", "new", "yourself"],
referencedClasses: []
}),
smalltalk.RChangeTask.klass);


smalltalk.addClass('RRemoveTask', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
category: 'not yet classified',
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localRemove();
_st(self)._updateList_(_st(self)._list());
_st(self)._notifyRemote();
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(self)._path()).__comma("/removeTask")),_st("type").__minus_gt("DELETE"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(self)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfter_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onFail_(x);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"executeDo:",{aBlock:aBlock},smalltalk.RRemoveTask)});},
args: ["aBlock"],
source: "executeDo: aBlock\x0a\x0a\x09self localRemove.\x0a\x09self updateList: self list.\x0a\x09self notifyRemote.\x0a\x0a\x09jQuery ajax: #{ \x0a\x09\x09'url' -> (self path,'/removeTask').\x0a\x09\x09'type' -> 'DELETE'.\x0a\x09\x09'cache' -> false.\x0a\x09\x09'data' -> self asJSONString.\x0a\x09\x09'success' -> [:x| self onAfter: x done: aBlock].\x0a\x09\x09'fail' -> [:x| self onFail: x].\x0a\x09\x09'error' -> [:x| self onFail: x]}",
messageSends: ["localRemove", "updateList:", "list", "notifyRemote", "ajax:", "->", ",", "path", "asJSONString", "onAfter:done:", "onFail:"],
referencedClasses: []
}),
smalltalk.RRemoveTask);

smalltalk.addMethod(
smalltalk.method({
selector: "localRemove",
category: 'not yet classified',
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st($RStorage())._delete_(_st(self)._task());
return self}, function($ctx1) {$ctx1.fill(self,"localRemove",{},smalltalk.RRemoveTask)});},
args: [],
source: "localRemove\x0a\x0a\x09RStorage delete: self task",
messageSends: ["delete:", "task"],
referencedClasses: ["RStorage"]
}),
smalltalk.RRemoveTask);

smalltalk.addMethod(
smalltalk.method({
selector: "react",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._app())._removeTask_(self);
return self}, function($ctx1) {$ctx1.fill(self,"react",{},smalltalk.RRemoveTask)});},
args: [],
source: "react\x0a\x0a\x09window app removeTask: self",
messageSends: ["removeTask:", "app"],
referencedClasses: []
}),
smalltalk.RRemoveTask);

smalltalk.addMethod(
smalltalk.method({
selector: "removeAndUpdateList:",
category: 'not yet classified',
fn: function (aList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localRemove();
_st(self)._updateList_(aList);
return self}, function($ctx1) {$ctx1.fill(self,"removeAndUpdateList:",{aList:aList},smalltalk.RRemoveTask)});},
args: ["aList"],
source: "removeAndUpdateList: aList\x0a\x0a\x09self localRemove.\x0a\x09self updateList: aList\x0a\x09",
messageSends: ["localRemove", "updateList:"],
referencedClasses: []
}),
smalltalk.RRemoveTask);

smalltalk.addMethod(
smalltalk.method({
selector: "updateList:",
category: 'not yet classified',
fn: function (aList) {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st(aList)._removeTask_(_st(_st(self)._task())._id());
_st($RStorage())._save_(aList);
return self}, function($ctx1) {$ctx1.fill(self,"updateList:",{aList:aList},smalltalk.RRemoveTask)});},
args: ["aList"],
source: "updateList: aList\x0a\x0a\x09aList removeTask: self task id.\x0a\x09RStorage save: aList.",
messageSends: ["removeTask:", "id", "task", "save:"],
referencedClasses: ["RStorage"]
}),
smalltalk.RRemoveTask);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
category: 'not yet classified',
fn: function (aTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._task_(aTask);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"for:",{aTask:aTask},smalltalk.RRemoveTask.klass)});},
args: ["aTask"],
source: "for: aTask \x0a\x0a\x09^ self new\x0a\x09\x09task: aTask;\x0a\x09\x09yourself",
messageSends: ["task:", "new", "yourself"],
referencedClasses: []
}),
smalltalk.RRemoveTask.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "for:list:",
category: 'not yet classified',
fn: function (aTask, aList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._task_(aTask);
_st($2)._list_(aList);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"for:list:",{aTask:aTask,aList:aList},smalltalk.RRemoveTask.klass)});},
args: ["aTask", "aList"],
source: "for: aTask list: aList\x0a\x0a\x09^ self new\x0a\x09\x09task: aTask;\x0a\x09\x09list: aList;\x0a\x09\x09yourself",
messageSends: ["task:", "new", "list:", "yourself"],
referencedClasses: []
}),
smalltalk.RRemoveTask.klass);


smalltalk.addClass('RList', smalltalk.RModel, [], 'Reactive');
smalltalk.RList.comment="this are persisted lists"
smalltalk.addMethod(
smalltalk.method({
selector: "addTask:",
category: 'not yet classified',
fn: function (aRTaskId) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._tasks())._add_(aRTaskId);
return self}, function($ctx1) {$ctx1.fill(self,"addTask:",{aRTaskId:aRTaskId},smalltalk.RList)});},
args: ["aRTaskId"],
source: "addTask: aRTaskId\x0a\x0a\x09self tasks add: aRTaskId",
messageSends: ["add:", "tasks"],
referencedClasses: []
}),
smalltalk.RList);

smalltalk.addMethod(
smalltalk.method({
selector: "removeTask:",
category: 'not yet classified',
fn: function (aRTaskId) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._tasks())._remove_ifAbsent_(aRTaskId,(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"removeTask:",{aRTaskId:aRTaskId},smalltalk.RList)});},
args: ["aRTaskId"],
source: "removeTask: aRTaskId\x0a\x0a\x09self tasks remove: aRTaskId ifAbsent:[nil]",
messageSends: ["remove:ifAbsent:", "tasks"],
referencedClasses: []
}),
smalltalk.RList);

smalltalk.addMethod(
smalltalk.method({
selector: "tasks",
category: 'not yet classified',
fn: function () {
var self=this;
function $Array(){return smalltalk.Array||(typeof Array=="undefined"?nil:Array)}
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=smalltalk.RModel.fn.prototype._tasks.apply(_st(self), []);
if(($receiver = $2) == nil || $receiver == undefined){
_st(self)._tasks_(_st($Array())._new());
$1=_st(self)._tasks();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"tasks",{},smalltalk.RList)});},
args: [],
source: "tasks\x0a\x0a\x09^ super tasks ifNil:[\x0a\x09\x09\x09self tasks: Array new.\x0a\x09\x09\x09self tasks]",
messageSends: ["ifNil:", "tasks:", "new", "tasks"],
referencedClasses: ["Array"]
}),
smalltalk.RList);


smalltalk.addMethod(
smalltalk.method({
selector: "atId:do:",
category: 'not yet classified',
fn: function (anId, aBlock) {
var self=this;
function $ModelReadError(){return smalltalk.ModelReadError||(typeof ModelReadError=="undefined"?nil:ModelReadError)}
return smalltalk.withContext(function($ctx1) { 
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt(_st(_st(_st(self)._path()).__comma("/getList?id=")).__comma(anId)),_st("type").__minus_gt("GET"),_st("cache").__minus_gt(false),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAfterRead_done_(x,aBlock);
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st($ModelReadError())._signal_(_st(_st(_st("Could not read ").__comma(_st(self)._name())).__comma(":  ")).__comma(_st(x)._responseText()));
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
return _st($ModelReadError())._signal_(_st(_st(_st("Could not read ").__comma(_st(self)._name())).__comma(":  ")).__comma(_st(x)._responseText()));
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"atId:do:",{anId:anId,aBlock:aBlock},smalltalk.RList.klass)});},
args: ["anId", "aBlock"],
source: "atId: anId do: aBlock\x0a\x0a\x09jQuery ajax: #{ \x0a\x09\x09'url' -> (self path, '/getList?id=',anId).\x0a\x09\x09'type'-> 'GET'.\x0a\x09\x09'cache'-> false.\x0a\x09\x09'success'-> [:x| self onAfterRead: x done: aBlock].\x0a\x09\x09'fail' -> [:x| ModelReadError signal: 'Could not read ', self name,':  ', x responseText].\x0a\x09\x09'error'-> [:x| ModelReadError signal: 'Could not read ', self name,':  ', x responseText]}",
messageSends: ["ajax:", "->", ",", "path", "onAfterRead:done:", "signal:", "responseText", "name"],
referencedClasses: ["ModelReadError"]
}),
smalltalk.RList.klass);


smalltalk.addClass('RProspect', smalltalk.RModel, [], 'Reactive');


smalltalk.addClass('RTask', smalltalk.RModel, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
smalltalk.RModel.fn.prototype._initialize.apply(_st(self), []);
_st(self)._isCompleted_(false);
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.RTask)});},
args: [],
source: "initialize\x0a\x09super initialize.\x0a\x09\x0a\x09self isCompleted: false",
messageSends: ["initialize", "isCompleted:"],
referencedClasses: []
}),
smalltalk.RTask);



smalltalk.addClass('RUser', smalltalk.RModel, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "addList:",
category: 'not yet classified',
fn: function (aRList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._lists())._add_(aRList);
return self}, function($ctx1) {$ctx1.fill(self,"addList:",{aRList:aRList},smalltalk.RUser)});},
args: ["aRList"],
source: "addList: aRList\x0a\x0a\x09self lists add: aRList",
messageSends: ["add:", "lists"],
referencedClasses: []
}),
smalltalk.RUser);

smalltalk.addMethod(
smalltalk.method({
selector: "lists",
category: 'not yet classified',
fn: function () {
var self=this;
function $OrderedCollection(){return smalltalk.OrderedCollection||(typeof OrderedCollection=="undefined"?nil:OrderedCollection)}
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=smalltalk.RModel.fn.prototype._lists.apply(_st(self), []);
if(($receiver = $2) == nil || $receiver == undefined){
_st(self)._lists_(_st($OrderedCollection())._new());
$1=_st(self)._lists();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"lists",{},smalltalk.RUser)});},
args: [],
source: "lists\x0a\x0a\x09^ super lists ifNil:[\x0a\x09\x09\x09self lists: OrderedCollection new.\x0a\x09\x09\x09self lists]",
messageSends: ["ifNil:", "lists:", "new", "lists"],
referencedClasses: ["OrderedCollection"]
}),
smalltalk.RUser);



smalltalk.addClass('RSession', smalltalk.Object, ['id', 'user', 'api', 'social'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "api",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@api"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeAPI();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"api",{},smalltalk.RSession)});},
args: [],
source: "api\x0a\x0a\x09^ api ifNil:[self initializeAPI]",
messageSends: ["ifNil:", "initializeAPI"],
referencedClasses: []
}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "asJSONString",
category: 'not yet classified',
fn: function () {
var self=this;
function $HasedCollection(){return smalltalk.HasedCollection||(typeof HasedCollection=="undefined"?nil:HasedCollection)}
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st($HasedCollection())._new();
_st($2)._at_put_("id",_st(self)._id());
_st($2)._at_put_("userId",_st(_st(self)._user())._id());
$3=_st($2)._asJSONString();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJSONString",{},smalltalk.RSession)});},
args: [],
source: "asJSONString\x0a\x0a\x09^ HasedCollection new\x0a\x09\x09at: 'id' put: self id;\x0a\x09\x09at: 'userId' put: self user id;\x0a\x09\x09asJSONString",
messageSends: ["at:put:", "id", "new", "user", "asJSONString"],
referencedClasses: ["HasedCollection"]
}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "getUser",
category: 'not yet classified',
fn: function (){
var self=this;
var anId,guy,newUser,newList;
function $RUser(){return smalltalk.RUser||(typeof RUser=="undefined"?nil:RUser)}
function $RList(){return smalltalk.RList||(typeof RList=="undefined"?nil:RList)}
function $RAddUser(){return smalltalk.RAddUser||(typeof RAddUser=="undefined"?nil:RAddUser)}
function $RAddList(){return smalltalk.RAddList||(typeof RAddList=="undefined"?nil:RAddList)}
function $MaplessModel(){return smalltalk.MaplessModel||(typeof MaplessModel=="undefined"?nil:MaplessModel)}
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$4,$1;
anId=_st(_st(window)._localStorage())._getItem_("userId");
guy=_st(_st(window)._localStorage())._getItem_(anId);
$2=_st(guy)._isNil();
if(smalltalk.assert($2)){
newUser=_st($RUser())._new();
newUser;
$3=_st($RList())._new();
_st($3)._id_("95faef2d-ffc8-4444-d2c7-3e9bbf97c415");
_st($3)._name_("Things to get done");
$4=_st($3)._yourself();
newList=$4;
newList;
_st(newUser)._listId_(_st(newList)._id());
_st(_st(window)._localStorage())._setItem_put_("userId",_st(newUser)._id());
_st(_st($RAddUser())._for_(newUser))._execute();
_st(_st($RAddList())._for_(newList))._execute();
$1=newUser;
} else {
$1=_st($MaplessModel())._fromJsonString_(guy);
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"getUser",{anId:anId,guy:guy,newUser:newUser,newList:newList},smalltalk.RSession)})},
args: [],
source: "getUser\x0a\x09| anId guy newUser newList |\x0a\x09anId := window localStorage getItem: 'userId'.\x0a\x09guy := window localStorage getItem: anId.\x0a\x09^ guy isNil\x0a\x09\x09ifTrue:[\x0a\x09\x09\x09newUser := RUser new.\x0a\x09\x09\x09newList := RList new \x0a\x09\x09\x09\x09\x09\x09\x09\x09id: '95faef2d-ffc8-4444-d2c7-3e9bbf97c415';\x0a\x09\x09\x09\x09\x09\x09\x09\x09name: 'Things to get done'; \x0a\x09\x09\x09\x09\x09\x09\x09\x09yourself.\x0a\x09\x09\x09newUser listId: newList id.\x0a\x09\x09\x09window localStorage setItem: 'userId' put: newUser id.\x0a\x09\x09\x09(RAddUser for: newUser) execute.\x0a\x09\x09\x09(RAddList for: newList) execute.\x0a\x09\x09\x09newUser]\x0a\x09\x09ifFalse:[MaplessModel fromJsonString: guy]",
messageSends: ["getItem:", "localStorage", "ifTrue:ifFalse:", "new", "id:", "name:", "yourself", "listId:", "id", "setItem:put:", "execute", "for:", "fromJsonString:", "isNil"],
referencedClasses: ["RUser", "RList", "RAddUser", "RAddList", "MaplessModel"]
}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "id",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@id"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeID();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"id",{},smalltalk.RSession)});},
args: [],
source: "id\x0a\x0a\x09^ id ifNil:[self initializeID] ",
messageSends: ["ifNil:", "initializeID"],
referencedClasses: []
}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeAPI",
category: 'not yet classified',
fn: function () {
var self=this;
function $RClient(){return smalltalk.RClient||(typeof RClient=="undefined"?nil:RClient)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@api"]=_st($RClient())._new();
$1=self["@api"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeAPI",{},smalltalk.RSession)});},
args: [],
source: "initializeAPI\x0a\x0a\x09^ api := RClient new",
messageSends: ["new"],
referencedClasses: ["RClient"]
}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeID",
category: 'not yet classified',
fn: function () {
var self=this;
function $MaplessModel(){return smalltalk.MaplessModel||(typeof MaplessModel=="undefined"?nil:MaplessModel)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@id"]=_st($MaplessModel())._newUUID();
$1=self["@id"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeID",{},smalltalk.RSession)});},
args: [],
source: "initializeID\x0a\x0a\x09^ id := MaplessModel newUUID",
messageSends: ["newUUID"],
referencedClasses: ["MaplessModel"]
}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeSocial",
category: 'not yet classified',
fn: function () {
var self=this;
function $SocialAPI(){return smalltalk.SocialAPI||(typeof SocialAPI=="undefined"?nil:SocialAPI)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@social"]=_st($SocialAPI())._new();
$1=self["@social"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeSocial",{},smalltalk.RSession)});},
args: [],
source: "initializeSocial\x0a\x0a\x09^ social := SocialAPI new",
messageSends: ["new"],
referencedClasses: ["SocialAPI"]
}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeUser",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@user"]=_st(self)._getUser();
$1=self["@user"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeUser",{},smalltalk.RSession)});},
args: [],
source: "initializeUser\x0a\x0a\x09^ user := self getUser",
messageSends: ["getUser"],
referencedClasses: []
}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "onOpen",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
_st(self)._initializeID();
$1=_st(_st(self)._social())._facebook();
_st($1)._appId_("318516538288855");
_st($1)._channelUrl_("http://tasks.flowingconcept.com/service/static/facebook/channel.html");
$2=_st($1)._yourself();
_st(_st(_st(self)._social())._linkedIn())._appId_("cekl05jzyc9p");
_st(_st(_st(self)._social())._googlePlus())._appId_("durable-ripsaw-381");
return self}, function($ctx1) {$ctx1.fill(self,"onOpen",{},smalltalk.RSession)});},
args: [],
source: "onOpen\x0a\x0a\x09self initializeID.\x0a\x09\x0a\x09self social facebook \x0a\x09\x09appId: '318516538288855';\x0a\x09\x09channelUrl: 'http://tasks.flowingconcept.com/service/static/facebook/channel.html';\x0a\x09\x09yourself.\x0a\x09\x09\x0a\x09self social linkedIn appId: 'cekl05jzyc9p'.\x0a\x09self social googlePlus appId: 'durable-ripsaw-381'.\x0a\x09",
messageSends: ["initializeID", "appId:", "facebook", "social", "channelUrl:", "yourself", "linkedIn", "googlePlus"],
referencedClasses: []
}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "social",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@social"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeSocial();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"social",{},smalltalk.RSession)});},
args: [],
source: "social\x0a\x0a\x09^ social ifNil:[self initializeSocial]",
messageSends: ["ifNil:", "initializeSocial"],
referencedClasses: []
}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "user",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@user"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeUser();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"user",{},smalltalk.RSession)});},
args: [],
source: "user\x0a\x0a\x09^ user ifNil:[ self initializeUser ]",
messageSends: ["ifNil:", "initializeUser"],
referencedClasses: []
}),
smalltalk.RSession);



smalltalk.addClass('RStorage', smalltalk.Object, [], 'Reactive');

smalltalk.addMethod(
smalltalk.method({
selector: "delete:",
category: 'not yet classified',
fn: function (aMaplessModel) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aMaplessModel)._localDelete();
return self}, function($ctx1) {$ctx1.fill(self,"delete:",{aMaplessModel:aMaplessModel},smalltalk.RStorage.klass)});},
args: ["aMaplessModel"],
source: "delete: aMaplessModel\x0a\x0a\x09aMaplessModel localDelete",
messageSends: ["localDelete"],
referencedClasses: []
}),
smalltalk.RStorage.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "delete:do:",
category: 'not yet classified',
fn: function (aMaplessModel, aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aMaplessModel)._remoteDeleteDo_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(aMaplessModel)._localDelete();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"delete:do:",{aMaplessModel:aMaplessModel,aBlock:aBlock},smalltalk.RStorage.klass)});},
args: ["aMaplessModel", "aBlock"],
source: "delete: aMaplessModel do: aBlock\x0a\x09\x22Deletes remotely, then (and if successful) locally aMaplessModel\x22\x0a\x09\x0a\x09aMaplessModel remoteDeleteDo: [aMaplessModel localDelete]",
messageSends: ["remoteDeleteDo:", "localDelete"],
referencedClasses: []
}),
smalltalk.RStorage.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "save:",
category: 'not yet classified',
fn: function (aMaplessModel) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aMaplessModel)._localSave();
return self}, function($ctx1) {$ctx1.fill(self,"save:",{aMaplessModel:aMaplessModel},smalltalk.RStorage.klass)});},
args: ["aMaplessModel"],
source: "save: aMaplessModel\x0a\x0a\x09aMaplessModel localSave",
messageSends: ["localSave"],
referencedClasses: []
}),
smalltalk.RStorage.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "save:do:",
category: 'not yet classified',
fn: function (aMaplessModel, aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=aMaplessModel;
_st($1)._localSave();
$2=_st($1)._remoteSaveDo_(aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"save:do:",{aMaplessModel:aMaplessModel,aBlock:aBlock},smalltalk.RStorage.klass)});},
args: ["aMaplessModel", "aBlock"],
source: "save: aMaplessModel do: aBlock\x0a\x09\x22Saves locally, then remotely aMaplessModel\x22\x0a\x09\x0a\x09aMaplessModel \x0a\x09\x09localSave;\x0a\x09\x09remoteSaveDo: aBlock",
messageSends: ["localSave", "remoteSaveDo:"],
referencedClasses: []
}),
smalltalk.RStorage.klass);


smalltalk.addClass('RWidget', smalltalk.Widget, ['model', 'announcer'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "announce:",
category: 'not yet classified',
fn: function (anAnnouncement) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._announcer())._announce_(anAnnouncement);
return $1;
}, function($ctx1) {$ctx1.fill(self,"announce:",{anAnnouncement:anAnnouncement},smalltalk.RWidget)});},
args: ["anAnnouncement"],
source: "announce: anAnnouncement\x0a\x0a\x09^ self announcer announce: anAnnouncement",
messageSends: ["announce:", "announcer"],
referencedClasses: []
}),
smalltalk.RWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "announcer",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@announcer"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeAnnouncer();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"announcer",{},smalltalk.RWidget)});},
args: [],
source: "announcer\x0a\x0a\x09^ announcer ifNil:[self initializeAnnouncer]",
messageSends: ["ifNil:", "initializeAnnouncer"],
referencedClasses: []
}),
smalltalk.RWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeAnnouncer",
category: 'not yet classified',
fn: function () {
var self=this;
function $Announcer(){return smalltalk.Announcer||(typeof Announcer=="undefined"?nil:Announcer)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@announcer"]=_st($Announcer())._new();
$1=self["@announcer"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeAnnouncer",{},smalltalk.RWidget)});},
args: [],
source: "initializeAnnouncer\x0a\x0a\x09^ announcer := Announcer new",
messageSends: ["new"],
referencedClasses: ["Announcer"]
}),
smalltalk.RWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "model",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@model"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"model",{},smalltalk.RWidget)});},
args: [],
source: "model\x0a\x0a\x09^ model",
messageSends: [],
referencedClasses: []
}),
smalltalk.RWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "model:",
category: 'not yet classified',
fn: function (aModel) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@model"]=aModel;
return self}, function($ctx1) {$ctx1.fill(self,"model:",{aModel:aModel},smalltalk.RWidget)});},
args: ["aModel"],
source: "model: aModel\x0a\x0a\x09model := aModel",
messageSends: [],
referencedClasses: []
}),
smalltalk.RWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "on:do:",
category: 'not yet classified',
fn: function (anAnnouncement, aReaction) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._announcer())._on_do_(anAnnouncement,aReaction);
return $1;
}, function($ctx1) {$ctx1.fill(self,"on:do:",{anAnnouncement:anAnnouncement,aReaction:aReaction},smalltalk.RWidget)});},
args: ["anAnnouncement", "aReaction"],
source: "on: anAnnouncement do: aReaction\x0a\x0a\x09^ self announcer on: anAnnouncement do: aReaction",
messageSends: ["on:do:", "announcer"],
referencedClasses: []
}),
smalltalk.RWidget);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
category: 'not yet classified',
fn: function (aModel) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st(self)._new();
_st($2)._model_(aModel);
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"for:",{aModel:aModel},smalltalk.RWidget.klass)});},
args: ["aModel"],
source: "for: aModel\x0a\x0a\x09^ self new\x0a\x09\x09model: aModel;\x0a\x09\x09yourself",
messageSends: ["model:", "new", "yourself"],
referencedClasses: []
}),
smalltalk.RWidget.klass);


smalltalk.addClass('ListWidget', smalltalk.RWidget, ['list', 'input', 'items'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "addTask",
category: 'not yet classified',
fn: function () {
var self=this;
var newTask,newTaskWidget;
function $RTask(){return smalltalk.RTask||(typeof RTask=="undefined"?nil:RTask)}
function $RAddTask(){return smalltalk.RAddTask||(typeof RAddTask=="undefined"?nil:RAddTask)}
function $TaskWidget(){return smalltalk.TaskWidget||(typeof TaskWidget=="undefined"?nil:TaskWidget)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st($RTask())._new();
_st($1)._initializeCreatedOn();
_st($1)._description_(_st(_st(self["@input"])._asJQuery())._val());
$2=_st($1)._yourself();
newTask=$2;
_st(_st($RAddTask())._for_in_(newTask,self["@model"]))._execute();
newTaskWidget=_st($TaskWidget())._for_(newTask);
_st(_st(self)._items())._add_(newTaskWidget);
_st(self)._renderTask_(newTaskWidget);
_st(self)._updateTasksMetric();
return self}, function($ctx1) {$ctx1.fill(self,"addTask",{newTask:newTask,newTaskWidget:newTaskWidget},smalltalk.ListWidget)});},
args: [],
source: "addTask\x0a\x0a\x09| newTask newTaskWidget |\x0a\x0a\x09newTask := RTask new\x0a\x09\x09initializeCreatedOn;\x0a\x09\x09description: input asJQuery val;\x0a\x09\x09yourself.\x0a\x09\x0a\x09(RAddTask for: newTask in: model) execute.\x0a\x0a\x09newTaskWidget := TaskWidget for: newTask.\x0a\x09\x0a\x09self items add: newTaskWidget.\x0a\x09self renderTask: newTaskWidget.\x0a\x09\x0a\x09self updateTasksMetric",
messageSends: ["initializeCreatedOn", "new", "description:", "val", "asJQuery", "yourself", "execute", "for:in:", "for:", "add:", "items", "renderTask:", "updateTasksMetric"],
referencedClasses: ["RTask", "RAddTask", "TaskWidget"]
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "addTask:",
category: 'not yet classified',
fn: function (anAddTask) {
var self=this;
var newTaskWidget;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(anAddTask)._saveAndUpdateList_(self["@model"]);
newTaskWidget=_st(self)._newTaskFor_(_st(anAddTask)._task());
_st(_st(self)._items())._add_(newTaskWidget);
_st(self)._renderTask_(newTaskWidget);
_st(newTaskWidget)._hide();
$1=_st(self)._isAllOrToDo();
if(smalltalk.assert($1)){
_st(newTaskWidget)._show();
};
_st(self)._updateTasksMetric();
return self}, function($ctx1) {$ctx1.fill(self,"addTask:",{anAddTask:anAddTask,newTaskWidget:newTaskWidget},smalltalk.ListWidget)});},
args: ["anAddTask"],
source: "addTask: anAddTask\x0a\x0a\x09| newTaskWidget |\x0a\x09\x0a\x09anAddTask saveAndUpdateList: model.\x0a\x0a\x09newTaskWidget := self newTaskFor: anAddTask task.\x0a\x09\x0a\x09self items add: newTaskWidget.\x0a\x09self renderTask: newTaskWidget.\x0a\x0a\x09newTaskWidget hide.\x09\x0a\x09self isAllOrToDo ifTrue:[\x0a\x09\x09newTaskWidget show].\x0a\x09\x09\x0a\x09self updateTasksMetric\x09\x09",
messageSends: ["saveAndUpdateList:", "newTaskFor:", "task", "add:", "items", "renderTask:", "hide", "ifTrue:", "show", "isAllOrToDo", "updateTasksMetric"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "changeList:",
category: 'not yet classified',
fn: function (aChageList) {
var self=this;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
return smalltalk.withContext(function($ctx1) { 
_st(self)._model_(_st(aChageList)._list());
_st(_st(_st(self["@list"])._asJQuery())._children_("li"))._remove();
_st(self)._initializeItems();
_st(self)._renderItemsOn_(_st($HTMLCanvas())._onJQuery_(_st(self["@list"])._asJQuery()));
return self}, function($ctx1) {$ctx1.fill(self,"changeList:",{aChageList:aChageList},smalltalk.ListWidget)});},
args: ["aChageList"],
source: "changeList: aChageList\x0a\x0a\x09self model: aChageList list.\x0a\x09(list asJQuery children: 'li') remove.\x0a\x09self initializeItems.\x0a\x09self renderItemsOn: (HTMLCanvas onJQuery: list asJQuery)\x0a\x09",
messageSends: ["model:", "list", "remove", "children:", "asJQuery", "initializeItems", "renderItemsOn:", "onJQuery:"],
referencedClasses: ["HTMLCanvas"]
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "changeTask:",
category: 'not yet classified',
fn: function (aChangeTask) {
var self=this;
var changedTask;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
changedTask=_st(self)._getTaskWidgetFor_(_st(aChangeTask)._task());
$1=changedTask;
if(($receiver = $1) == nil || $receiver == undefined){
$1;
} else {
_st(changedTask)._changedTask_(_st(aChangeTask)._task());
};
$2=_st(self)._isDone();
if(smalltalk.assert($2)){
_st(changedTask)._showIfDone();
};
$3=_st(self)._isToDo();
if(smalltalk.assert($3)){
_st(changedTask)._showIfToDo();
};
_st(self)._updateTasksMetric();
return self}, function($ctx1) {$ctx1.fill(self,"changeTask:",{aChangeTask:aChangeTask,changedTask:changedTask},smalltalk.ListWidget)});},
args: ["aChangeTask"],
source: "changeTask: aChangeTask\x0a\x0a\x09| changedTask |\x0a\x0a\x09changedTask := self getTaskWidgetFor: aChangeTask task.\x0a\x09changedTask ifNotNil:[\x0a\x09\x09changedTask changedTask: aChangeTask task].\x0a\x09\x09\x0a\x09self isDone ifTrue:[\x0a\x09\x09changedTask showIfDone].\x0a\x0a\x09self isToDo ifTrue:[\x0a\x09\x09changedTask showIfToDo].\x0a\x09\x09\x0a\x09self updateTasksMetric\x09\x0a\x09\x09",
messageSends: ["getTaskWidgetFor:", "task", "ifNotNil:", "changedTask:", "ifTrue:", "showIfDone", "isDone", "showIfToDo", "isToDo", "updateTasksMetric"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "currentTaskIds",
category: 'not yet classified',
fn: function () {
var self=this;
var children,elements;
function $OrderedCollection(){return smalltalk.OrderedCollection||(typeof OrderedCollection=="undefined"?nil:OrderedCollection)}
return smalltalk.withContext(function($ctx1) { 
var $1;
children=_st(_st(self["@list"])._asJQuery())._children();
elements=_st($OrderedCollection())._new();
_st((0))._to_do_(_st(_st(children)._length()).__minus((1)),(function(i){
return smalltalk.withContext(function($ctx2) {
return _st(elements)._add_(_st(_st(children)._at_(i))._id());
}, function($ctx2) {$ctx2.fillBlock({i:i},$ctx1)})}));
$1=elements;
return $1;
}, function($ctx1) {$ctx1.fill(self,"currentTaskIds",{children:children,elements:elements},smalltalk.ListWidget)});},
args: [],
source: "currentTaskIds\x0a\x09| children elements |\x0a\x09children := list asJQuery children.\x0a\x09elements := OrderedCollection new.\x0a\x090 to: children length -1 do:[:i|\x0a\x09\x09elements add: (children at: i) id].\x0a\x09\x09\x0a\x09^ elements\x0a\x09",
messageSends: ["children", "asJQuery", "new", "to:do:", "-", "length", "add:", "id", "at:"],
referencedClasses: ["OrderedCollection"]
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getAll",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._items();
return $1;
}, function($ctx1) {$ctx1.fill(self,"getAll",{},smalltalk.ListWidget)});},
args: [],
source: "getAll\x0a\x09\x09\x0a\x09\x09^ self items",
messageSends: ["items"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getDone",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._getAll())._select_((function(e){
return smalltalk.withContext(function($ctx2) {
return _st(e)._isCompleted();
}, function($ctx2) {$ctx2.fillBlock({e:e},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"getDone",{},smalltalk.ListWidget)});},
args: [],
source: "getDone\x0a\x09\x09\x0a\x09\x09^ self getAll select:[:e| e isCompleted]",
messageSends: ["select:", "isCompleted", "getAll"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getTaskAt:",
category: 'not yet classified',
fn: function (anId) {
var self=this;
function $MaplessModel(){return smalltalk.MaplessModel||(typeof MaplessModel=="undefined"?nil:MaplessModel)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($MaplessModel())._localLoadAt_(anId);
return $1;
}, function($ctx1) {$ctx1.fill(self,"getTaskAt:",{anId:anId},smalltalk.ListWidget)});},
args: ["anId"],
source: "getTaskAt: anId\x0a\x0a\x09^ MaplessModel localLoadAt: anId",
messageSends: ["localLoadAt:"],
referencedClasses: ["MaplessModel"]
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getTaskWidgetFor:",
category: 'not yet classified',
fn: function (aTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._items())._detect_ifNone_((function(e){
return smalltalk.withContext(function($ctx2) {
return _st(_st(_st(e)._model())._id()).__eq(_st(aTask)._id());
}, function($ctx2) {$ctx2.fillBlock({e:e},$ctx1)})}),(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"getTaskWidgetFor:",{aTask:aTask},smalltalk.ListWidget)});},
args: ["aTask"],
source: "getTaskWidgetFor: aTask\x0a\x0a\x09^ self items \x0a\x09\x09detect:[:e| e model id = aTask id ]\x0a\x09\x09ifNone:[nil]",
messageSends: ["detect:ifNone:", "=", "id", "model", "items"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getTasks",
category: 'not yet classified',
fn: function () {
var self=this;
function $JSON(){return smalltalk.JSON||(typeof JSON=="undefined"?nil:JSON)}
function $RTask(){return smalltalk.RTask||(typeof RTask=="undefined"?nil:RTask)}
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$1=_st(_st(self["@model"])._tasks())._collect_((function(each){
return smalltalk.withContext(function($ctx2) {
$2=_st(each)._isString();
if(smalltalk.assert($2)){
return _st(self)._getTaskAt_(each);
} else {
return _st($RTask())._fromJsonString_(_st($JSON())._stringify_(each));
};
}, function($ctx2) {$ctx2.fillBlock({each:each},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"getTasks",{},smalltalk.ListWidget)});},
args: [],
source: "getTasks\x0a\x0a\x09^ model tasks collect:[:each|\x0a\x09\x09each isString\x0a\x09\x09\x09ifTrue:[self getTaskAt: each]\x0a\x09\x09\x09ifFalse:[RTask fromJsonString: (JSON stringify: each) ]]",
messageSends: ["collect:", "ifTrue:ifFalse:", "getTaskAt:", "fromJsonString:", "stringify:", "isString", "tasks"],
referencedClasses: ["JSON", "RTask"]
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getToDo",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._getAll())._reject_((function(e){
return smalltalk.withContext(function($ctx2) {
return _st(e)._isCompleted();
}, function($ctx2) {$ctx2.fillBlock({e:e},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"getToDo",{},smalltalk.ListWidget)});},
args: [],
source: "getToDo\x0a\x09\x09\x0a\x09\x09^ self getAll reject:[:e| e isCompleted]",
messageSends: ["reject:", "isCompleted", "getAll"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeItems",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@items"]=_st(self)._makeItems();
$1=self["@items"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeItems",{},smalltalk.ListWidget)});},
args: [],
source: "initializeItems\x0a\x0a\x09^ items := self makeItems",
messageSends: ["makeItems"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isAll",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st("#all")._asJQuery())._is_(".selectedTab");
return $1;
}, function($ctx1) {$ctx1.fill(self,"isAll",{},smalltalk.ListWidget)});},
args: [],
source: "isAll\x0a\x0a\x09^ '#all' asJQuery is:'.selectedTab'",
messageSends: ["is:", "asJQuery"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isAllOrToDo",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._isAll())._or_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._isToDo();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"isAllOrToDo",{},smalltalk.ListWidget)});},
args: [],
source: "isAllOrToDo\x0a\x09^ self isAll or:[\x0a\x09self isToDo]",
messageSends: ["or:", "isToDo", "isAll"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isDone",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st("#done")._asJQuery())._is_(".selectedTab");
return $1;
}, function($ctx1) {$ctx1.fill(self,"isDone",{},smalltalk.ListWidget)});},
args: [],
source: "isDone\x0a\x0a\x09^ '#done' asJQuery is:'.selectedTab'",
messageSends: ["is:", "asJQuery"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isToDo",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st("#todo")._asJQuery())._is_(".selectedTab");
return $1;
}, function($ctx1) {$ctx1.fill(self,"isToDo",{},smalltalk.ListWidget)});},
args: [],
source: "isToDo\x0a\x0a\x09^ '#todo' asJQuery is:'.selectedTab'",
messageSends: ["is:", "asJQuery"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "items",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@items"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeItems();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"items",{},smalltalk.ListWidget)});},
args: [],
source: "items\x0a\x0a\x09^ items ifNil:[self initializeItems]",
messageSends: ["ifNil:", "initializeItems"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "makeItems",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._getTasks())._collect_((function(e){
return smalltalk.withContext(function($ctx2) {
return _st(self)._newTaskFor_(e);
}, function($ctx2) {$ctx2.fillBlock({e:e},$ctx1)})}));
return $1;
}, function($ctx1) {$ctx1.fill(self,"makeItems",{},smalltalk.ListWidget)});},
args: [],
source: "makeItems\x0a\x0a\x09^ self getTasks collect:[:e|\x0a\x09\x09self newTaskFor: e]",
messageSends: ["collect:", "newTaskFor:", "getTasks"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "newTaskFor:",
category: 'not yet classified',
fn: function (aTaskModel) {
var self=this;
function $RTaskChanged(){return smalltalk.RTaskChanged||(typeof RTaskChanged=="undefined"?nil:RTaskChanged)}
function $TaskWidget(){return smalltalk.TaskWidget||(typeof TaskWidget=="undefined"?nil:TaskWidget)}
function $RTaskRemoved(){return smalltalk.RTaskRemoved||(typeof RTaskRemoved=="undefined"?nil:RTaskRemoved)}
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
$2=_st($TaskWidget())._for_(aTaskModel);
_st($2)._on_do_($RTaskChanged(),(function(ann){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onTaskChanged_(ann);
}, function($ctx2) {$ctx2.fillBlock({ann:ann},$ctx1)})}));
_st($2)._on_do_($RTaskRemoved(),(function(ann){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onTaskRemove_(ann);
}, function($ctx2) {$ctx2.fillBlock({ann:ann},$ctx1)})}));
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"newTaskFor:",{aTaskModel:aTaskModel},smalltalk.ListWidget)});},
args: ["aTaskModel"],
source: "newTaskFor: aTaskModel\x0a\x0a\x09^ (TaskWidget for: aTaskModel)\x0a\x09\x09on: RTaskChanged do:[:ann| self onTaskChanged: ann];\x0a\x09\x09on: RTaskRemoved do:[:ann| self onTaskRemove: ann];\x0a\x09\x09yourself",
messageSends: ["on:do:", "onTaskChanged:", "for:", "onTaskRemove:", "yourself"],
referencedClasses: ["RTaskChanged", "TaskWidget", "RTaskRemoved"]
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onAdd",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(_st(self["@input"])._asJQuery())._val())._isNil())._or_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(_st(self["@input"])._asJQuery())._val())._isEmpty();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
if(! smalltalk.assert($1)){
_st(self)._addTask();
_st(_st(self["@input"])._asJQuery())._val_("");
_st(_st(self["@input"])._asJQuery())._focus();
};
return self}, function($ctx1) {$ctx1.fill(self,"onAdd",{},smalltalk.ListWidget)});},
args: [],
source: "onAdd\x0a\x0a\x09(input asJQuery val isNil or:[\x0a\x09input asJQuery val isEmpty]) ifFalse:[\x0a\x09\x09self addTask.\x0a\x09\x09input asJQuery val: ''.\x0a\x09\x09input asJQuery focus]",
messageSends: ["ifFalse:", "addTask", "val:", "asJQuery", "focus", "or:", "isEmpty", "val", "isNil"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onAllTasks",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._items())._do_((function(e){
return smalltalk.withContext(function($ctx2) {
return _st(e)._show();
}, function($ctx2) {$ctx2.fillBlock({e:e},$ctx1)})}));
_st(self)._unselectTabs();
_st(_st("#all")._asJQuery())._addClass_("selectedTab");
_st(self)._updateTasksMetric();
return self}, function($ctx1) {$ctx1.fill(self,"onAllTasks",{},smalltalk.ListWidget)});},
args: [],
source: "onAllTasks\x0a\x0a\x09self items do:[:e| e show].\x0a\x09self unselectTabs.\x0a\x09'#all' asJQuery addClass: 'selectedTab'.\x0a\x09\x0a\x09self updateTasksMetric",
messageSends: ["do:", "show", "items", "unselectTabs", "addClass:", "asJQuery", "updateTasksMetric"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onDoneTasks",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._items())._do_((function(e){
return smalltalk.withContext(function($ctx2) {
return _st(e)._showIfDone();
}, function($ctx2) {$ctx2.fillBlock({e:e},$ctx1)})}));
_st(self)._unselectTabs();
_st(_st("#done")._asJQuery())._addClass_("selectedTab");
_st(self)._updateTasksMetric();
return self}, function($ctx1) {$ctx1.fill(self,"onDoneTasks",{},smalltalk.ListWidget)});},
args: [],
source: "onDoneTasks\x0a\x09\x0a\x09self items do:[:e| e showIfDone].\x0a\x09self unselectTabs.\x0a\x09'#done' asJQuery addClass: 'selectedTab'.\x0a\x09\x0a\x09self updateTasksMetric",
messageSends: ["do:", "showIfDone", "items", "unselectTabs", "addClass:", "asJQuery", "updateTasksMetric"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onKeyUp:",
category: 'not yet classified',
fn: function (e) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(e)._keyCode()).__eq((13));
if(smalltalk.assert($1)){
_st(self)._onAdd();
};
return self}, function($ctx1) {$ctx1.fill(self,"onKeyUp:",{e:e},smalltalk.ListWidget)});},
args: ["e"],
source: "onKeyUp: e\x0a\x0a\x09e keyCode = 13 ifTrue:[\x0a\x09\x09self onAdd]",
messageSends: ["ifTrue:", "onAdd", "=", "keyCode"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onSorted:",
category: 'not yet classified',
fn: function (anEvent) {
var self=this;
function $RChangeList(){return smalltalk.RChangeList||(typeof RChangeList=="undefined"?nil:RChangeList)}
function $RTasksSorted(){return smalltalk.RTasksSorted||(typeof RTasksSorted=="undefined"?nil:RTasksSorted)}
return smalltalk.withContext(function($ctx1) { 
_st(self["@model"])._tasks_(_st(self)._currentTaskIds());
_st(_st($RChangeList())._for_(self["@model"]))._execute();
_st(self)._announce_(_st($RTasksSorted())._for_(self));
return self}, function($ctx1) {$ctx1.fill(self,"onSorted:",{anEvent:anEvent},smalltalk.ListWidget)});},
args: ["anEvent"],
source: "onSorted: anEvent\x0a\x0a\x09model tasks: self currentTaskIds.\x0a\x09(RChangeList for: model) execute.\x0a\x09\x0a\x09self announce: (RTasksSorted for: self)",
messageSends: ["tasks:", "currentTaskIds", "execute", "for:", "announce:"],
referencedClasses: ["RChangeList", "RTasksSorted"]
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onTaskChanged:",
category: 'not yet classified',
fn: function (ann) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._updateTasksMetric();
return self}, function($ctx1) {$ctx1.fill(self,"onTaskChanged:",{ann:ann},smalltalk.ListWidget)});},
args: ["ann"],
source: "onTaskChanged: ann\x0a\x0a\x09self updateTasksMetric",
messageSends: ["updateTasksMetric"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onTaskRemove:",
category: 'not yet classified',
fn: function (ann) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self["@model"])._removeTask_(_st(_st(_st(ann)._subject())._model())._id());
_st(self["@model"])._localSave();
return self}, function($ctx1) {$ctx1.fill(self,"onTaskRemove:",{ann:ann},smalltalk.ListWidget)});},
args: ["ann"],
source: "onTaskRemove: ann\x0a\x0a\x09model removeTask: ann subject model id.\x0a\x09model localSave.\x0a\x09",
messageSends: ["removeTask:", "id", "model", "subject", "localSave"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onToDoTasks",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._items())._do_((function(e){
return smalltalk.withContext(function($ctx2) {
return _st(e)._showIfToDo();
}, function($ctx2) {$ctx2.fillBlock({e:e},$ctx1)})}));
_st(self)._unselectTabs();
_st(_st("#todo")._asJQuery())._addClass_("selectedTab");
_st(self)._updateTasksMetric();
return self}, function($ctx1) {$ctx1.fill(self,"onToDoTasks",{},smalltalk.ListWidget)});},
args: [],
source: "onToDoTasks\x0a\x09\x0a\x09self items do:[:e| e showIfToDo].\x0a\x09self unselectTabs.\x0a\x09'#todo' asJQuery addClass: 'selectedTab'.\x0a\x09self updateTasksMetric",
messageSends: ["do:", "showIfToDo", "items", "unselectTabs", "addClass:", "asJQuery", "updateTasksMetric"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "removeTask:",
category: 'not yet classified',
fn: function (aRemoveTask) {
var self=this;
var removedTask;
return smalltalk.withContext(function($ctx1) { 
var $1;
removedTask=_st(self)._getTaskWidgetFor_(_st(aRemoveTask)._task());
_st(aRemoveTask)._removeAndUpdateList_(self["@model"]);
$1=removedTask;
if(($receiver = $1) == nil || $receiver == undefined){
$1;
} else {
_st(removedTask)._remove();
_st(_st(self)._items())._remove_ifAbsent_(removedTask,(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
};
_st(self)._updateTasksMetric();
return self}, function($ctx1) {$ctx1.fill(self,"removeTask:",{aRemoveTask:aRemoveTask,removedTask:removedTask},smalltalk.ListWidget)});},
args: ["aRemoveTask"],
source: "removeTask: aRemoveTask\x0a\x0a\x09| removedTask |\x0a\x0a\x09removedTask := self getTaskWidgetFor: aRemoveTask task.\x0a\x09aRemoveTask removeAndUpdateList: model.\x0a\x09removedTask ifNotNil:[ \x0a\x09\x09removedTask remove.\x0a\x09\x09self items remove: removedTask ifAbsent:[nil]].\x0a\x09self updateTasksMetric\x09\x09",
messageSends: ["getTaskWidgetFor:", "task", "removeAndUpdateList:", "ifNotNil:", "remove", "remove:ifAbsent:", "items", "updateTasksMetric"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "render",
category: 'not yet classified',
fn: function () {
var self=this;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
return smalltalk.withContext(function($ctx1) { 
_st(self)._renderOn_(_st($HTMLCanvas())._onJQuery_(_st("#wrapper")._asJQuery()));
_st(_st("#wrapper")._asJQuery())._css_val_("min-height","50px");
return self}, function($ctx1) {$ctx1.fill(self,"render",{},smalltalk.ListWidget)});},
args: [],
source: "render\x0a\x0a\x09self renderOn: (HTMLCanvas onJQuery: '#wrapper' asJQuery).\x0a\x09\x0a\x09'#wrapper' asJQuery css: 'min-height' val: '50px'",
messageSends: ["renderOn:", "onJQuery:", "asJQuery", "css:val:"],
referencedClasses: ["HTMLCanvas"]
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderAddTaskOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$5,$6,$7,$8,$4,$2;
$1=_st(html)._div();
_st($1)._class_("addTaskWrapper");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._div();
_st($3)._id_("addTask");
$4=_st($3)._with_((function(){
return smalltalk.withContext(function($ctx3) {
$5=_st(html)._input();
_st($5)._at_put_("placeholder","Something else to get done?");
_st($5)._onKeyUp_((function(e){
return smalltalk.withContext(function($ctx4) {
return _st(self)._onKeyUp_(e);
}, function($ctx4) {$ctx4.fillBlock({e:e},$ctx1)})}));
$6=_st($5)._yourself();
self["@input"]=$6;
self["@input"];
$7=_st(html)._a();
_st($7)._id_("addTaskButton");
_st($7)._onClick_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(self)._onAdd();
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$8=_st($7)._with_("+");
return $8;
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
return $4;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderAddTaskOn:",{html:html},smalltalk.ListWidget)});},
args: ["html"],
source: "renderAddTaskOn: html\x0a\x0a\x09html div\x0a\x09\x09class: 'addTaskWrapper';\x0a\x09\x09with:[\x0a\x0a\x09html div\x0a\x09\x09id: 'addTask';\x0a\x09\x09with:[\x0a\x09\x09\x09input := html input\x0a\x09\x09\x09\x09at: 'placeholder' put: 'Something else to get done?';\x0a\x09\x09\x09\x09onKeyUp:[:e| self onKeyUp: e];\x0a\x09\x09\x09\x09yourself.\x0a\x09\x09\x0a\x09\x09\x09html a\x0a\x09\x09\x09\x09id: 'addTaskButton';\x0a\x09\x09\x09\x09onClick:[self onAdd];\x0a\x09\x09\x09\x09with: '+']]\x0a\x09\x09",
messageSends: ["class:", "div", "with:", "id:", "at:put:", "input", "onKeyUp:", "yourself", "a", "onClick:", "onAdd"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderItemsOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
_st(self["@list"])._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(self)._items())._do_((function(e){
return smalltalk.withContext(function($ctx3) {
_st(e)._renderOn_(html);
$1=_st(self)._isDone();
if(smalltalk.assert($1)){
_st(e)._showIfDone();
};
$2=_st(self)._isToDo();
if(smalltalk.assert($2)){
return _st(e)._showIfToDo();
};
}, function($ctx3) {$ctx3.fillBlock({e:e},$ctx1)})}));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderItemsOn:",{html:html},smalltalk.ListWidget)});},
args: ["html"],
source: "renderItemsOn: html\x0a\x0a\x09list with:[\x0a\x09\x09self items do:[:e| \x0a\x09\x09\x09e renderOn: html.\x0a\x0a\x09\x09\x09self isDone ifTrue:[\x0a\x09\x09\x09\x09e showIfDone].\x0a\x09\x09\x09self isToDo ifTrue:[\x0a\x09\x09\x09\x09e showIfToDo]]]\x0a\x09\x09",
messageSends: ["with:", "do:", "renderOn:", "ifTrue:", "showIfDone", "isDone", "showIfToDo", "isToDo", "items"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderListHeaderOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$4,$5,$6,$2;
$1=_st(html)._div();
_st($1)._id_("listHeader");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._div();
_st($3)._id_("listName");
$4=_st($3)._with_(_st(self["@model"])._name());
$4;
$5=_st(html)._div();
_st($5)._id_("details");
$6=_st($5)._with_((function(){
return smalltalk.withContext(function($ctx3) {
_st(self)._renderMetricsOn_(html);
return _st(self)._renderTabsOn_(html);
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
return $6;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderListHeaderOn:",{html:html},smalltalk.ListWidget)});},
args: ["html"],
source: "renderListHeaderOn: html\x0a\x0a\x09html div \x0a\x09\x09id:'listHeader';\x0a\x09\x09with:[\x0a\x09\x09\x09html div id: 'listName';with: model name.\x0a\x09\x09\x09html div\x0a\x09\x09\x09\x09id:'details';\x0a\x09\x09\x09\x09with:[\x0a\x09\x09\x09\x09\x09self renderMetricsOn: html.\x0a\x09\x09\x09\x09\x09self renderTabsOn: html]].",
messageSends: ["id:", "div", "with:", "name", "renderMetricsOn:", "renderTabsOn:"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderListOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@list"]=_st(html)._ul();
_st(_st(self["@list"])._asJQuery())._disableSelection();
_st(_st(self["@list"])._asJQuery())._sortable_(smalltalk.HashedCollection._fromPairs_([_st("update").__minus_gt((function(e,ui){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onSorted_(e);
}, function($ctx2) {$ctx2.fillBlock({e:e,ui:ui},$ctx1)})}))]));
_st(self)._renderItemsOn_(html);
return self}, function($ctx1) {$ctx1.fill(self,"renderListOn:",{html:html},smalltalk.ListWidget)});},
args: ["html"],
source: "renderListOn: html\x0a\x0a\x09list := html ul.\x0a\x09list asJQuery disableSelection.\x0a\x09list asJQuery sortable: #{\x0a\x09\x09'update' -> [:e :ui| self onSorted: e]}.\x0a\x09self renderItemsOn: html.\x0a\x09\x0a\x09\x09",
messageSends: ["ul", "disableSelection", "asJQuery", "sortable:", "->", "onSorted:", "renderItemsOn:"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderMetricsOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$4,$5,$6,$2;
$1=_st(html)._div();
_st($1)._id_("metrics");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._span();
_st($3)._id_("tasks");
$4=_st($3)._with_("x tasks");
$4;
$5=_st(html)._span();
_st($5)._id_("clients");
$6=_st($5)._with_("1 guy (you)");
return $6;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderMetricsOn:",{html:html},smalltalk.ListWidget)});},
args: ["html"],
source: "renderMetricsOn: html\x0a\x0a\x09html div\x0a\x09\x09id: 'metrics';\x0a\x09\x09with:[\x0a\x09\x09\x09html span id: 'tasks'; with: 'x tasks'.\x0a\x09\x09\x09html span id: 'clients'; with: '1 guy (you)'.\x0a\x09\x09\x09].\x0a\x09\x09\x09",
messageSends: ["id:", "div", "with:", "span"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(html)._div();
_st($1)._id_("listWrapper");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._renderListHeaderOn_(html);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st(self)._renderListOn_(html);
_st(self)._renderAddTaskOn_(html);
_st(self)._onAllTasks();
return self}, function($ctx1) {$ctx1.fill(self,"renderOn:",{html:html},smalltalk.ListWidget)});},
args: ["html"],
source: "renderOn: html\x0a\x0a\x09html div\x0a\x09\x09id: 'listWrapper';\x0a\x09\x09with:[\x0a\x09\x09\x09self renderListHeaderOn: html].\x0a\x0a\x09self renderListOn: html.\x0a\x09self renderAddTaskOn: html.\x0a\x09\x0a\x09self onAllTasks",
messageSends: ["id:", "div", "with:", "renderListHeaderOn:", "renderListOn:", "renderAddTaskOn:", "onAllTasks"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderTabsOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4,$5,$6;
$1=_st(html)._div();
_st($1)._id_("done");
_st($1)._class_("tab");
_st($1)._onClick_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onDoneTasks();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(html)._span_("Done");
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$3=_st(html)._div();
_st($3)._id_("todo");
_st($3)._class_("tab");
_st($3)._onClick_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onToDoTasks();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$4=_st($3)._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(html)._span_("To-Do");
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$5=_st(html)._div();
_st($5)._id_("all");
_st($5)._onClick_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onAllTasks();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st($5)._class_("tab");
$6=_st($5)._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(html)._span_("All");
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderTabsOn:",{html:html},smalltalk.ListWidget)});},
args: ["html"],
source: "renderTabsOn: html\x0a\x0a\x09html div\x0a\x09\x09id: 'done';\x0a\x09\x09class: 'tab';\x0a\x09\x09onClick:[self onDoneTasks];\x0a\x09\x09with:[html span: 'Done'].\x0a\x0a\x09html div\x0a\x09\x09id: 'todo';\x0a\x09\x09class: 'tab';\x0a\x09\x09onClick:[self onToDoTasks];\x0a\x09\x09with:[html span: 'To-Do'].\x0a\x09\x09\x0a\x09html div\x0a\x09\x09id: 'all';\x0a\x09\x09onClick:[self onAllTasks];\x0a\x09\x09class: 'tab';\x0a\x09\x09with:[html span: 'All'].\x0a\x09\x09",
messageSends: ["id:", "div", "class:", "onClick:", "onDoneTasks", "with:", "span:", "onToDoTasks", "onAllTasks"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderTask:",
category: 'not yet classified',
fn: function (aTaskWidget) {
var self=this;
var html;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
return smalltalk.withContext(function($ctx1) { 
html=_st($HTMLCanvas())._onJQuery_(_st(self["@list"])._asJQuery());
_st(aTaskWidget)._renderOn_(html);
return self}, function($ctx1) {$ctx1.fill(self,"renderTask:",{aTaskWidget:aTaskWidget,html:html},smalltalk.ListWidget)});},
args: ["aTaskWidget"],
source: "renderTask: aTaskWidget\x0a\x09| html |\x0a\x0a\x09html := HTMLCanvas onJQuery: list asJQuery.\x0a\x09aTaskWidget renderOn: html",
messageSends: ["onJQuery:", "asJQuery", "renderOn:"],
referencedClasses: ["HTMLCanvas"]
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "unselectTabs",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(_st("#details")._asJQuery())._find_(".tab"))._removeClass_("selectedTab");
return self}, function($ctx1) {$ctx1.fill(self,"unselectTabs",{},smalltalk.ListWidget)});},
args: [],
source: "unselectTabs\x0a\x0a\x09('#details' asJQuery find:'.tab') removeClass: 'selectedTab'",
messageSends: ["removeClass:", "find:", "asJQuery"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateAllTasksMetric",
category: 'not yet classified',
fn: function () {
var self=this;
var many;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
many=_st(_st(self)._getAll())._size();
$1=_st(many)._isZero();
if(smalltalk.assert($1)){
$2=_st(_st("#tasks")._asJQuery())._text_("Life is too short for no doing it!");
return $2;
};
$3=_st(many).__gt((1));
if(smalltalk.assert($3)){
_st(_st("#tasks")._asJQuery())._text_(_st(_st(many)._asString()).__comma(" tasks total"));
} else {
_st(_st("#tasks")._asJQuery())._text_("only 1 task!");
};
return self}, function($ctx1) {$ctx1.fill(self,"updateAllTasksMetric",{many:many},smalltalk.ListWidget)});},
args: [],
source: "updateAllTasksMetric\x0a\x0a\x09| many |\x0a\x0a\x09many := self getAll size.\x0a\x09\x0a\x09many isZero ifTrue:[\x0a\x09\x09^ '#tasks' asJQuery text:  'Life is too short for no doing it!'].\x0a\x09\x09\x0a\x09many > 1\x0a\x09\x09ifTrue:[ '#tasks' asJQuery text: many asString, ' tasks total']\x0a\x09\x09ifFalse:[ '#tasks' asJQuery text:  'only 1 task!' ]",
messageSends: ["size", "getAll", "ifTrue:", "text:", "asJQuery", "isZero", "ifTrue:ifFalse:", ",", "asString", ">"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateDoneTasksMetric",
category: 'not yet classified',
fn: function () {
var self=this;
var many;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4,$5;
many=_st(_st(self)._getDone())._size();
$1=_st(many)._isZero();
if(smalltalk.assert($1)){
$2=_st(_st("#tasks")._asJQuery())._text_("Come on! let's ship something!");
return $2;
};
$3=_st(_st(_st(self)._getAll())._size()).__eq(many);
if(smalltalk.assert($3)){
$4=_st(_st("#tasks")._asJQuery())._text_(_st(_st(many)._asString()).__comma(" tasks done, OMG it's all done!"));
return $4;
};
$5=_st(many).__gt((1));
if(smalltalk.assert($5)){
_st(_st("#tasks")._asJQuery())._text_(_st(_st(many)._asString()).__comma(" tasks done"));
} else {
_st(_st("#tasks")._asJQuery())._text_("only 1 task done");
};
return self}, function($ctx1) {$ctx1.fill(self,"updateDoneTasksMetric",{many:many},smalltalk.ListWidget)});},
args: [],
source: "updateDoneTasksMetric\x0a\x0a\x09| many |\x0a\x0a\x09many := self getDone size.\x0a\x09\x0a\x09many isZero ifTrue:[\x0a\x09\x09^ '#tasks' asJQuery text:  'Come on! let''s ship something!'].\x0a\x0a\x09self getAll size = many ifTrue:[\x0a\x09\x09^ '#tasks' asJQuery text:  many asString, ' tasks done, OMG it''s all done!'].\x0a\x0a\x09many > 1\x0a\x09\x09ifTrue:[ '#tasks' asJQuery text: many asString, ' tasks done']\x0a\x09\x09ifFalse:[ '#tasks' asJQuery text:  'only 1 task done' ]",
messageSends: ["size", "getDone", "ifTrue:", "text:", "asJQuery", "isZero", ",", "asString", "=", "getAll", "ifTrue:ifFalse:", ">"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateTasksMetric",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4,$5,$6;
$1=_st(self)._isDone();
if(smalltalk.assert($1)){
$2=_st(self)._updateDoneTasksMetric();
return $2;
};
$3=_st(self)._isToDo();
if(smalltalk.assert($3)){
$4=_st(self)._updateToDoTasksMetric();
return $4;
};
$5=_st(self)._isAll();
if(smalltalk.assert($5)){
$6=_st(self)._updateAllTasksMetric();
return $6;
};
return self}, function($ctx1) {$ctx1.fill(self,"updateTasksMetric",{},smalltalk.ListWidget)});},
args: [],
source: "updateTasksMetric\x0a\x0a\x09self isDone ifTrue:[\x0a\x09\x09^ self updateDoneTasksMetric].\x0a\x0a\x09self isToDo ifTrue:[\x0a\x09\x09^ self updateToDoTasksMetric].\x0a\x0a\x09self isAll ifTrue:[\x0a\x09\x09^ self updateAllTasksMetric].",
messageSends: ["ifTrue:", "updateDoneTasksMetric", "isDone", "updateToDoTasksMetric", "isToDo", "updateAllTasksMetric", "isAll"],
referencedClasses: []
}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateToDoTasksMetric",
category: 'not yet classified',
fn: function () {
var self=this;
var many;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
many=_st(_st(self)._getToDo())._size();
$1=_st(many)._isZero();
if(smalltalk.assert($1)){
$2=_st(_st("#tasks")._asJQuery())._text_("Let's do something?");
return $2;
};
$3=_st(many).__gt((1));
if(smalltalk.assert($3)){
_st(_st("#tasks")._asJQuery())._text_(_st(_st(many)._asString()).__comma(" tasks to do"));
} else {
_st(_st("#tasks")._asJQuery())._text_("only 1 task to do");
};
return self}, function($ctx1) {$ctx1.fill(self,"updateToDoTasksMetric",{many:many},smalltalk.ListWidget)});},
args: [],
source: "updateToDoTasksMetric\x0a\x0a\x09| many |\x0a\x0a\x09many := self getToDo size.\x0a\x0a\x09many isZero ifTrue:[\x0a\x09\x09^ '#tasks' asJQuery text:  'Let''s do something?'].\x0a\x09\x0a\x09many > 1\x0a\x09\x09ifTrue:[ '#tasks' asJQuery text: many asString, ' tasks to do']\x0a\x09\x09ifFalse:[ '#tasks' asJQuery text:  'only 1 task to do' ]",
messageSends: ["size", "getToDo", "ifTrue:", "text:", "asJQuery", "isZero", "ifTrue:ifFalse:", ",", "asString", ">"],
referencedClasses: []
}),
smalltalk.ListWidget);



smalltalk.addClass('ReactiveWidget', smalltalk.RWidget, ['list', 'session'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "addTask:",
category: 'not yet classified',
fn: function (anAddTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._list())._addTask_(anAddTask);
return self}, function($ctx1) {$ctx1.fill(self,"addTask:",{anAddTask:anAddTask},smalltalk.ReactiveWidget)});},
args: ["anAddTask"],
source: "addTask: anAddTask\x0a\x0a\x09self list addTask: anAddTask",
messageSends: ["addTask:", "list"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "changeClients:",
category: 'not yet classified',
fn: function (aChangeClients) {
var self=this;
var clients;
return smalltalk.withContext(function($ctx1) { 
var $1;
clients=_st(aChangeClients)._clients();
$1=_st(clients).__gt((1));
if(smalltalk.assert($1)){
_st(_st("#clients")._asJQuery())._text_(_st(_st(clients)._asString()).__comma(" guys"));
} else {
_st(_st("#clients")._asJQuery())._text_("1 guy (you)");
};
_st(_st("#clients")._asJQuery())._effect_op_duration_("pulsate",smalltalk.HashedCollection._fromPairs_([_st("times").__minus_gt((3))]),(500));
return self}, function($ctx1) {$ctx1.fill(self,"changeClients:",{aChangeClients:aChangeClients,clients:clients},smalltalk.ReactiveWidget)});},
args: ["aChangeClients"],
source: "changeClients: aChangeClients\x0a\x0a\x09| clients |\x0a\x09\x0a\x09clients := aChangeClients clients.\x0a\x09clients > 1\x0a\x09\x09ifTrue:[ '#clients'asJQuery text: clients asString, ' guys']\x0a\x09\x09ifFalse:[ '#clients'asJQuery text:  '1 guy (you)' ].\x0a\x0a\x09'#clients'asJQuery effect: 'pulsate' op: #{'times'->3} duration: 500",
messageSends: ["clients", "ifTrue:ifFalse:", "text:", ",", "asString", "asJQuery", ">", "effect:op:duration:", "->"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "changeList:",
category: 'not yet classified',
fn: function (aChageList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._list())._changeList_(aChageList);
return self}, function($ctx1) {$ctx1.fill(self,"changeList:",{aChageList:aChageList},smalltalk.ReactiveWidget)});},
args: ["aChageList"],
source: "changeList: aChageList\x0a\x0a\x09self list changeList: aChageList",
messageSends: ["changeList:", "list"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "changeTask:",
category: 'not yet classified',
fn: function (aChangeTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._list())._changeTask_(aChangeTask);
return self}, function($ctx1) {$ctx1.fill(self,"changeTask:",{aChangeTask:aChangeTask},smalltalk.ReactiveWidget)});},
args: ["aChangeTask"],
source: "changeTask: aChangeTask\x0a\x0a\x09self list changeTask: aChangeTask",
messageSends: ["changeTask:", "list"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getListAt:",
category: 'not yet classified',
fn: function (anId) {
var self=this;
function $MaplessModel(){return smalltalk.MaplessModel||(typeof MaplessModel=="undefined"?nil:MaplessModel)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($MaplessModel())._localLoadAt_(anId);
return $1;
}, function($ctx1) {$ctx1.fill(self,"getListAt:",{anId:anId},smalltalk.ReactiveWidget)});},
args: ["anId"],
source: "getListAt: anId\x0a\x0a\x09^ MaplessModel localLoadAt: anId",
messageSends: ["localLoadAt:"],
referencedClasses: ["MaplessModel"]
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "hasTip",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(window)._localStorage())._getItem_("showTipAtStart"))._isNil();
return $1;
}, function($ctx1) {$ctx1.fill(self,"hasTip",{},smalltalk.ReactiveWidget)});},
args: [],
source: "hasTip\x0a\x0a\x09^ (window localStorage getItem: 'showTipAtStart') isNil",
messageSends: ["isNil", "getItem:", "localStorage"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeListDo:",
category: 'not yet classified',
fn: function (aBlock) {
var self=this;
function $ListWidget(){return smalltalk.ListWidget||(typeof ListWidget=="undefined"?nil:ListWidget)}
function $JSON(){return smalltalk.JSON||(typeof JSON=="undefined"?nil:JSON)}
function $RTask(){return smalltalk.RTask||(typeof RTask=="undefined"?nil:RTask)}
function $RList(){return smalltalk.RList||(typeof RList=="undefined"?nil:RList)}
function $RTasksSorted(){return smalltalk.RTasksSorted||(typeof RTasksSorted=="undefined"?nil:RTasksSorted)}
function $RTaskRemoved(){return smalltalk.RTaskRemoved||(typeof RTaskRemoved=="undefined"?nil:RTaskRemoved)}
return smalltalk.withContext(function($ctx1) { 
var $2,$3,$1;
self["@list"]=_st($ListWidget())._new();
_st($RList())._atId_do_("95faef2d-ffc8-4444-d2c7-3e9bbf97c415",(function(aList){
return smalltalk.withContext(function($ctx2) {
_st(self["@list"])._model_(aList);
_st(_st(aList)._tasks())._do_((function(each){
return smalltalk.withContext(function($ctx3) {
return _st(_st($RTask())._fromJsonString_(_st($JSON())._stringify_(each)))._localSave();
}, function($ctx3) {$ctx3.fillBlock({each:each},$ctx1)})}));
return _st(aBlock)._value();
}, function($ctx2) {$ctx2.fillBlock({aList:aList},$ctx1)})}));
$2=self["@list"];
_st($2)._on_do_($RTasksSorted(),(function(ann){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onListChanged_(ann);
}, function($ctx2) {$ctx2.fillBlock({ann:ann},$ctx1)})}));
_st($2)._on_do_($RTaskRemoved(),(function(ann){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onTaskRemoved_(ann);
}, function($ctx2) {$ctx2.fillBlock({ann:ann},$ctx1)})}));
$3=_st($2)._yourself();
$1=$3;
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeListDo:",{aBlock:aBlock},smalltalk.ReactiveWidget)});},
args: ["aBlock"],
source: "initializeListDo: aBlock \x0a\x0a\x09list := ListWidget new.\x0a\x09\x0a\x09RList \x0a\x09\x09\x22atId: model listId \x22\x0a\x09\x09atId: '95faef2d-ffc8-4444-d2c7-3e9bbf97c415'\x0a\x09\x09do: [:aList | \x0a\x09\x09\x09\x09list model: aList.\x0a\x09\x09\x09\x09aList tasks do:[:each| (RTask fromJsonString: (JSON stringify: each)) localSave].\x0a\x09\x09\x09\x09aBlock value].\x0a\x0a\x09^ list\x0a\x09\x09on: RTasksSorted do:[:ann| self onListChanged: ann];\x0a\x09\x09on: RTaskRemoved do:[:ann| self onTaskRemoved: ann];\x0a\x09\x09yourself.",
messageSends: ["new", "atId:do:", "model:", "do:", "localSave", "fromJsonString:", "stringify:", "tasks", "value", "on:do:", "onListChanged:", "onTaskRemoved:", "yourself"],
referencedClasses: ["ListWidget", "JSON", "RTask", "RList", "RTasksSorted", "RTaskRemoved"]
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeSession",
category: 'not yet classified',
fn: function () {
var self=this;
function $RSession(){return smalltalk.RSession||(typeof RSession=="undefined"?nil:RSession)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@session"]=_st($RSession())._new();
$1=self["@session"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeSession",{},smalltalk.ReactiveWidget)});},
args: [],
source: "initializeSession\x0a\x0a\x09^ session := RSession new",
messageSends: ["new"],
referencedClasses: ["RSession"]
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isLocalhost",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(window)._location())._hostname())._match_("localhost");
return $1;
}, function($ctx1) {$ctx1.fill(self,"isLocalhost",{},smalltalk.ReactiveWidget)});},
args: [],
source: "isLocalhost\x0a\x0a\x09^ window location hostname match: 'localhost'",
messageSends: ["match:", "hostname", "location"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isProduction",
category: 'not yet classified',
fn: function () {
var self=this;
function $Browser(){return smalltalk.Browser||(typeof Browser=="undefined"?nil:Browser)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($Browser())._isNil();
return $1;
}, function($ctx1) {$ctx1.fill(self,"isProduction",{},smalltalk.ReactiveWidget)});},
args: [],
source: "isProduction\x0a\x0a\x09^ Browser isNil",
messageSends: ["isNil"],
referencedClasses: ["Browser"]
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isPublished",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._isLocalhost())._not();
return $1;
}, function($ctx1) {$ctx1.fill(self,"isPublished",{},smalltalk.ReactiveWidget)});},
args: [],
source: "isPublished\x0a\x0a\x09^ self isLocalhost not",
messageSends: ["not", "isLocalhost"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isSociallyLoggedIn",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
return false;
}, function($ctx1) {$ctx1.fill(self,"isSociallyLoggedIn",{},smalltalk.ReactiveWidget)});},
args: [],
source: "isSociallyLoggedIn\x0a\x0a\x09^ false",
messageSends: [],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "list",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@list"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"list",{},smalltalk.ReactiveWidget)});},
args: [],
source: "list \x0a\x0a\x09^ list ",
messageSends: [],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "listDo:",
category: 'not yet classified',
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@list"];
$1=_st($2)._ifNil_ifNotNil_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._initializeListDo_(aBlock);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}),aBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"listDo:",{aBlock:aBlock},smalltalk.ReactiveWidget)});},
args: ["aBlock"],
source: "listDo: aBlock \x0a\x0a\x09^ list \x0a\x09\x09ifNil:[self initializeListDo: aBlock]\x0a\x09\x09ifNotNil: aBlock",
messageSends: ["ifNil:ifNotNil:", "initializeListDo:"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "loadFacebookSDK",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
(function(d, s, id){
     var js, fjs = d.getElementsByTagName(s)[0];
     if (d.getElementById(id)) {return;}
     js = d.createElement(s); js.id = id;
     js.src = "//connect.facebook.net/en_US/all.js";
     fjs.parentNode.insertBefore(js, fjs);
   }(document, 'script', 'facebook-jssdk'));;
return self}, function($ctx1) {$ctx1.fill(self,"loadFacebookSDK",{},smalltalk.ReactiveWidget)});},
args: [],
source: "loadFacebookSDK\x0a<(function(d, s, id){\x0a     var js, fjs = d.getElementsByTagName(s)[0];\x0a     if (d.getElementById(id)) {return;}\x0a     js = d.createElement(s); js.id = id;\x0a     js.src = \x22//connect.facebook.net/en_US/all.js\x22;\x0a     fjs.parentNode.insertBefore(js, fjs);\x0a   }(document, 'script', 'facebook-jssdk'));>",
messageSends: [],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onGotIt",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._localStorage())._setItem_put_("showTipAtStart",false);
return self}, function($ctx1) {$ctx1.fill(self,"onGotIt",{},smalltalk.ReactiveWidget)});},
args: [],
source: "onGotIt\x0a\x0a\x09window localStorage setItem: 'showTipAtStart' put: false",
messageSends: ["setItem:put:", "localStorage"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onListChanged:",
category: 'not yet classified',
fn: function (ann) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self}, function($ctx1) {$ctx1.fill(self,"onListChanged:",{ann:ann},smalltalk.ReactiveWidget)});},
args: ["ann"],
source: "onListChanged: ann\x0a\x0a\x09\x22should send the command to the server\x22",
messageSends: [],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onNay",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._asJQuery())._scrollTop_((0));
_st(_st("#nayWrapper")._asJQuery())._slideDown_(smalltalk.HashedCollection._fromPairs_([_st("complete").__minus_gt((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(_st("#feedback")._asJQuery())._find_("textarea"))._focus();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"onNay",{},smalltalk.ReactiveWidget)});},
args: [],
source: "onNay\x0a\x0a\x09window asJQuery scrollTop: 0.\x0a\x09'#nayWrapper' asJQuery slideDown:#{'complete'->[('#feedback' asJQuery find: 'textarea') focus]}.",
messageSends: ["scrollTop:", "asJQuery", "slideDown:", "->", "focus", "find:"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onNayKeyUp:",
category: 'not yet classified',
fn: function (e) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(_st(e)._keyCode()).__eq((27));
if(smalltalk.assert($1)){
_st(_st("#nayWrapper")._asJQuery())._slideUp();
};
$2=_st(_st(e)._keyCode()).__eq((13));
if(smalltalk.assert($2)){
_st(self)._onOkNay();
};
return self}, function($ctx1) {$ctx1.fill(self,"onNayKeyUp:",{e:e},smalltalk.ReactiveWidget)});},
args: ["e"],
source: "onNayKeyUp: e\x0a\x0a\x09e keyCode = 27 ifTrue:['#nayWrapper' asJQuery slideUp.].\x0a\x0a\x09e keyCode = 13 ifTrue:[\x0a\x09\x09self onOkNay].",
messageSends: ["ifTrue:", "slideUp", "asJQuery", "=", "keyCode", "onOkNay"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onNayTextAreaKeyUp:",
category: 'not yet classified',
fn: function (e) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(e)._keyCode()).__eq((27));
if(smalltalk.assert($1)){
_st(_st("#nayWrapper")._asJQuery())._slideUp();
};
return self}, function($ctx1) {$ctx1.fill(self,"onNayTextAreaKeyUp:",{e:e},smalltalk.ReactiveWidget)});},
args: ["e"],
source: "onNayTextAreaKeyUp: e\x0a\x0a\x09e keyCode = 27 ifTrue:['#nayWrapper' asJQuery slideUp.].",
messageSends: ["ifTrue:", "slideUp", "asJQuery", "=", "keyCode"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onOkNay",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st("#nayWrapper")._asJQuery())._slideUp();
_st(self)._sendFeedback();
return self}, function($ctx1) {$ctx1.fill(self,"onOkNay",{},smalltalk.ReactiveWidget)});},
args: [],
source: "onOkNay\x0a\x0a\x09'#nayWrapper' asJQuery slideUp.\x0a\x09\x0a\x09self sendFeedback",
messageSends: ["slideUp", "asJQuery", "sendFeedback"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onOkYay",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st("#yayWrapper")._asJQuery())._slideUp();
_st(self)._sendEmail();
return self}, function($ctx1) {$ctx1.fill(self,"onOkYay",{},smalltalk.ReactiveWidget)});},
args: [],
source: "onOkYay\x0a\x0a\x09'#yayWrapper' asJQuery slideUp.\x0a\x09\x0a\x09self sendEmail",
messageSends: ["slideUp", "asJQuery", "sendEmail"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onOpen",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st("#loader")._asJQuery())._remove();
_st(_st(window)._document())._title_("Let's do this!");
_st(window)._at_put_("app",self);
_st(_st(self)._session())._onOpen();
_st(self)._model_(_st(_st(self)._session())._user());
return self}, function($ctx1) {$ctx1.fill(self,"onOpen",{},smalltalk.ReactiveWidget)});},
args: [],
source: "onOpen\x0a\x0a\x09'#loader' asJQuery remove.\x0a\x09window document title: 'Let''s do this!'.\x0a\x09\x0a\x09window at: 'app' put: self.\x0a\x09self session onOpen.\x0a\x09self model: self session user.",
messageSends: ["remove", "asJQuery", "title:", "document", "at:put:", "onOpen", "session", "model:", "user"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onTaskRemoved:",
category: 'not yet classified',
fn: function (ann) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self}, function($ctx1) {$ctx1.fill(self,"onTaskRemoved:",{ann:ann},smalltalk.ReactiveWidget)});},
args: ["ann"],
source: "onTaskRemoved: ann\x0a\x0a\x09\x22should send the command to the server\x22",
messageSends: [],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onYay",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._asJQuery())._scrollTop_((0));
_st(_st("#yayWrapper")._asJQuery())._slideDown_(smalltalk.HashedCollection._fromPairs_([_st("complete").__minus_gt((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st("#yayEmail")._asJQuery())._focus();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"onYay",{},smalltalk.ReactiveWidget)});},
args: [],
source: "onYay\x0a\x0a\x09window asJQuery scrollTop: 0.\x0a\x09'#yayWrapper' asJQuery slideDown: #{'complete'->['#yayEmail' asJQuery focus]}.",
messageSends: ["scrollTop:", "asJQuery", "slideDown:", "->", "focus"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onYayKeyUp:",
category: 'not yet classified',
fn: function (e) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(_st(e)._keyCode()).__eq((27));
if(smalltalk.assert($1)){
_st(_st("#yayWrapper")._asJQuery())._slideUp();
};
$2=_st(_st(e)._keyCode()).__eq((13));
if(smalltalk.assert($2)){
_st(self)._onOkYay();
};
return self}, function($ctx1) {$ctx1.fill(self,"onYayKeyUp:",{e:e},smalltalk.ReactiveWidget)});},
args: ["e"],
source: "onYayKeyUp: e\x0a\x0a\x09e keyCode = 27 ifTrue:['#yayWrapper' asJQuery slideUp.].\x0a\x0a\x09e keyCode = 13 ifTrue:[\x0a\x09\x09self onOkYay].",
messageSends: ["ifTrue:", "slideUp", "asJQuery", "=", "keyCode", "onOkYay"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "open",
category: 'not yet classified',
fn: function () {
var self=this;
var html;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
function $RChangeClients(){return smalltalk.RChangeClients||(typeof RChangeClients=="undefined"?nil:RChangeClients)}
return smalltalk.withContext(function($ctx1) { 
html=_st($HTMLCanvas())._onJQuery_(_st("body")._asJQuery());
_st(self)._onOpen();
_st(_st(_st(self)._session())._api())._onOpen_((function(){
return smalltalk.withContext(function($ctx2) {
_st(self)._renderOn_(html);
return _st((function(){
return smalltalk.withContext(function($ctx3) {
return _st(_st($RChangeClients())._new())._execute();
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}))._valueWithTimeout_((100));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st(_st(_st(self)._session())._api())._open();
return self}, function($ctx1) {$ctx1.fill(self,"open",{html:html},smalltalk.ReactiveWidget)});},
args: [],
source: "open\x0a\x09| html |\x0a\x09\x0a\x09html := HTMLCanvas onJQuery: 'body' asJQuery.\x0a\x09\x0a\x09self onOpen.\x0a\x09self session api onOpen:[ \x0a\x09\x09self renderOn: html.\x0a\x09\x09[RChangeClients new execute] valueWithTimeout: 100 ].\x0a\x09self session api open\x09\x0a\x09",
messageSends: ["onJQuery:", "asJQuery", "onOpen", "onOpen:", "renderOn:", "valueWithTimeout:", "execute", "new", "api", "session", "open"],
referencedClasses: ["HTMLCanvas", "RChangeClients"]
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "openBrowser",
category: 'not yet classified',
fn: function () {
var self=this;
function $Browser(){return smalltalk.Browser||(typeof Browser=="undefined"?nil:Browser)}
return smalltalk.withContext(function($ctx1) { 
_st($Browser())._open();
return self}, function($ctx1) {$ctx1.fill(self,"openBrowser",{},smalltalk.ReactiveWidget)});},
args: [],
source: "openBrowser\x0a\x0a\x09Browser open  ",
messageSends: ["open"],
referencedClasses: ["Browser"]
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "removeTask:",
category: 'not yet classified',
fn: function (aRemoveTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._list())._removeTask_(aRemoveTask);
return self}, function($ctx1) {$ctx1.fill(self,"removeTask:",{aRemoveTask:aRemoveTask},smalltalk.ReactiveWidget)});},
args: ["aRemoveTask"],
source: "removeTask: aRemoveTask\x0a\x0a\x09self list removeTask: aRemoveTask",
messageSends: ["removeTask:", "list"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderAmberOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
var canvas;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
canvas=_st($HTMLCanvas())._onJQuery_(_st("#header")._asJQuery());
$1=_st(canvas)._a();
_st($1)._id_("ide");
_st($1)._onClick_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._openBrowser();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$2=_st($1)._with_("IDE");
return self}, function($ctx1) {$ctx1.fill(self,"renderAmberOn:",{html:html,canvas:canvas},smalltalk.ReactiveWidget)});},
args: ["html"],
source: "renderAmberOn: html\x0a\x0a\x09| canvas |\x0a\x09\x0a\x09canvas := HTMLCanvas onJQuery: '#header' asJQuery.\x0a\x09\x0a\x09canvas a\x0a\x09\x09id: 'ide';\x0a\x09\x09onClick:[self openBrowser];\x0a\x09\x09with: 'IDE'",
messageSends: ["onJQuery:", "asJQuery", "id:", "a", "onClick:", "openBrowser", "with:"],
referencedClasses: ["HTMLCanvas"]
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderFacebookLikeOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(html)._div();
_st($1)._class_("fb-like");
_st($1)._at_put_("data-send","true");
_st($1)._at_put_("data-width","450");
_st($1)._at_put_("data-show-faces","true");
$2=_st($1)._yourself();
return self}, function($ctx1) {$ctx1.fill(self,"renderFacebookLikeOn:",{html:html},smalltalk.ReactiveWidget)});},
args: ["html"],
source: "renderFacebookLikeOn: html\x0a\x09\x0a\x09html div\x0a\x09\x09class: 'fb-like';\x0a\x09\x09at: 'data-send' put: 'true';\x0a\x09\x09at: 'data-width' put: '450';\x0a\x09\x09at: 'data-show-faces' put: 'true';\x0a\x09\x09yourself.\x0a\x09\x09\x0a\x09\x09",
messageSends: ["class:", "div", "at:put:", "yourself"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderFooterOn:",
category: 'not yet classified',
fn: function (html){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$4,$5,$7,$8,$6,$9,$11,$12,$10,$13,$15,$16,$17,$18,$14,$19,$20,$21,$22,$2;
$1=_st(html)._div();
_st($1)._id_("footer");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
_st(self)._renderSocialLoginOn_(html);
_st(self)._renderLikeOn_(html);
$3=_st(html)._img();
_st($3)._class_("vignette");
$4=_st($3)._src_("static/img/vignette.png");
$4;
$5=_st(html)._p();
_st($5)._class_("bgText");
$6=_st($5)._with_((function(){
return smalltalk.withContext(function($ctx3) {
_st(html)._span_("Written by ");
$7=_st(html)._a();
_st($7)._href_("http://www.linkedin.com/profile/view?id=46055105");
_st($7)._target_("_blank");
$8=_st($7)._with_("Sebastian Sastre");
return $8;
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
$6;
$9=_st(html)._p();
_st($9)._class_("bgText");
$10=_st($9)._with_((function(){
return smalltalk.withContext(function($ctx3) {
_st(html)._span_(" author of ");
$11=_st(html)._a();
_st($11)._class_("bgText");
_st($11)._href_("http://airflowing.com");
_st($11)._target_("_blank");
$12=_st($11)._with_("airflowing");
return $12;
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
$10;
$13=_st(html)._p();
_st($13)._class_("bgText");
$14=_st($13)._with_((function(){
return smalltalk.withContext(function($ctx3) {
$15=_st(html)._a();
_st($15)._href_("http://amber-lang.net");
_st($15)._target_("_blank");
$16=_st($15)._with_("Amber");
$16;
_st(html)._span_(" frontend | ");
$17=_st(html)._a();
_st($17)._href_("http://www.pharo-project.org/");
_st($17)._target_("_blank");
$18=_st($17)._with_("Pharo");
$18;
return _st(html)._span_(" backend");
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
$14;
$19=_st(html)._a();
_st($19)._class_("bgText");
_st($19)._href_("http://flowingconcept.com");
_st($19)._target_("_blank");
$20=_st($19)._with_("flowing");
$20;
$21=_st(html)._p();
_st($21)._class_("bgText");
$22=_st($21)._with_("October 2013");
return $22;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderFooterOn:",{html:html},smalltalk.ReactiveWidget)})},
args: ["html"],
source: "renderFooterOn: html\x0a\x0a\x09html div\x0a\x09\x09id: 'footer';\x0a\x09\x09with:[\x0a\x09\x09\x09self renderSocialLoginOn: html.\x0a\x09\x09\x09self renderLikeOn: html.\x0a\x09\x09\x09html img\x0a\x09\x09\x09\x09class: 'vignette';\x0a\x09\x09\x09\x09src: 'static/img/vignette.png'.\x0a\x09\x09\x09html p\x0a\x09\x09\x09\x09class: 'bgText';\x0a\x09\x09\x09\x09with:[\x0a\x09\x09\x09\x09\x09html span: 'Written by '.\x0a\x09\x09\x09\x09\x09html a\x0a\x09\x09\x09\x09\x09\x09href: 'http://www.linkedin.com/profile/view?id=46055105';\x0a\x09\x09\x09\x09\x09\x09target:'_blank';\x0a\x09\x09\x09\x09\x09\x09with: 'Sebastian Sastre'].\x0a\x09\x09html p\x0a\x09\x09\x09class: 'bgText';\x0a\x09\x09\x09with:[\x0a\x09\x09\x09\x09html span: ' author of '.\x0a\x09\x09\x09\x09html a\x0a\x09\x09\x09\x09\x09class: 'bgText';\x0a\x09\x09\x09\x09\x09href: 'http://airflowing.com';\x0a\x09\x09\x09\x09\x09target:'_blank';\x0a\x09\x09\x09\x09\x09with: 'airflowing'.\x0a\x09\x09\x09].\x0a\x0a\x09\x09html p\x0a\x09\x09\x09\x09class: 'bgText';\x0a\x09\x09\x09\x09with:[\x0a\x09\x09\x09\x09\x09html a\x0a\x09\x09\x09\x09\x09\x09href: 'http://amber-lang.net';\x0a\x09\x09\x09\x09\x09\x09target:'_blank';\x0a\x09\x09\x09\x09\x09\x09with: 'Amber'.\x0a\x09\x09\x09\x09\x09html span: ' frontend | '.\x0a\x09\x09\x09\x09\x09html a\x0a\x09\x09\x09\x09\x09\x09href: 'http://www.pharo-project.org/';\x0a\x09\x09\x09\x09\x09\x09target:'_blank';\x0a\x09\x09\x09\x09\x09\x09with: 'Pharo'.\x0a\x09\x09\x09\x09\x09html span: ' backend'].\x0a\x09\x09\x09html a\x0a\x09\x09\x09\x09class: 'bgText';\x0a\x09\x09\x09\x09href: 'http://flowingconcept.com';\x0a\x09\x09\x09\x09target:'_blank';\x0a\x09\x09\x09\x09with: 'flowing'.\x0a\x09\x09html p\x0a\x09\x09\x09\x09class: 'bgText';\x0a\x09\x09\x09\x09with: 'October 2013'.\x0a\x09\x09]",
messageSends: ["id:", "div", "with:", "renderSocialLoginOn:", "renderLikeOn:", "class:", "img", "src:", "p", "span:", "href:", "a", "target:"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderHeaderOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
$1=_st(self)._isProduction();
if(smalltalk.assert($1)){
return nil;
};
$2=_st(html)._div();
_st($2)._id_("header");
$3=_st($2)._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._renderAmberOn_(html);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderHeaderOn:",{html:html},smalltalk.ReactiveWidget)});},
args: ["html"],
source: "renderHeaderOn: html\x0a\x0a\x09self isProduction ifTrue:[^nil].\x0a\x09\x0a\x09html div\x0a\x09\x09id: 'header';\x0a\x09\x09with:[\x0a\x09\x09\x09self renderAmberOn: html.\x0a\x09\x09\x09]",
messageSends: ["ifTrue:", "isProduction", "id:", "div", "with:", "renderAmberOn:"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderLikeOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$4,$5,$6,$2;
$1=_st(html)._p();
_st($1)._class_("bgText");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
_st(html)._span_("Like ♥?");
$3=_st(html)._a();
_st($3)._class_("yayNay");
_st($3)._onClick_((function(){
return smalltalk.withContext(function($ctx3) {
return _st(self)._onYay();
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
$4=_st($3)._with_("Yay!");
$4;
$5=_st(html)._a();
_st($5)._class_("yayNay");
_st($5)._onClick_((function(){
return smalltalk.withContext(function($ctx3) {
return _st(self)._onNay();
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
$6=_st($5)._with_("Nay!");
return $6;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderLikeOn:",{html:html},smalltalk.ReactiveWidget)});},
args: ["html"],
source: "renderLikeOn: html\x0a\x0a\x09html p\x0a\x09\x09class: 'bgText';\x0a\x09\x09\x09with:[\x0a\x09\x09\x09\x09html span: 'Like ♥?'.\x0a\x09\x09\x09\x09html a\x0a\x09\x09\x09\x09\x09class: 'yayNay';\x0a\x09\x09\x09\x09\x09onClick:[self onYay];\x0a\x09\x09\x09\x09\x09with: 'Yay!'.\x0a\x09\x09\x09\x09html a\x0a\x09\x09\x09\x09\x09class: 'yayNay';\x0a\x09\x09\x09\x09\x09onClick:[self onNay];\x0a\x09\x09\x09\x09\x09with: 'Nay!']",
messageSends: ["class:", "p", "with:", "span:", "a", "onClick:", "onYay", "onNay"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderListFooterOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self}, function($ctx1) {$ctx1.fill(self,"renderListFooterOn:",{html:html},smalltalk.ReactiveWidget)});},
args: ["html"],
source: "renderListFooterOn: html\x0a\x0a\x09\x22html div\x0a\x09\x09id: 'listFooter';\x0a\x09\x09with:[html span: 'things'].\x22",
messageSends: [],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderListOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(html)._div();
_st($1)._id_("listLoaderBar");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(html)._img())._src_("static/img/loaderBar.gif");
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st(self)._listDo_((function(){
return smalltalk.withContext(function($ctx2) {
_st(_st("#listLoaderBar")._asJQuery())._remove();
_st(_st("#mainTitle")._asJQuery())._text_("Let's do this!");
_st(_st(self)._list())._render();
return _st(self)._renderListFooterOn_(html);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderListOn:",{html:html},smalltalk.ReactiveWidget)});},
args: ["html"],
source: "renderListOn: html\x0a\x0a\x09html div\x0a\x09\x09\x09id: 'listLoaderBar';\x0a\x09\x09\x09with:[html img src: 'static/img/loaderBar.gif'].\x0a\x09\x09\x0a\x09self listDo:[\x0a\x09\x09'#listLoaderBar' asJQuery remove.\x0a\x09\x09'#mainTitle' asJQuery text: 'Let''s do this!'.\x09\x09\x0a\x09\x09self list render.\x0a\x09\x09self renderListFooterOn: html]",
messageSends: ["id:", "div", "with:", "src:", "img", "listDo:", "remove", "asJQuery", "text:", "render", "list", "renderListFooterOn:"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderNayOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$5,$7,$8,$6,$9,$11,$12,$10,$13,$14,$15,$16,$4,$2;
$1=_st(html)._div();
_st($1)._id_("nayWrapper");
_st($1)._onClick_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st("#Wrapper")._asJQuery())._slideUp();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._div();
_st($3)._id_("nay");
$4=_st($3)._with_((function(){
return smalltalk.withContext(function($ctx3) {
_st(_st(html)._img())._src_("static/img/ocean.png");
$5=_st(html)._div();
_st($5)._class_("photoCredit");
$6=_st($5)._with_((function(){
return smalltalk.withContext(function($ctx4) {
$7=_st(html)._a();
_st($7)._class_("photoCredit");
_st($7)._target_("_blank");
_st($7)._href_("http://www.flickr.com/photos/milanboers/3506659147");
$8=_st($7)._with_("Photo by Milan Boers");
return $8;
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$6;
_st(_st(html)._h2())._with_("A drop into the ocean...");
_st(_st(html)._p())._with_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(html)._span_("...closer to a better world.");
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$9=_st(html)._div();
_st($9)._id_("feedback");
$10=_st($9)._with_((function(){
return smalltalk.withContext(function($ctx4) {
_st(_st(html)._label())._with_("Your qualified feedback might just do that:");
$11=_st(html)._textarea();
_st($11)._onKeyUp_((function(e){
return smalltalk.withContext(function($ctx5) {
return _st(self)._onNayTextAreaKeyUp_(e);
}, function($ctx5) {$ctx5.fillBlock({e:e},$ctx1)})}));
_st($11)._at_put_("placeholder","What you did or did not like? What do you think would be nice to have next?");
$12=_st($11)._yourself();
return $12;
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$10;
_st(_st(html)._p())._with_((function(){
return smalltalk.withContext(function($ctx4) {
$13=_st(html)._input();
_st($13)._id_("nayEmail");
_st($13)._at_put_("placeholder","Email for contact? (optional)");
_st($13)._onKeyUp_((function(e){
return smalltalk.withContext(function($ctx5) {
return _st(self)._onNayKeyUp_(e);
}, function($ctx5) {$ctx5.fillBlock({e:e},$ctx1)})}));
$14=_st($13)._yourself();
return $14;
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$15=_st(html)._a();
_st($15)._class_("dismiss");
_st($15)._onClick_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(self)._onOkNay();
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$16=_st($15)._with_("Ok");
return $16;
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
return $4;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderNayOn:",{html:html},smalltalk.ReactiveWidget)});},
args: ["html"],
source: "renderNayOn: html\x0a\x0a\x09html div\x0a\x09\x09id: 'nayWrapper';\x0a\x09\x09onClick:['#Wrapper' asJQuery slideUp];\x0a\x09\x09with:[\x0a\x09\x09\x09html div\x0a\x09\x09\x09\x09id: 'nay';\x0a\x09\x09\x09\x09with:[\x0a\x09\x09\x09\x09\x09html img src: 'static/img/ocean.png'.\x0a\x09\x09\x09\x09\x09html div class: 'photoCredit'; with: [html a class: 'photoCredit'; target: '_blank'; href: 'http://www.flickr.com/photos/milanboers/3506659147'; with: 'Photo by Milan Boers'].\x0a\x09\x09\x09\x09\x09html h2 with:'A drop into the ocean...'.\x0a\x09\x09\x09\x09\x09html p with: [\x0a\x09\x09\x09\x09\x09\x09html span: '...closer to a better world.'].\x0a\x09\x09\x09\x09\x09html div\x0a\x09\x09\x09\x09\x09\x09id: 'feedback';\x0a\x09\x09\x09\x09\x09\x09with: [\x0a\x09\x09\x09\x09\x09\x09\x09html label with: 'Your qualified feedback might just do that:'.\x0a\x09\x09\x09\x09\x09\x09\x09html textarea\x0a\x09\x09\x09\x09\x09\x09\x09\x09onKeyUp:[:e| self onNayTextAreaKeyUp:e];\x0a\x09\x09\x09\x09\x09\x09\x09\x09at: 'placeholder' put: 'What you did or did not like? What do you think would be nice to have next?';\x0a\x09\x09\x09\x09\x09\x09\x09\x09yourself].\x09\x09\x09\x09\x09\x09\x0a\x09\x09\x09\x09\x09html p with: [\x0a\x09\x09\x09\x09\x09\x09\x09html input\x0a\x09\x09\x09\x09\x09\x09\x09\x09id: 'nayEmail';\x0a\x09\x09\x09\x09\x09\x09\x09\x09at: 'placeholder' put: 'Email for contact? (optional)';\x0a\x09\x09\x09\x09\x09\x09\x09\x09onKeyUp:[:e| self onNayKeyUp:e];\x0a\x09\x09\x09\x09\x09\x09\x09\x09yourself].\x09\x09\x09\x09\x09\x09\x0a\x09\x09\x09\x09\x09html a \x0a\x09\x09\x09\x09\x09\x09class: 'dismiss';\x0a\x09\x09\x09\x09\x09\x09onClick:[self onOkNay]; with: 'Ok'.\x0a\x09\x09\x09]]",
messageSends: ["id:", "div", "onClick:", "slideUp", "asJQuery", "with:", "src:", "img", "class:", "a", "target:", "href:", "h2", "span:", "p", "label", "onKeyUp:", "onNayTextAreaKeyUp:", "textarea", "at:put:", "yourself", "input", "onNayKeyUp:", "onOkNay"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
_st(self)._renderHeaderOn_(html);
_st(self)._renderTipOn_(html);
_st(self)._renderTitleOn_(html);
$1=_st(html)._div();
_st($1)._id_("wrapper");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(self)._renderListOn_(html);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st(self)._renderFooterOn_(html);
_st(self)._renderYayOn_(html);
_st(self)._renderNayOn_(html);
return self}, function($ctx1) {$ctx1.fill(self,"renderOn:",{html:html},smalltalk.ReactiveWidget)});},
args: ["html"],
source: "renderOn: html\x0a\x0a\x09self renderHeaderOn: html.\x0a\x09self renderTipOn: html.\x0a\x0a\x09self renderTitleOn: html.\x09\x0a\x0a\x09html div\x0a\x09\x09id: 'wrapper';\x0a\x09\x09with:[\x0a\x09\x09\x09self renderListOn: html].\x0a\x09\x09\x09\x0a\x09self renderFooterOn: html.\x09\x09\x09\x0a\x09self renderYayOn: html.\x0a\x09self renderNayOn: html.\x0a\x09",
messageSends: ["renderHeaderOn:", "renderTipOn:", "renderTitleOn:", "id:", "div", "with:", "renderListOn:", "renderFooterOn:", "renderYayOn:", "renderNayOn:"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderSocialLoginOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3,$4;
$1=_st(self)._isSociallyLoggedIn();
if(smalltalk.assert($1)){
return nil;
};
_st(_st(html)._div())._id_("fb-root");
$2=_st(self)._isPublished();
if(smalltalk.assert($2)){
_st(_st(_st(_st(self)._session())._social())._facebook())._load();
};
$3=_st(html)._script();
_st($3)._at_put_("type","in/Login");
$4=_st($3)._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(html)._with_("Hello!");
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st(_st(_st(_st(self)._session())._social())._linkedIn())._renderLoadOn_(html);
return self}, function($ctx1) {$ctx1.fill(self,"renderSocialLoginOn:",{html:html},smalltalk.ReactiveWidget)});},
args: ["html"],
source: "renderSocialLoginOn: html\x0a\x09\x0a\x09self isSociallyLoggedIn ifTrue:[ ^ nil].\x0a\x09\x0a\x09html div id: 'fb-root'.\x0a\x09self isPublished ifTrue:[\x0a\x09\x09self session social facebook load].\x0a\x0a\x09html script\x0a\x09\x09at: 'type' put: 'in/Login';\x0a\x09\x09with:[ \x0a\x09\x09\x09html with: 'Hello!'.\x0a\x09\x09\x09\x22(html tag: '?')\x0a\x09\x09\x09\x09at: 'js' put: 'firstName';\x0a\x09\x09\x09\x09yourself.\x0a\x09\x09\x09(html tag: '?')\x0a\x09\x09\x09\x09at: 'js' put: 'lastName';\x0a\x09\x09\x09\x09yourself\x22].\x0a\x0a\x09self session social linkedIn renderLoadOn: html.",
messageSends: ["ifTrue:", "isSociallyLoggedIn", "id:", "div", "load", "facebook", "social", "session", "isPublished", "at:put:", "script", "with:", "renderLoadOn:", "linkedIn"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderTipOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$4,$6,$7,$5,$3;
$1=_st(self)._hasTip();
if(smalltalk.assert($1)){
_st((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st("#tipWrapper")._asJQuery())._slideDown_(smalltalk.HashedCollection._fromPairs_([_st("complete").__minus_gt(_st((function(){
return smalltalk.withContext(function($ctx3) {
return _st(_st("#tipWrapper")._asJQuery())._slideUp();
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}))._valueWithTimeout_((8000)))]));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))._valueWithTimeout_((5000));
};
$2=_st(html)._div();
_st($2)._id_("tipWrapper");
_st($2)._onClick_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st("#tipWrapper")._asJQuery())._slideUp();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$3=_st($2)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$4=_st(html)._div();
_st($4)._id_("tip");
$5=_st($4)._with_((function(){
return smalltalk.withContext(function($ctx3) {
_st(_st(html)._img())._src_("static/img/manyClients.png");
_st(_st(html)._h2())._with_("Connect more devices for extra fun");
_st(_st(html)._h2())._with_(":D");
_st(_st(html)._p())._with_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(html)._span_("Try dragging, adding and changing tasks.");
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$6=_st(html)._a();
_st($6)._class_("dismiss");
_st($6)._onClick_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(self)._onGotIt();
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$7=_st($6)._with_("Ok, got it!");
return $7;
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
return $5;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderTipOn:",{html:html},smalltalk.ReactiveWidget)});},
args: ["html"],
source: "renderTipOn: html\x0a\x0a\x09self hasTip ifTrue:[\x0a\x09\x09['#tipWrapper' asJQuery slideDown: #{'complete'->(['#tipWrapper' asJQuery slideUp] valueWithTimeout: 8000)} ] valueWithTimeout: 5000].\x0a\x0a\x09html div\x0a\x09\x09id: 'tipWrapper';\x0a\x09\x09onClick:['#tipWrapper' asJQuery slideUp];\x0a\x09\x09with:[\x0a\x09\x09\x09html div\x0a\x09\x09\x09\x09id: 'tip';\x0a\x09\x09\x09\x09with:[\x0a\x09\x09\x09\x09\x09html img src: 'static/img/manyClients.png'.\x0a\x09\x09\x09\x09\x09html h2 with:'Connect more devices for extra fun'.\x0a\x09\x09\x09\x09\x09html h2 with:':D'.\x0a\x09\x09\x09\x09\x09html p with:[\x0a\x09\x09\x09\x09\x09\x09html span: 'Try dragging, adding and changing tasks.'].\x0a\x09\x09\x09\x09\x09html a \x0a\x09\x09\x09\x09\x09\x09class: 'dismiss';\x0a\x09\x09\x09\x09\x09\x09onClick:[self onGotIt]; with: 'Ok, got it!'.\x0a\x09\x09\x09]]",
messageSends: ["ifTrue:", "valueWithTimeout:", "slideDown:", "->", "slideUp", "asJQuery", "hasTip", "id:", "div", "onClick:", "with:", "src:", "img", "h2", "span:", "p", "class:", "a", "onGotIt"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderTitleOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st("#mainTitle")._asJQuery())._text_("Let's do...");
return self}, function($ctx1) {$ctx1.fill(self,"renderTitleOn:",{html:html},smalltalk.ReactiveWidget)});},
args: ["html"],
source: "renderTitleOn: html\x0a\x09\x0a\x09'#mainTitle' asJQuery text: 'Let''s do...'",
messageSends: ["text:", "asJQuery"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderYayOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$5,$7,$8,$6,$9,$11,$12,$10,$13,$14,$15,$16,$4,$2;
$1=_st(html)._div();
_st($1)._id_("yayWrapper");
_st($1)._onClick_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st("#Wrapper")._asJQuery())._slideUp();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._div();
_st($3)._id_("yay");
$4=_st($3)._with_((function(){
return smalltalk.withContext(function($ctx3) {
_st(_st(html)._img())._src_("static/img/baby.png");
$5=_st(html)._div();
_st($5)._class_("photoCredit");
$6=_st($5)._with_((function(){
return smalltalk.withContext(function($ctx4) {
$7=_st(html)._a();
_st($7)._class_("photoCredit");
_st($7)._target_("_blank");
_st($7)._href_("http://www.flickr.com/photos/paparutzi/1062532768");
$8=_st($7)._with_("Photo by Christina Rutz");
return $8;
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$6;
_st(_st(html)._h2())._with_("Small...");
_st(_st(html)._p())._with_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(html)._span_("...is how things start.");
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
_st(_st(html)._p())._with_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(html)._span_("Are you a doer? This experiment is for something that will help doers and change makers in engaging the right people with their work and make this world a better place to live in.");
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
_st(_st(html)._p())._with_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(html)._span_("Makes sense?");
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
_st(_st(html)._div())._class_("g-plusone");
$9=_st(html)._p();
_st($9)._class_("feedback");
$10=_st($9)._with_((function(){
return smalltalk.withContext(function($ctx4) {
$11=_st(html)._a();
_st($11)._href_("https://docs.google.com/forms/d/17cVwbIWz72C2T4nKrfVGQLP4zf8JGomyp6zJ2_mGspk/viewform");
_st($11)._target_("_blank");
$12=_st($11)._with_("This");
$12;
return _st(html)._span_("3 question poll will also help us help you!");
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$10;
_st(_st(html)._p())._with_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(_st(html)._label())._with_((function(){
return smalltalk.withContext(function($ctx5) {
_st(html)._p_((function(){
return smalltalk.withContext(function($ctx6) {
_st(html)._span_("Want an ");
_st(html)._strong_("early invite");
return _st(html)._span_("?");
}, function($ctx6) {$ctx6.fillBlock({},$ctx1)})}));
$13=_st(html)._input();
_st($13)._id_("yayEmail");
_st($13)._at_put_("placeholder","Enter your email");
_st($13)._onKeyUp_((function(e){
return smalltalk.withContext(function($ctx6) {
return _st(self)._onYayKeyUp_(e);
}, function($ctx6) {$ctx6.fillBlock({e:e},$ctx1)})}));
$14=_st($13)._yourself();
return $14;
}, function($ctx5) {$ctx5.fillBlock({},$ctx1)})}));
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$15=_st(html)._a();
_st($15)._class_("dismiss");
_st($15)._onClick_((function(){
return smalltalk.withContext(function($ctx4) {
return _st(self)._onOkYay();
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
$16=_st($15)._with_("Ok");
return $16;
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
return $4;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
_st(self)._setGooglePlus();
return self}, function($ctx1) {$ctx1.fill(self,"renderYayOn:",{html:html},smalltalk.ReactiveWidget)});},
args: ["html"],
source: "renderYayOn: html\x0a\x0a\x09html div\x0a\x09\x09id: 'yayWrapper';\x0a\x09\x09onClick:['#Wrapper' asJQuery slideUp];\x0a\x09\x09with:[\x0a\x09\x09\x09html div\x0a\x09\x09\x09\x09id: 'yay';\x0a\x09\x09\x09\x09with:[\x0a\x09\x09\x09\x09\x09html img src: 'static/img/baby.png'.\x0a\x09\x09\x09\x09\x09html div class: 'photoCredit'; with: [html a class: 'photoCredit'; target: '_blank'; href: 'http://www.flickr.com/photos/paparutzi/1062532768'; with: 'Photo by Christina Rutz'].\x0a\x09\x09\x09\x09\x09html h2 with:'Small...'.\x0a\x09\x09\x09\x09\x09html p with:[\x0a\x09\x09\x09\x09\x09\x09html span: '...is how things start.'].\x0a\x09\x09\x09\x09\x09html p with:[\x0a\x09\x09\x09\x09\x09\x09html span: 'Are you a doer? This experiment is for something that will help doers and change makers in engaging the right people with their work and make this world a better place to live in.'].\x0a\x09\x09\x09\x09\x09html p with:[\x0a\x09\x09\x09\x09\x09\x09html span: 'Makes sense?'].\x0a\x09\x09\x09\x09\x09html div class: 'g-plusone'.\x0a\x09\x09\x09\x09\x09html p \x0a\x09\x09\x09\x09\x09\x09class: 'feedback';\x0a\x09\x09\x09\x09\x09\x09with:[\x0a\x09\x09\x09\x09\x09\x09\x09html a href:'https://docs.google.com/forms/d/17cVwbIWz72C2T4nKrfVGQLP4zf8JGomyp6zJ2_mGspk/viewform';target:'_blank';with: 'This'.\x0a\x09\x09\x09\x09\x09\x09\x09html span: '3 question poll will also help us help you!'].\x0a\x09\x09\x09html p with: [\x0a\x09\x09\x09\x09\x09\x09\x09html label\x0a\x09\x09\x09\x09\x09\x09\x09\x09with:[\x0a\x09\x09\x09\x09\x09\x09\x09\x09\x09html p: [\x0a\x09\x09\x09\x09\x09\x09\x09\x09\x09\x09html span: 'Want an '.\x0a\x09\x09\x09\x09\x09\x09\x09\x09\x09\x09html strong: 'early invite'.\x0a\x09\x09\x09\x09\x09\x09\x09\x09\x09\x09html span: '?'].\x0a\x09\x09\x09\x09\x09\x09\x09\x09\x09html input\x0a\x09\x09\x09\x09\x09\x09\x09\x09\x09\x09id: 'yayEmail';\x0a\x09\x09\x09\x09\x09\x09\x09\x09\x09\x09at: 'placeholder' put: 'Enter your email';\x0a\x09\x09\x09\x09\x09\x09\x09\x09\x09\x09onKeyUp:[:e| self onYayKeyUp:e];\x0a\x09\x09\x09\x09\x09\x09\x09\x09\x09\x09yourself]].\x09\x09\x09\x09\x09\x09\x0a\x09\x09\x09\x09\x09html a \x0a\x09\x09\x09\x09\x09\x09class: 'dismiss';\x0a\x09\x09\x09\x09\x09\x09onClick:[self onOkYay]; with: 'Ok'.\x0a\x09\x09\x09]].\x0a\x09\x09\x09\x0a\x09\x22self setFacebookPageLike.\x22\x0a\x09self setGooglePlus",
messageSends: ["id:", "div", "onClick:", "slideUp", "asJQuery", "with:", "src:", "img", "class:", "a", "target:", "href:", "h2", "span:", "p", "p:", "strong:", "input", "at:put:", "onKeyUp:", "onYayKeyUp:", "yourself", "label", "onOkYay", "setGooglePlus"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "sendEmail",
category: 'not yet classified',
fn: function () {
var self=this;
var email,prospect;
function $RProspect(){return smalltalk.RProspect||(typeof RProspect=="undefined"?nil:RProspect)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
email=_st(_st("#yayEmail")._asJQuery())._val();
$1=_st(email)._isEmpty();
if(smalltalk.assert($1)){
return nil;
};
$2=_st($RProspect())._new();
_st($2)._email_(email);
$3=_st($2)._yourself();
prospect=$3;
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt("api/prospect"),_st("type").__minus_gt("POST"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(prospect)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"sendEmail",{email:email,prospect:prospect},smalltalk.ReactiveWidget)});},
args: [],
source: "sendEmail\x0a\x0a\x09| email prospect |\x0a\x0a\x09email := '#yayEmail' asJQuery val.\x0a\x09email isEmpty ifTrue:[^ nil].\x0a\x09\x0a\x09prospect := RProspect new\x0a\x09\x09email: email;\x0a\x09\x09yourself.\x0a\x09\x09\x0a\x09jQuery ajax: #{ \x0a\x09\x09'url' -> ('api/prospect').\x0a\x09\x09'type' -> 'POST'.\x0a\x09\x09'cache' -> false.\x0a\x09\x09'data' -> prospect asJSONString.\x0a\x09\x09'success' -> [:x| ].\x0a\x09\x09'fail' -> [:x| ].\x0a\x09\x09'error' -> [:x| ]}\x09\x0a\x09",
messageSends: ["val", "asJQuery", "ifTrue:", "isEmpty", "email:", "new", "yourself", "ajax:", "->", "asJSONString"],
referencedClasses: ["RProspect"]
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "sendFeedback",
category: 'not yet classified',
fn: function () {
var self=this;
var feedback,email,prospect;
function $RProspect(){return smalltalk.RProspect||(typeof RProspect=="undefined"?nil:RProspect)}
return smalltalk.withContext(function($ctx1) { 
var $1,$2,$3;
feedback=_st(_st(_st("#feedback")._asJQuery())._find_("textarea"))._val();
$1=_st(feedback)._isEmpty();
if(smalltalk.assert($1)){
return nil;
};
email=_st(_st("#nayEmail")._asJQuery())._val();
$2=_st($RProspect())._new();
_st($2)._feedback_(feedback);
_st($2)._email_(email);
$3=_st($2)._yourself();
prospect=$3;
_st(jQuery)._ajax_(smalltalk.HashedCollection._fromPairs_([_st("url").__minus_gt("api/prospect"),_st("type").__minus_gt("POST"),_st("cache").__minus_gt(false),_st("data").__minus_gt(_st(prospect)._asJSONString()),_st("success").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("fail").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})})),_st("error").__minus_gt((function(x){
return smalltalk.withContext(function($ctx2) {
}, function($ctx2) {$ctx2.fillBlock({x:x},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"sendFeedback",{feedback:feedback,email:email,prospect:prospect},smalltalk.ReactiveWidget)});},
args: [],
source: "sendFeedback\x0a\x0a\x09| feedback email prospect |\x0a\x0a\x09feedback := ('#feedback' asJQuery find: 'textarea') val.\x0a\x09feedback isEmpty ifTrue:[^ nil].\x0a\x0a\x09email := '#nayEmail' asJQuery val.\x0a\x09\x0a\x09prospect := RProspect new\x0a\x09\x09feedback: feedback;\x0a\x09\x09email: email;\x0a\x09\x09yourself.\x0a\x09\x09\x0a\x09jQuery ajax: #{ \x0a\x09\x09'url' -> ('api/prospect').\x0a\x09\x09'type' -> 'POST'.\x0a\x09\x09'cache' -> false.\x0a\x09\x09'data' -> prospect asJSONString.\x0a\x09\x09'success' -> [:x| ].\x0a\x09\x09'fail' -> [:x| ].\x0a\x09\x09'error' -> [:x| ]}\x09\x0a\x09",
messageSends: ["val", "find:", "asJQuery", "ifTrue:", "isEmpty", "feedback:", "new", "email:", "yourself", "ajax:", "->", "asJSONString"],
referencedClasses: ["RProspect"]
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "session",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@session"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeSession();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"session",{},smalltalk.ReactiveWidget)});},
args: [],
source: "session\x0a\x0a\x09^ session ifNil:[self initializeSession]",
messageSends: ["ifNil:", "initializeSession"],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "setFacebookPageLike",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
(function(d, s, id){
     var js, fjs = d.getElementsByTagName(s)[0];
     if (d.getElementById(id)) {return;}
     js = d.createElement(s); js.id = id;
     js.src = "//connect.facebook.net/en_US/all.js";
     fjs.parentNode.insertBefore(js, fjs);
   }(document, 'script', 'facebook-jssdk'));;
return self}, function($ctx1) {$ctx1.fill(self,"setFacebookPageLike",{},smalltalk.ReactiveWidget)});},
args: [],
source: "setFacebookPageLike\x0a<(function(d, s, id){\x0a     var js, fjs = d.getElementsByTagName(s)[0];\x0a     if (d.getElementById(id)) {return;}\x0a     js = d.createElement(s); js.id = id;\x0a     js.src = \x22//connect.facebook.net/en_US/all.js\x22;\x0a     fjs.parentNode.insertBefore(js, fjs);\x0a   }(document, 'script', 'facebook-jssdk'));>",
messageSends: [],
referencedClasses: []
}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "setGooglePlus",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
(function() {
    var po = document.createElement('script'); po.type = 'text/javascript'; po.async = true;
    po.src = 'https://apis.google.com/js/plusone.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(po, s);
  })();;
return self}, function($ctx1) {$ctx1.fill(self,"setGooglePlus",{},smalltalk.ReactiveWidget)});},
args: [],
source: "setGooglePlus\x0a\x09<(function() {\x0a    var po = document.createElement('script'); po.type = 'text/javascript'; po.async = true;\x0a    po.src = 'https://apis.google.com/js/plusone.js';\x0a    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(po, s);\x0a  })();>",
messageSends: [],
referencedClasses: []
}),
smalltalk.ReactiveWidget);


smalltalk.addMethod(
smalltalk.method({
selector: "open",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._new())._open();
return $1;
}, function($ctx1) {$ctx1.fill(self,"open",{},smalltalk.ReactiveWidget.klass)});},
args: [],
source: "open\x0a\x0a\x09^ self new open",
messageSends: ["open", "new"],
referencedClasses: []
}),
smalltalk.ReactiveWidget.klass);


smalltalk.addClass('TaskWidget', smalltalk.RWidget, ['listItem', 'view', 'editor'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "changedTask:",
category: 'not yet classified',
fn: function (aTask) {
var self=this;
function $RChangeTask(){return smalltalk.RChangeTask||(typeof RChangeTask=="undefined"?nil:RChangeTask)}
return smalltalk.withContext(function($ctx1) { 
_st(self)._updateTask_(aTask);
_st(self)._model_(aTask);
_st(_st($RChangeTask())._for_(self["@model"]))._localSave();
return self}, function($ctx1) {$ctx1.fill(self,"changedTask:",{aTask:aTask},smalltalk.TaskWidget)});},
args: ["aTask"],
source: "changedTask: aTask\x0a\x0a\x09self updateTask: aTask.\x0a\x09self model: aTask.\x0a\x09(RChangeTask for: model) localSave.",
messageSends: ["updateTask:", "model:", "localSave", "for:"],
referencedClasses: ["RChangeTask"]
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "editor",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@editor"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeEditor();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"editor",{},smalltalk.TaskWidget)});},
args: [],
source: "editor\x0a\x0a\x09^ editor ifNil:[self initializeEditor]",
messageSends: ["ifNil:", "initializeEditor"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "hide",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self["@listItem"])._asJQuery())._hide();
return self}, function($ctx1) {$ctx1.fill(self,"hide",{},smalltalk.TaskWidget)});},
args: [],
source: "hide\x0a\x0a\x09listItem asJQuery hide",
messageSends: ["hide", "asJQuery"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeEditor",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@editor"]=_st(self)._makeEditor();
$1=self["@editor"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeEditor",{},smalltalk.TaskWidget)});},
args: [],
source: "initializeEditor\x0a\x09\x0a\x09\x09^ editor := self makeEditor",
messageSends: ["makeEditor"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isCompleted",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(self["@listItem"])._asJQuery())._find_("input"))._is_(":checked");
return $1;
}, function($ctx1) {$ctx1.fill(self,"isCompleted",{},smalltalk.TaskWidget)});},
args: [],
source: "isCompleted \x0a\x0a\x09^ (listItem asJQuery find: 'input') is:':checked'",
messageSends: ["is:", "find:", "asJQuery"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "makeEditor",
category: 'not yet classified',
fn: function () {
var self=this;
var html;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
return smalltalk.withContext(function($ctx1) { 
var $1;
html=_st($HTMLCanvas())._onJQuery_(_st(self["@listItem"])._asJQuery());
$1=_st(self)._renderEditorOn_(html);
return $1;
}, function($ctx1) {$ctx1.fill(self,"makeEditor",{html:html},smalltalk.TaskWidget)});},
args: [],
source: "makeEditor\x0a\x09| html |\x0a\x09html := HTMLCanvas onJQuery: listItem asJQuery.\x0a\x09\x0a\x09^ self renderEditorOn: html",
messageSends: ["onJQuery:", "asJQuery", "renderEditorOn:"],
referencedClasses: ["HTMLCanvas"]
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onCancel",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(_st(self)._editor())._asJQuery())._hide();
_st(_st(self["@view"])._asJQuery())._show();
return self}, function($ctx1) {$ctx1.fill(self,"onCancel",{},smalltalk.TaskWidget)});},
args: [],
source: "onCancel\x0a\x0a\x09self editor asJQuery hide.\x0a\x09view asJQuery show.",
messageSends: ["hide", "asJQuery", "editor", "show"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onCompletionCheckboxClicked:",
category: 'not yet classified',
fn: function (anEvent) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(anEvent)._stopPropagation();
_st(self)._setCompletion();
return self}, function($ctx1) {$ctx1.fill(self,"onCompletionCheckboxClicked:",{anEvent:anEvent},smalltalk.TaskWidget)});},
args: ["anEvent"],
source: "onCompletionCheckboxClicked: anEvent\x0a\x09\x0a\x09anEvent stopPropagation.\x0a\x09\x0a\x09self setCompletion",
messageSends: ["stopPropagation", "setCompletion"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onCompletionClicked:",
category: 'not yet classified',
fn: function (anEvent) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(self["@listItem"])._asJQuery())._find_("input"))._is_(":checked");
if(smalltalk.assert($1)){
_st(_st(_st(self["@listItem"])._asJQuery())._find_("input"))._prop_put_("checked",false);
} else {
_st(_st(_st(self["@listItem"])._asJQuery())._find_("input"))._attr_put_("checked",true);
};
_st(self)._setCompletion();
return self}, function($ctx1) {$ctx1.fill(self,"onCompletionClicked:",{anEvent:anEvent},smalltalk.TaskWidget)});},
args: ["anEvent"],
source: "onCompletionClicked: anEvent\x0a\x09\x09\x0a\x09((listItem asJQuery find: 'input') is:':checked') \x0a\x09\x09ifTrue:[((listItem asJQuery find: 'input') prop:'checked' put: false)]\x0a\x09\x09ifFalse:[((listItem asJQuery find: 'input') attr:'checked' put: true)].\x0a\x09\x09\x0a\x09self setCompletion",
messageSends: ["ifTrue:ifFalse:", "prop:put:", "find:", "asJQuery", "attr:put:", "is:", "setCompletion"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onDescriptionClicked",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self["@view"])._asJQuery())._hide();
_st(_st(_st(_st(self)._editor())._asJQuery())._find_("input"))._val_(_st(self["@model"])._description());
_st(_st(_st(self)._editor())._asJQuery())._show();
_st(_st(_st(_st(self)._editor())._asJQuery())._find_("input"))._focus();
return self}, function($ctx1) {$ctx1.fill(self,"onDescriptionClicked",{},smalltalk.TaskWidget)});},
args: [],
source: "onDescriptionClicked\x0a\x0a\x09view asJQuery hide.\x09\x0a\x09(self editor asJQuery find: 'input') val: model description.\x0a\x09self editor asJQuery show.\x0a\x09(self editor asJQuery find: 'input') focus.",
messageSends: ["hide", "asJQuery", "val:", "description", "find:", "editor", "show", "focus"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onDone",
category: 'not yet classified',
fn: function () {
var self=this;
function $RChangeTask(){return smalltalk.RChangeTask||(typeof RChangeTask=="undefined"?nil:RChangeTask)}
return smalltalk.withContext(function($ctx1) { 
_st(_st(_st(self)._editor())._asJQuery())._hide();
_st(self["@model"])._description_(_st(_st(_st(_st(self)._editor())._asJQuery())._find_("input"))._val());
_st(_st($RChangeTask())._for_(self["@model"]))._execute();
_st(_st(_st(self["@view"])._asJQuery())._find_(".taskDescription"))._text_(_st(self["@model"])._description());
_st(_st(self["@view"])._asJQuery())._show();
_st(_st(self["@view"])._asJQuery())._effect_("highlight");
return self}, function($ctx1) {$ctx1.fill(self,"onDone",{},smalltalk.TaskWidget)});},
args: [],
source: "onDone\x0a\x0a\x09self editor asJQuery hide.\x0a\x09model description: (self editor asJQuery find: 'input') val.\x0a\x09(RChangeTask for: model) execute.\x0a\x09\x0a\x09(view asJQuery find: '.taskDescription') text: model description.\x0a\x09view asJQuery show.\x0a\x09view asJQuery effect: 'highlight'.",
messageSends: ["hide", "asJQuery", "editor", "description:", "val", "find:", "execute", "for:", "text:", "description", "show", "effect:"],
referencedClasses: ["RChangeTask"]
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onKeyUp:",
category: 'not yet classified',
fn: function (e) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(_st(e)._keyCode()).__eq((13));
if(smalltalk.assert($1)){
_st(self)._onDone();
};
$2=_st(_st(e)._keyCode()).__eq((27));
if(smalltalk.assert($2)){
_st(self)._onCancel();
};
return self}, function($ctx1) {$ctx1.fill(self,"onKeyUp:",{e:e},smalltalk.TaskWidget)});},
args: ["e"],
source: "onKeyUp: e\x0a\x0a\x09e keyCode = 13 ifTrue:[\x0a\x09\x09self onDone].\x0a\x0a\x09e keyCode = 27 ifTrue:[\x0a\x09\x09self onCancel].",
messageSends: ["ifTrue:", "onDone", "=", "keyCode", "onCancel"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onRemove:",
category: 'not yet classified',
fn: function (anEvent) {
var self=this;
function $RRemoveTask(){return smalltalk.RRemoveTask||(typeof RRemoveTask=="undefined"?nil:RRemoveTask)}
function $RTaskRemoved(){return smalltalk.RTaskRemoved||(typeof RTaskRemoved=="undefined"?nil:RTaskRemoved)}
return smalltalk.withContext(function($ctx1) { 
_st(anEvent)._stopPropagation();
_st(self)._remove();
_st(_st($RRemoveTask())._for_list_(self["@model"],_st(_st(_st(window)._app())._list())._model()))._execute();
_st(self)._announce_(_st($RTaskRemoved())._for_(self));
return self}, function($ctx1) {$ctx1.fill(self,"onRemove:",{anEvent:anEvent},smalltalk.TaskWidget)});},
args: ["anEvent"],
source: "onRemove: anEvent\x0a\x0a\x09anEvent stopPropagation.\x0a\x09self remove.\x0a\x09(RRemoveTask for: model list: window app list model) execute.\x0a\x09self announce: (RTaskRemoved for: self).\x0a\x09",
messageSends: ["stopPropagation", "remove", "execute", "for:list:", "model", "list", "app", "announce:", "for:"],
referencedClasses: ["RRemoveTask", "RTaskRemoved"]
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "remove",
category: 'not yet classified',
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st(_st(self["@listItem"])._asJQuery())._remove();
_st($RStorage())._delete_(self["@model"]);
return self}, function($ctx1) {$ctx1.fill(self,"remove",{},smalltalk.TaskWidget)});},
args: [],
source: "remove\x0a\x0a\x09listItem asJQuery remove.\x0a\x09RStorage delete: model.",
messageSends: ["remove", "asJQuery", "delete:"],
referencedClasses: ["RStorage"]
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderCompletionOldOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$4,$2;
$1=_st(html)._div();
_st($1)._class_("completionWrapper");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._input();
_st($3)._class_("completion");
_st($3)._type_("checkbox");
$4=_st($3)._at_put_("name",_st(self["@model"])._id());
return $4;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderCompletionOldOn:",{html:html},smalltalk.TaskWidget)});},
args: ["html"],
source: "renderCompletionOldOn: html\x0a\x0a\x09html div\x0a\x09\x09class: 'completionWrapper';\x0a\x09\x09with:[\x0a\x09\x09\x09html input\x0a\x09\x09\x09\x09class: 'completion';\x0a\x09\x09\x09\x09type: 'checkbox';\x0a\x09\x09\x09\x09at: 'name' put: model id]",
messageSends: ["class:", "div", "with:", "input", "type:", "at:put:", "id"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderCompletionOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$4,$2;
$1=_st(html)._div();
_st($1)._class_("completionWrapper");
_st($1)._onClick_((function(e){
return smalltalk.withContext(function($ctx2) {
return _st(self)._onCompletionClicked_(e);
}, function($ctx2) {$ctx2.fillBlock({e:e},$ctx1)})}));
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._input();
_st($3)._onClick_((function(e){
return smalltalk.withContext(function($ctx3) {
return _st(self)._onCompletionCheckboxClicked_(e);
}, function($ctx3) {$ctx3.fillBlock({e:e},$ctx1)})}));
_st($3)._class_("completion");
_st($3)._type_("checkbox");
$4=_st($3)._at_put_("name",_st(self["@model"])._id());
return $4;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderCompletionOn:",{html:html},smalltalk.TaskWidget)});},
args: ["html"],
source: "renderCompletionOn: html\x0a\x0a\x09html div\x0a\x09\x09class: 'completionWrapper';\x0a\x09\x09onClick: [:e| self onCompletionClicked: e];\x0a\x09\x09with:[\x0a\x09\x09\x09html input\x0a\x09\x09\x09\x09onClick: [:e| self onCompletionCheckboxClicked: e];\x0a\x09\x09\x09\x09class: 'completion';\x0a\x09\x09\x09\x09type: 'checkbox';\x0a\x09\x09\x09\x09at: 'name' put: model id]",
messageSends: ["class:", "div", "onClick:", "onCompletionClicked:", "with:", "onCompletionCheckboxClicked:", "input", "type:", "at:put:", "id"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderCompletionTableOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$4,$2;
$1=_st(html)._table();
_st($1)._class_("completion");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(html)._tr())._with_((function(){
return smalltalk.withContext(function($ctx3) {
return _st(_st(html)._td())._with_((function(){
return smalltalk.withContext(function($ctx4) {
$3=_st(html)._input();
_st($3)._type_("checkbox");
$4=_st($3)._at_put_("name",_st(self["@model"])._id());
return $4;
}, function($ctx4) {$ctx4.fillBlock({},$ctx1)})}));
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderCompletionTableOn:",{html:html},smalltalk.TaskWidget)});},
args: ["html"],
source: "renderCompletionTableOn: html\x0a\x0a\x09html table\x0a\x09\x09class: 'completion';\x0a\x09\x09with:[\x0a\x09\x09\x09html tr with:[\x0a\x09\x09\x09\x09html td with:[\x0a\x09\x09\x09\x09\x09html input\x0a\x09\x09\x09\x09\x09\x09type: 'checkbox';\x0a\x09\x09\x09\x09\x09\x09at: 'name' put: model id]]]",
messageSends: ["class:", "table", "with:", "type:", "input", "at:put:", "id", "td", "tr"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderCompletionWrappedOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$4,$2;
$1=_st(html)._div();
_st($1)._class_("completionWrapper");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._input();
_st($3)._class_("completion");
_st($3)._type_("checkbox");
$4=_st($3)._at_put_("name",_st(self["@model"])._id());
return $4;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"renderCompletionWrappedOn:",{html:html},smalltalk.TaskWidget)});},
args: ["html"],
source: "renderCompletionWrappedOn: html\x0a\x0a\x09html div\x0a\x09\x09class: 'completionWrapper';\x0a\x09\x09with:[\x0a\x09\x09\x09html input\x0a\x09\x09\x09\x09class: 'completion';\x0a\x09\x09\x09\x09type: 'checkbox';\x0a\x09\x09\x09\x09at: 'name' put: model id]",
messageSends: ["class:", "div", "with:", "input", "type:", "at:put:", "id"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderEditorOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
var container;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$5,$6,$4,$2,$7;
$1=_st(html)._div();
_st($1)._class_("taskEditorWrapper");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._div();
_st($3)._class_("taskEditor");
$4=_st($3)._with_((function(){
return smalltalk.withContext(function($ctx3) {
$5=_st(html)._input();
_st($5)._class_("taskDescription");
_st($5)._onKeyUp_((function(e){
return smalltalk.withContext(function($ctx4) {
return _st(self)._onKeyUp_(e);
}, function($ctx4) {$ctx4.fillBlock({e:e},$ctx1)})}));
$6=_st($5)._with_(_st(self["@model"])._description());
return $6;
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
return $4;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
container=$2;
$7=container;
return $7;
}, function($ctx1) {$ctx1.fill(self,"renderEditorOn:",{html:html,container:container},smalltalk.TaskWidget)});},
args: ["html"],
source: "renderEditorOn: html\x0a\x0a\x09| container |\x0a\x0a\x09container := html div\x0a\x09\x09class: 'taskEditorWrapper';\x0a\x09\x09with:[\x0a\x09\x09html div\x0a\x09\x09\x09\x09\x09class:'taskEditor';\x0a\x09\x09\x09\x09\x09with:[\x0a\x09\x09\x09\x09\x09\x09html input\x0a\x09\x09\x09\x09\x09\x09\x09class: 'taskDescription';\x0a\x09\x09\x09\x09\x09\x09\x09onKeyUp:[:e| self onKeyUp: e];\x0a\x09\x09\x09\x09\x09\x09\x09with: model description]].\x0a\x09\x09\x09\x09\x09\x09\x09\x0a\x09^ container",
messageSends: ["class:", "div", "with:", "input", "onKeyUp:", "description"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(html)._li();
_st($1)._id_(_st(self["@model"])._id());
_st($1)._class_("task");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
_st(self)._renderCompletionOn_(html);
return _st(self)._renderViewOn_(html);
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
self["@listItem"]=$2;
_st(self)._updateTask_(self["@model"]);
return self}, function($ctx1) {$ctx1.fill(self,"renderOn:",{html:html},smalltalk.TaskWidget)});},
args: ["html"],
source: "renderOn: html\x0a\x0a\x09listItem := html li\x0a\x09\x09id: model id;\x0a\x09\x09class: 'task';\x0a\x09\x09with: [\x0a\x09\x09\x09self renderCompletionOn: html.\x0a\x09\x09\x09self renderViewOn: html].\x0a\x09\x09\x09\x0a\x09self updateTask: model.",
messageSends: ["id:", "id", "li", "class:", "with:", "renderCompletionOn:", "renderViewOn:", "updateTask:"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderViewOn:",
category: 'not yet classified',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$3,$5,$6,$7,$8,$4,$2;
$1=_st(html)._div();
_st($1)._class_("taskViewWrapper");
$2=_st($1)._with_((function(){
return smalltalk.withContext(function($ctx2) {
$3=_st(html)._div();
_st($3)._class_("taskView");
_st($3)._onClick_((function(){
return smalltalk.withContext(function($ctx3) {
return _st(self)._onDescriptionClicked();
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
$4=_st($3)._with_((function(){
return smalltalk.withContext(function($ctx3) {
$5=_st(html)._span();
_st($5)._class_("taskDescription");
$6=_st($5)._with_(_st(self["@model"])._description());
$6;
$7=_st(html)._img();
_st($7)._class_("delete");
_st($7)._onClick_((function(e){
return smalltalk.withContext(function($ctx4) {
return _st(self)._onRemove_(e);
}, function($ctx4) {$ctx4.fillBlock({e:e},$ctx1)})}));
$8=_st($7)._src_("static/img/delete.png");
return $8;
}, function($ctx3) {$ctx3.fillBlock({},$ctx1)})}));
return $4;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
self["@view"]=$2;
return self}, function($ctx1) {$ctx1.fill(self,"renderViewOn:",{html:html},smalltalk.TaskWidget)});},
args: ["html"],
source: "renderViewOn: html\x0a\x0a\x09view := html div\x0a\x09\x09class: 'taskViewWrapper';\x0a\x09\x09with:[\x09html div\x0a\x09\x09\x09\x09\x09class:'taskView';\x0a\x09\x09\x09\x09\x09onClick:[self onDescriptionClicked];\x0a\x09\x09\x09\x09\x09with:[\x0a\x09\x09\x09\x09\x09\x09html span\x0a\x09\x09\x09\x09\x09\x09\x09class: 'taskDescription';\x0a\x09\x09\x09\x09\x09\x09\x09with: model description.\x0a\x09\x09\x09\x09\x09\x09html img\x0a\x09\x09\x09\x09\x09\x09\x09class: 'delete';\x0a\x09\x09\x09\x09\x09\x09\x09onClick:[:e| self onRemove: e];\x0a\x09\x09\x09\x09\x09\x09\x09src: 'static/img/delete.png']]",
messageSends: ["class:", "div", "with:", "onClick:", "onDescriptionClicked", "span", "description", "img", "onRemove:", "src:"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "setCompletion",
category: 'not yet classified',
fn: function () {
var self=this;
function $RChangeTask(){return smalltalk.RChangeTask||(typeof RChangeTask=="undefined"?nil:RChangeTask)}
function $RTaskChanged(){return smalltalk.RTaskChanged||(typeof RTaskChanged=="undefined"?nil:RTaskChanged)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._isCompleted();
if(smalltalk.assert($1)){
_st(_st(_st(self["@listItem"])._asJQuery())._find_("span.taskDescription"))._addClass_("completed");
} else {
_st(_st(_st(self["@listItem"])._asJQuery())._find_("span.taskDescription"))._removeClass_("completed");
};
_st(self["@model"])._isCompleted_(_st(self)._isCompleted());
_st(_st($RChangeTask())._for_(self["@model"]))._execute();
_st(self)._announce_(_st($RTaskChanged())._for_(self));
return self}, function($ctx1) {$ctx1.fill(self,"setCompletion",{},smalltalk.TaskWidget)});},
args: [],
source: "setCompletion\x0a\x0a\x09self isCompleted\x0a\x09\x09ifTrue:[(listItem asJQuery find: 'span.taskDescription') addClass: 'completed']\x0a\x09\x09ifFalse:[(listItem asJQuery find: 'span.taskDescription') removeClass: 'completed'].\x0a\x09\x09\x0a\x09model isCompleted: self isCompleted.\x0a\x09(RChangeTask for: model) execute.\x0a\x09self announce: (RTaskChanged for: self)\x0a\x09",
messageSends: ["ifTrue:ifFalse:", "addClass:", "find:", "asJQuery", "removeClass:", "isCompleted", "isCompleted:", "execute", "for:", "announce:"],
referencedClasses: ["RChangeTask", "RTaskChanged"]
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "show",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self["@listItem"])._asJQuery())._show();
return self}, function($ctx1) {$ctx1.fill(self,"show",{},smalltalk.TaskWidget)});},
args: [],
source: "show\x0a\x0a\x09listItem asJQuery show",
messageSends: ["show", "asJQuery"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "showIfDone",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._isCompleted();
if(smalltalk.assert($1)){
_st(self)._show();
} else {
_st(self)._hide();
};
return self}, function($ctx1) {$ctx1.fill(self,"showIfDone",{},smalltalk.TaskWidget)});},
args: [],
source: "showIfDone\x0a\x09self isCompleted\x0a\x09\x09ifTrue:[self show]\x0a\x09\x09ifFalse:[self hide]",
messageSends: ["ifTrue:ifFalse:", "show", "hide", "isCompleted"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "showIfToDo",
category: 'not yet classified',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._isCompleted();
if(smalltalk.assert($1)){
_st(self)._hide();
} else {
_st(self)._show();
};
return self}, function($ctx1) {$ctx1.fill(self,"showIfToDo",{},smalltalk.TaskWidget)});},
args: [],
source: "showIfToDo\x0a\x09self isCompleted\x0a\x09\x09ifTrue:[self hide]\x0a\x09\x09ifFalse:[self show]",
messageSends: ["ifTrue:ifFalse:", "hide", "show", "isCompleted"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateTask:",
category: 'not yet classified',
fn: function (aTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._updateTaskDescription_(aTask);
_st(self)._updateTaskCompletion_(aTask);
return self}, function($ctx1) {$ctx1.fill(self,"updateTask:",{aTask:aTask},smalltalk.TaskWidget)});},
args: ["aTask"],
source: "updateTask: aTask\x0a\x0a\x09self updateTaskDescription: aTask.\x0a\x09self updateTaskCompletion: aTask.",
messageSends: ["updateTaskDescription:", "updateTaskCompletion:"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateTaskCompletion:",
category: 'not yet classified',
fn: function (aTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(_st(_st(self["@listItem"])._asJQuery())._find_("input"))._attr_put_("checked",_st(aTask)._isCompleted());
$1=_st(aTask)._isCompleted();
if(smalltalk.assert($1)){
_st(_st(_st(self["@listItem"])._asJQuery())._find_("span.taskDescription"))._addClass_("completed");
} else {
_st(_st(_st(self["@listItem"])._asJQuery())._find_("span.taskDescription"))._removeClass_("completed");
};
return self}, function($ctx1) {$ctx1.fill(self,"updateTaskCompletion:",{aTask:aTask},smalltalk.TaskWidget)});},
args: ["aTask"],
source: "updateTaskCompletion: aTask\x0a\x0a\x09(listItem asJQuery find: 'input') attr:'checked' put: aTask isCompleted.\x0a\x0a\x09aTask isCompleted\x0a\x09\x09ifTrue:[(listItem asJQuery find: 'span.taskDescription') addClass: 'completed']\x0a\x09\x09ifFalse:[(listItem asJQuery find: 'span.taskDescription') removeClass: 'completed'].\x0a\x09",
messageSends: ["attr:put:", "isCompleted", "find:", "asJQuery", "ifTrue:ifFalse:", "addClass:", "removeClass:"],
referencedClasses: []
}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateTaskDescription:",
category: 'not yet classified',
fn: function (aTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self["@model"])._description_(_st(aTask)._description());
_st(_st(self["@view"])._asJQuery())._show();
_st(_st(self["@view"])._asJQuery())._effect_("highlight");
_st(_st(_st(self["@view"])._asJQuery())._find_(".taskDescription"))._text_(_st(self["@model"])._description());
return self}, function($ctx1) {$ctx1.fill(self,"updateTaskDescription:",{aTask:aTask},smalltalk.TaskWidget)});},
args: ["aTask"],
source: "updateTaskDescription: aTask\x0a\x0a\x09model description: aTask description.\x0a\x09view asJQuery show.\x0a\x09view asJQuery effect: 'highlight'.\x0a\x09\x0a\x09(view asJQuery find: '.taskDescription') text: model description.",
messageSends: ["description:", "description", "show", "asJQuery", "effect:", "text:", "find:"],
referencedClasses: []
}),
smalltalk.TaskWidget);



