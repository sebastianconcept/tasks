smalltalk.addPackage('Reactive');
smalltalk.addClass('RAnnouncement', smalltalk.Object, ['subject'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "subject",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@subject"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"subject",{},smalltalk.RAnnouncement)});},
messageSends: []}),
smalltalk.RAnnouncement);

smalltalk.addMethod(
smalltalk.method({
selector: "subject:",
fn: function (anObject) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@subject"]=anObject;
return self}, function($ctx1) {$ctx1.fill(self,"subject:",{anObject:anObject},smalltalk.RAnnouncement)});},
messageSends: []}),
smalltalk.RAnnouncement);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
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
messageSends: ["subject:", "new", "yourself"]}),
smalltalk.RAnnouncement.klass);


smalltalk.addClass('RTaskChanged', smalltalk.RAnnouncement, [], 'Reactive');


smalltalk.addClass('RTaskRemoved', smalltalk.RAnnouncement, [], 'Reactive');


smalltalk.addClass('RTasksSorted', smalltalk.RAnnouncement, [], 'Reactive');


smalltalk.addClass('RClient', smalltalk.Object, ['socket', 'uri', 'onOpen'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "initializeSocket",
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
messageSends: ["onopen:", "onOpen", "makeSocketOn:", "uri", "onclose:", "onmessage:", "onMessage:", "onerror:", "onError:", "yourself"]}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeURI",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@uri"]=_st(_st("ws://").__comma(_st(_st(window)._location())._hostname())).__comma(":21004/socket");
$1=self["@uri"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeURI",{},smalltalk.RClient)});},
messageSends: [",", "hostname", "location"]}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "makeSocketOn:",
fn: function (anUri) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
return new WebSocket(anUri);
return self}, function($ctx1) {$ctx1.fill(self,"makeSocketOn:",{anUri:anUri},smalltalk.RClient)});},
messageSends: []}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "onError:",
fn: function (anException) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._halt();
return self}, function($ctx1) {$ctx1.fill(self,"onError:",{anException:anException},smalltalk.RClient)});},
messageSends: ["halt"]}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "onMessage:",
fn: function (anEvent) {
var self=this;
var command;
function $MaplessModel(){return smalltalk.MaplessModel||(typeof MaplessModel=="undefined"?nil:MaplessModel)}
return smalltalk.withContext(function($ctx1) { 
command=_st($MaplessModel())._fromJsonString_(_st(anEvent)._data());
_st(command)._react();
return self}, function($ctx1) {$ctx1.fill(self,"onMessage:",{anEvent:anEvent,command:command},smalltalk.RClient)});},
messageSends: ["fromJsonString:", "data", "react"]}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "onOpen",
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
messageSends: ["ifNil:"]}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "onOpen:",
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@onOpen"]=aBlock;
return self}, function($ctx1) {$ctx1.fill(self,"onOpen:",{aBlock:aBlock},smalltalk.RClient)});},
messageSends: []}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "open",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._initializeSocket();
return self}, function($ctx1) {$ctx1.fill(self,"open",{},smalltalk.RClient)});},
messageSends: ["initializeSocket"]}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "send:",
fn: function (aString) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._socket())._send_(aString);
return $1;
}, function($ctx1) {$ctx1.fill(self,"send:",{aString:aString},smalltalk.RClient)});},
messageSends: ["send:", "socket"]}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "socket",
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
messageSends: ["ifNil:", "initializeSocket"]}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "uri",
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
messageSends: ["ifNil:", "initializeURI"]}),
smalltalk.RClient);

smalltalk.addMethod(
smalltalk.method({
selector: "uri:",
fn: function (aString) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@uri"]=aString;
return self}, function($ctx1) {$ctx1.fill(self,"uri:",{aString:aString},smalltalk.RClient)});},
messageSends: []}),
smalltalk.RClient);



smalltalk.addClass('RModel', smalltalk.MaplessModel, [], 'Reactive');

smalltalk.addMethod(
smalltalk.method({
selector: "path",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
return "api";
}, function($ctx1) {$ctx1.fill(self,"path",{},smalltalk.RModel.klass)});},
messageSends: []}),
smalltalk.RModel.klass);


smalltalk.addClass('RCommand', smalltalk.RModel, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "api",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(window)._app())._session())._api();
return $1;
}, function($ctx1) {$ctx1.fill(self,"api",{},smalltalk.RCommand)});},
messageSends: ["api", "session", "app"]}),
smalltalk.RCommand);

smalltalk.addMethod(
smalltalk.method({
selector: "execute",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._executeDo_((function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"execute",{},smalltalk.RCommand)});},
messageSends: ["executeDo:"]}),
smalltalk.RCommand);

smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
smalltalk.RModel.fn.prototype._initialize.apply(_st(self), []);
_st(self)._sessionId_(_st(_st(_st(window)._app())._session())._id());
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.RCommand)});},
messageSends: ["initialize", "sessionId:", "id", "session", "app"]}),
smalltalk.RCommand);

smalltalk.addMethod(
smalltalk.method({
selector: "notifyRemote",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._api())._send_(_st(self)._asJSONString());
return self}, function($ctx1) {$ctx1.fill(self,"notifyRemote",{},smalltalk.RCommand)});},
messageSends: ["send:", "asJSONString", "api"]}),
smalltalk.RCommand);

smalltalk.addMethod(
smalltalk.method({
selector: "onAfter:done:",
fn: function (x, aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aBlock)._value();
return self}, function($ctx1) {$ctx1.fill(self,"onAfter:done:",{x:x,aBlock:aBlock},smalltalk.RCommand)});},
messageSends: ["value"]}),
smalltalk.RCommand);

smalltalk.addMethod(
smalltalk.method({
selector: "onFail:",
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._halt();
return self}, function($ctx1) {$ctx1.fill(self,"onFail:",{aBlock:aBlock},smalltalk.RCommand)});},
messageSends: ["halt"]}),
smalltalk.RCommand);

smalltalk.addMethod(
smalltalk.method({
selector: "react",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._subclassResponsibility();
return self}, function($ctx1) {$ctx1.fill(self,"react",{},smalltalk.RCommand)});},
messageSends: ["subclassResponsibility"]}),
smalltalk.RCommand);



smalltalk.addClass('RAddList', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
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
messageSends: ["localSave", "ajax:", "->", ",", "path", "asJSONString", "onAfter:done:", "onFail:"]}),
smalltalk.RAddList);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave",
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st($RStorage())._save_(_st(self)._list());
return self}, function($ctx1) {$ctx1.fill(self,"localSave",{},smalltalk.RAddList)});},
messageSends: ["save:", "list"]}),
smalltalk.RAddList);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
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
messageSends: ["list:", "new", "yourself"]}),
smalltalk.RAddList.klass);


smalltalk.addClass('RAddTask', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
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
messageSends: ["localSave", "updateList:", "list", "notifyRemote", "ajax:", "->", ",", "path", "asJSONString", "onAfter:done:", "onFail:"]}),
smalltalk.RAddTask);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave",
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st($RStorage())._save_(_st(self)._task());
return self}, function($ctx1) {$ctx1.fill(self,"localSave",{},smalltalk.RAddTask)});},
messageSends: ["save:", "task"]}),
smalltalk.RAddTask);

smalltalk.addMethod(
smalltalk.method({
selector: "react",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._app())._addTask_(self);
return self}, function($ctx1) {$ctx1.fill(self,"react",{},smalltalk.RAddTask)});},
messageSends: ["addTask:", "app"]}),
smalltalk.RAddTask);

smalltalk.addMethod(
smalltalk.method({
selector: "saveAndUpdateList:",
fn: function (aList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localSave();
_st(self)._updateList_(aList);
return self}, function($ctx1) {$ctx1.fill(self,"saveAndUpdateList:",{aList:aList},smalltalk.RAddTask)});},
messageSends: ["localSave", "updateList:"]}),
smalltalk.RAddTask);

smalltalk.addMethod(
smalltalk.method({
selector: "updateList:",
fn: function (aList) {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st(aList)._addTask_(_st(_st(self)._task())._id());
_st($RStorage())._save_(aList);
return self}, function($ctx1) {$ctx1.fill(self,"updateList:",{aList:aList},smalltalk.RAddTask)});},
messageSends: ["addTask:", "id", "task", "save:"]}),
smalltalk.RAddTask);


smalltalk.addMethod(
smalltalk.method({
selector: "for:in:",
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
messageSends: ["task:", "new", "list:", "yourself"]}),
smalltalk.RAddTask.klass);


smalltalk.addClass('RAddUser', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
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
messageSends: ["localSave", "ajax:", "->", ",", "path", "asJSONString", "onAfter:done:", "onFail:"]}),
smalltalk.RAddUser);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave",
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st($RStorage())._save_(_st(self)._user());
return self}, function($ctx1) {$ctx1.fill(self,"localSave",{},smalltalk.RAddUser)});},
messageSends: ["save:", "user"]}),
smalltalk.RAddUser);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
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
messageSends: ["user:", "new", "yourself"]}),
smalltalk.RAddUser.klass);


smalltalk.addClass('RChangeClients', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._notifyRemote();
return self}, function($ctx1) {$ctx1.fill(self,"executeDo:",{aBlock:aBlock},smalltalk.RChangeClients)});},
messageSends: ["notifyRemote"]}),
smalltalk.RChangeClients);

smalltalk.addMethod(
smalltalk.method({
selector: "react",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._app())._changeClients_(self);
return self}, function($ctx1) {$ctx1.fill(self,"react",{},smalltalk.RChangeClients)});},
messageSends: ["changeClients:", "app"]}),
smalltalk.RChangeClients);



smalltalk.addClass('RChangeList', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
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
messageSends: ["localSave", "notifyRemote", "ajax:", "->", ",", "path", "asJSONString", "onAfter:done:", "onFail:"]}),
smalltalk.RChangeList);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave",
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st($RStorage())._save_(_st(self)._list());
return self}, function($ctx1) {$ctx1.fill(self,"localSave",{},smalltalk.RChangeList)});},
messageSends: ["save:", "list"]}),
smalltalk.RChangeList);

smalltalk.addMethod(
smalltalk.method({
selector: "react",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._app())._changeList_(self);
return self}, function($ctx1) {$ctx1.fill(self,"react",{},smalltalk.RChangeList)});},
messageSends: ["changeList:", "app"]}),
smalltalk.RChangeList);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
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
messageSends: ["list:", "new", "yourself"]}),
smalltalk.RChangeList.klass);


smalltalk.addClass('RChangeTask', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
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
messageSends: ["localSave", "notifyRemote", "ajax:", "->", ",", "path", "asJSONString", "onAfter:done:", "onFail:"]}),
smalltalk.RChangeTask);

smalltalk.addMethod(
smalltalk.method({
selector: "localSave",
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st($RStorage())._save_(_st(self)._task());
return self}, function($ctx1) {$ctx1.fill(self,"localSave",{},smalltalk.RChangeTask)});},
messageSends: ["save:", "task"]}),
smalltalk.RChangeTask);

smalltalk.addMethod(
smalltalk.method({
selector: "react",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._app())._changeTask_(self);
return self}, function($ctx1) {$ctx1.fill(self,"react",{},smalltalk.RChangeTask)});},
messageSends: ["changeTask:", "app"]}),
smalltalk.RChangeTask);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
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
messageSends: ["task:", "new", "yourself"]}),
smalltalk.RChangeTask.klass);


smalltalk.addClass('RRemoveTask', smalltalk.RCommand, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "executeDo:",
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
messageSends: ["localRemove", "updateList:", "list", "notifyRemote", "ajax:", "->", ",", "path", "asJSONString", "onAfter:done:", "onFail:"]}),
smalltalk.RRemoveTask);

smalltalk.addMethod(
smalltalk.method({
selector: "localRemove",
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st($RStorage())._delete_(_st(self)._task());
return self}, function($ctx1) {$ctx1.fill(self,"localRemove",{},smalltalk.RRemoveTask)});},
messageSends: ["delete:", "task"]}),
smalltalk.RRemoveTask);

smalltalk.addMethod(
smalltalk.method({
selector: "react",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._app())._removeTask_(self);
return self}, function($ctx1) {$ctx1.fill(self,"react",{},smalltalk.RRemoveTask)});},
messageSends: ["removeTask:", "app"]}),
smalltalk.RRemoveTask);

smalltalk.addMethod(
smalltalk.method({
selector: "removeAndUpdateList:",
fn: function (aList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._localRemove();
_st(self)._updateList_(aList);
return self}, function($ctx1) {$ctx1.fill(self,"removeAndUpdateList:",{aList:aList},smalltalk.RRemoveTask)});},
messageSends: ["localRemove", "updateList:"]}),
smalltalk.RRemoveTask);

smalltalk.addMethod(
smalltalk.method({
selector: "updateList:",
fn: function (aList) {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st(aList)._removeTask_(_st(_st(self)._task())._id());
_st($RStorage())._save_(aList);
return self}, function($ctx1) {$ctx1.fill(self,"updateList:",{aList:aList},smalltalk.RRemoveTask)});},
messageSends: ["removeTask:", "id", "task", "save:"]}),
smalltalk.RRemoveTask);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
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
messageSends: ["task:", "new", "yourself"]}),
smalltalk.RRemoveTask.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "for:list:",
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
messageSends: ["task:", "new", "list:", "yourself"]}),
smalltalk.RRemoveTask.klass);


smalltalk.addClass('RList', smalltalk.RModel, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "addTask:",
fn: function (aRTaskId) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._tasks())._add_(aRTaskId);
return self}, function($ctx1) {$ctx1.fill(self,"addTask:",{aRTaskId:aRTaskId},smalltalk.RList)});},
messageSends: ["add:", "tasks"]}),
smalltalk.RList);

smalltalk.addMethod(
smalltalk.method({
selector: "removeTask:",
fn: function (aRTaskId) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._tasks())._remove_ifAbsent_(aRTaskId,(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"removeTask:",{aRTaskId:aRTaskId},smalltalk.RList)});},
messageSends: ["remove:ifAbsent:", "tasks"]}),
smalltalk.RList);

smalltalk.addMethod(
smalltalk.method({
selector: "tasks",
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
messageSends: ["ifNil:", "tasks:", "new", "tasks"]}),
smalltalk.RList);


smalltalk.addMethod(
smalltalk.method({
selector: "atId:do:",
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
messageSends: ["ajax:", "->", ",", "path", "onAfterRead:done:", "signal:", "responseText", "name"]}),
smalltalk.RList.klass);


smalltalk.addClass('RProspect', smalltalk.RModel, [], 'Reactive');


smalltalk.addClass('RTask', smalltalk.RModel, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
smalltalk.RModel.fn.prototype._initialize.apply(_st(self), []);
_st(self)._isCompleted_(false);
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.RTask)});},
messageSends: ["initialize", "isCompleted:"]}),
smalltalk.RTask);



smalltalk.addClass('RUser', smalltalk.RModel, [], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "addList:",
fn: function (aRList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._lists())._add_(aRList);
return self}, function($ctx1) {$ctx1.fill(self,"addList:",{aRList:aRList},smalltalk.RUser)});},
messageSends: ["add:", "lists"]}),
smalltalk.RUser);

smalltalk.addMethod(
smalltalk.method({
selector: "lists",
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
messageSends: ["ifNil:", "lists:", "new", "lists"]}),
smalltalk.RUser);



smalltalk.addClass('RSession', smalltalk.Object, ['id', 'user', 'api', 'social'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "api",
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
messageSends: ["ifNil:", "initializeAPI"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "asJSONString",
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
messageSends: ["at:put:", "id", "new", "user", "asJSONString"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "getUser",
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
messageSends: ["getItem:", "localStorage", "ifTrue:ifFalse:", "new", "id:", "name:", "yourself", "listId:", "id", "setItem:put:", "execute", "for:", "fromJsonString:", "isNil"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "id",
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
messageSends: ["ifNil:", "initializeID"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeAPI",
fn: function () {
var self=this;
function $RClient(){return smalltalk.RClient||(typeof RClient=="undefined"?nil:RClient)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@api"]=_st($RClient())._new();
$1=self["@api"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeAPI",{},smalltalk.RSession)});},
messageSends: ["new"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeID",
fn: function () {
var self=this;
function $MaplessModel(){return smalltalk.MaplessModel||(typeof MaplessModel=="undefined"?nil:MaplessModel)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@id"]=_st($MaplessModel())._newUUID();
$1=self["@id"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeID",{},smalltalk.RSession)});},
messageSends: ["newUUID"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeSocial",
fn: function () {
var self=this;
function $SocialAPI(){return smalltalk.SocialAPI||(typeof SocialAPI=="undefined"?nil:SocialAPI)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@social"]=_st($SocialAPI())._new();
$1=self["@social"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeSocial",{},smalltalk.RSession)});},
messageSends: ["new"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeUser",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@user"]=_st(self)._getUser();
$1=self["@user"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeUser",{},smalltalk.RSession)});},
messageSends: ["getUser"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "onOpen",
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
messageSends: ["initializeID", "appId:", "facebook", "social", "channelUrl:", "yourself", "linkedIn", "googlePlus"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "social",
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
messageSends: ["ifNil:", "initializeSocial"]}),
smalltalk.RSession);

smalltalk.addMethod(
smalltalk.method({
selector: "user",
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
messageSends: ["ifNil:", "initializeUser"]}),
smalltalk.RSession);



smalltalk.addClass('RStorage', smalltalk.Object, [], 'Reactive');

smalltalk.addMethod(
smalltalk.method({
selector: "delete:",
fn: function (aMaplessModel) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aMaplessModel)._localDelete();
return self}, function($ctx1) {$ctx1.fill(self,"delete:",{aMaplessModel:aMaplessModel},smalltalk.RStorage.klass)});},
messageSends: ["localDelete"]}),
smalltalk.RStorage.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "delete:do:",
fn: function (aMaplessModel, aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aMaplessModel)._remoteDeleteDo_((function(){
return smalltalk.withContext(function($ctx2) {
return _st(aMaplessModel)._localDelete();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"delete:do:",{aMaplessModel:aMaplessModel,aBlock:aBlock},smalltalk.RStorage.klass)});},
messageSends: ["remoteDeleteDo:", "localDelete"]}),
smalltalk.RStorage.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "save:",
fn: function (aMaplessModel) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(aMaplessModel)._localSave();
return self}, function($ctx1) {$ctx1.fill(self,"save:",{aMaplessModel:aMaplessModel},smalltalk.RStorage.klass)});},
messageSends: ["localSave"]}),
smalltalk.RStorage.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "save:do:",
fn: function (aMaplessModel, aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=aMaplessModel;
_st($1)._localSave();
$2=_st($1)._remoteSaveDo_(aBlock);
return self}, function($ctx1) {$ctx1.fill(self,"save:do:",{aMaplessModel:aMaplessModel,aBlock:aBlock},smalltalk.RStorage.klass)});},
messageSends: ["localSave", "remoteSaveDo:"]}),
smalltalk.RStorage.klass);


smalltalk.addClass('RWidget', smalltalk.Widget, ['model', 'announcer'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "announce:",
fn: function (anAnnouncement) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._announcer())._announce_(anAnnouncement);
return $1;
}, function($ctx1) {$ctx1.fill(self,"announce:",{anAnnouncement:anAnnouncement},smalltalk.RWidget)});},
messageSends: ["announce:", "announcer"]}),
smalltalk.RWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "announcer",
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
messageSends: ["ifNil:", "initializeAnnouncer"]}),
smalltalk.RWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeAnnouncer",
fn: function () {
var self=this;
function $Announcer(){return smalltalk.Announcer||(typeof Announcer=="undefined"?nil:Announcer)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@announcer"]=_st($Announcer())._new();
$1=self["@announcer"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeAnnouncer",{},smalltalk.RWidget)});},
messageSends: ["new"]}),
smalltalk.RWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "model",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@model"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"model",{},smalltalk.RWidget)});},
messageSends: []}),
smalltalk.RWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "model:",
fn: function (aModel) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@model"]=aModel;
return self}, function($ctx1) {$ctx1.fill(self,"model:",{aModel:aModel},smalltalk.RWidget)});},
messageSends: []}),
smalltalk.RWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "on:do:",
fn: function (anAnnouncement, aReaction) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._announcer())._on_do_(anAnnouncement,aReaction);
return $1;
}, function($ctx1) {$ctx1.fill(self,"on:do:",{anAnnouncement:anAnnouncement,aReaction:aReaction},smalltalk.RWidget)});},
messageSends: ["on:do:", "announcer"]}),
smalltalk.RWidget);


smalltalk.addMethod(
smalltalk.method({
selector: "for:",
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
messageSends: ["model:", "new", "yourself"]}),
smalltalk.RWidget.klass);


smalltalk.addClass('ListWidget', smalltalk.RWidget, ['list', 'input', 'items'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "addTask",
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
messageSends: ["initializeCreatedOn", "new", "description:", "val", "asJQuery", "yourself", "execute", "for:in:", "for:", "add:", "items", "renderTask:", "updateTasksMetric"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "addTask:",
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
messageSends: ["saveAndUpdateList:", "newTaskFor:", "task", "add:", "items", "renderTask:", "hide", "ifTrue:", "show", "isAllOrToDo", "updateTasksMetric"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "changeList:",
fn: function (aChageList) {
var self=this;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
return smalltalk.withContext(function($ctx1) { 
_st(self)._model_(_st(aChageList)._list());
_st(_st(_st(self["@list"])._asJQuery())._children_("li"))._remove();
_st(self)._initializeItems();
_st(self)._renderItemsOn_(_st($HTMLCanvas())._onJQuery_(_st(self["@list"])._asJQuery()));
return self}, function($ctx1) {$ctx1.fill(self,"changeList:",{aChageList:aChageList},smalltalk.ListWidget)});},
messageSends: ["model:", "list", "remove", "children:", "asJQuery", "initializeItems", "renderItemsOn:", "onJQuery:"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "changeTask:",
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
messageSends: ["getTaskWidgetFor:", "task", "ifNotNil:", "changedTask:", "ifTrue:", "showIfDone", "isDone", "showIfToDo", "isToDo", "updateTasksMetric"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "currentTaskIds",
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
messageSends: ["children", "asJQuery", "new", "to:do:", "-", "length", "add:", "id", "at:"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getAll",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._items();
return $1;
}, function($ctx1) {$ctx1.fill(self,"getAll",{},smalltalk.ListWidget)});},
messageSends: ["items"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getDone",
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
messageSends: ["select:", "isCompleted", "getAll"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getTaskAt:",
fn: function (anId) {
var self=this;
function $MaplessModel(){return smalltalk.MaplessModel||(typeof MaplessModel=="undefined"?nil:MaplessModel)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($MaplessModel())._localLoadAt_(anId);
return $1;
}, function($ctx1) {$ctx1.fill(self,"getTaskAt:",{anId:anId},smalltalk.ListWidget)});},
messageSends: ["localLoadAt:"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getTaskWidgetFor:",
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
messageSends: ["detect:ifNone:", "=", "id", "model", "items"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getTasks",
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
messageSends: ["collect:", "ifTrue:ifFalse:", "getTaskAt:", "fromJsonString:", "stringify:", "isString", "tasks"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getToDo",
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
messageSends: ["reject:", "isCompleted", "getAll"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeItems",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@items"]=_st(self)._makeItems();
$1=self["@items"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeItems",{},smalltalk.ListWidget)});},
messageSends: ["makeItems"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isAll",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st("#all")._asJQuery())._is_(".selectedTab");
return $1;
}, function($ctx1) {$ctx1.fill(self,"isAll",{},smalltalk.ListWidget)});},
messageSends: ["is:", "asJQuery"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isAllOrToDo",
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
messageSends: ["or:", "isToDo", "isAll"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isDone",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st("#done")._asJQuery())._is_(".selectedTab");
return $1;
}, function($ctx1) {$ctx1.fill(self,"isDone",{},smalltalk.ListWidget)});},
messageSends: ["is:", "asJQuery"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isToDo",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st("#todo")._asJQuery())._is_(".selectedTab");
return $1;
}, function($ctx1) {$ctx1.fill(self,"isToDo",{},smalltalk.ListWidget)});},
messageSends: ["is:", "asJQuery"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "items",
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
messageSends: ["ifNil:", "initializeItems"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "makeItems",
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
messageSends: ["collect:", "newTaskFor:", "getTasks"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "newTaskFor:",
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
messageSends: ["on:do:", "onTaskChanged:", "for:", "onTaskRemove:", "yourself"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onAdd",
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
messageSends: ["ifFalse:", "addTask", "val:", "asJQuery", "focus", "or:", "isEmpty", "val", "isNil"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onAllTasks",
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
messageSends: ["do:", "show", "items", "unselectTabs", "addClass:", "asJQuery", "updateTasksMetric"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onDoneTasks",
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
messageSends: ["do:", "showIfDone", "items", "unselectTabs", "addClass:", "asJQuery", "updateTasksMetric"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onKeyUp:",
fn: function (e) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(e)._keyCode()).__eq((13));
if(smalltalk.assert($1)){
_st(self)._onAdd();
};
return self}, function($ctx1) {$ctx1.fill(self,"onKeyUp:",{e:e},smalltalk.ListWidget)});},
messageSends: ["ifTrue:", "onAdd", "=", "keyCode"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onSorted:",
fn: function (anEvent) {
var self=this;
function $RChangeList(){return smalltalk.RChangeList||(typeof RChangeList=="undefined"?nil:RChangeList)}
function $RTasksSorted(){return smalltalk.RTasksSorted||(typeof RTasksSorted=="undefined"?nil:RTasksSorted)}
return smalltalk.withContext(function($ctx1) { 
_st(self["@model"])._tasks_(_st(self)._currentTaskIds());
_st(_st($RChangeList())._for_(self["@model"]))._execute();
_st(self)._announce_(_st($RTasksSorted())._for_(self));
return self}, function($ctx1) {$ctx1.fill(self,"onSorted:",{anEvent:anEvent},smalltalk.ListWidget)});},
messageSends: ["tasks:", "currentTaskIds", "execute", "for:", "announce:"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onTaskChanged:",
fn: function (ann) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._updateTasksMetric();
return self}, function($ctx1) {$ctx1.fill(self,"onTaskChanged:",{ann:ann},smalltalk.ListWidget)});},
messageSends: ["updateTasksMetric"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onTaskRemove:",
fn: function (ann) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self["@model"])._removeTask_(_st(_st(_st(ann)._subject())._model())._id());
_st(self["@model"])._localSave();
return self}, function($ctx1) {$ctx1.fill(self,"onTaskRemove:",{ann:ann},smalltalk.ListWidget)});},
messageSends: ["removeTask:", "id", "model", "subject", "localSave"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onToDoTasks",
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
messageSends: ["do:", "showIfToDo", "items", "unselectTabs", "addClass:", "asJQuery", "updateTasksMetric"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "removeTask:",
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
messageSends: ["getTaskWidgetFor:", "task", "removeAndUpdateList:", "ifNotNil:", "remove", "remove:ifAbsent:", "items", "updateTasksMetric"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "render",
fn: function () {
var self=this;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
return smalltalk.withContext(function($ctx1) { 
_st(self)._renderOn_(_st($HTMLCanvas())._onJQuery_(_st("#wrapper")._asJQuery()));
_st(_st("#wrapper")._asJQuery())._css_val_("min-height","50px");
return self}, function($ctx1) {$ctx1.fill(self,"render",{},smalltalk.ListWidget)});},
messageSends: ["renderOn:", "onJQuery:", "asJQuery", "css:val:"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderAddTaskOn:",
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
messageSends: ["class:", "div", "with:", "id:", "at:put:", "input", "onKeyUp:", "yourself", "a", "onClick:", "onAdd"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderItemsOn:",
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
messageSends: ["with:", "do:", "renderOn:", "ifTrue:", "showIfDone", "isDone", "showIfToDo", "isToDo", "items"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderListHeaderOn:",
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
messageSends: ["id:", "div", "with:", "name", "renderMetricsOn:", "renderTabsOn:"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderListOn:",
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
messageSends: ["ul", "disableSelection", "asJQuery", "sortable:", "->", "onSorted:", "renderItemsOn:"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderMetricsOn:",
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
messageSends: ["id:", "div", "with:", "span"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderOn:",
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
messageSends: ["id:", "div", "with:", "renderListHeaderOn:", "renderListOn:", "renderAddTaskOn:", "onAllTasks"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderTabsOn:",
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
messageSends: ["id:", "div", "class:", "onClick:", "onDoneTasks", "with:", "span:", "onToDoTasks", "onAllTasks"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderTask:",
fn: function (aTaskWidget) {
var self=this;
var html;
function $HTMLCanvas(){return smalltalk.HTMLCanvas||(typeof HTMLCanvas=="undefined"?nil:HTMLCanvas)}
return smalltalk.withContext(function($ctx1) { 
html=_st($HTMLCanvas())._onJQuery_(_st(self["@list"])._asJQuery());
_st(aTaskWidget)._renderOn_(html);
return self}, function($ctx1) {$ctx1.fill(self,"renderTask:",{aTaskWidget:aTaskWidget,html:html},smalltalk.ListWidget)});},
messageSends: ["onJQuery:", "asJQuery", "renderOn:"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "unselectTabs",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(_st("#details")._asJQuery())._find_(".tab"))._removeClass_("selectedTab");
return self}, function($ctx1) {$ctx1.fill(self,"unselectTabs",{},smalltalk.ListWidget)});},
messageSends: ["removeClass:", "find:", "asJQuery"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateAllTasksMetric",
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
messageSends: ["size", "getAll", "ifTrue:", "text:", "asJQuery", "isZero", "ifTrue:ifFalse:", ",", "asString", ">"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateDoneTasksMetric",
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
messageSends: ["size", "getDone", "ifTrue:", "text:", "asJQuery", "isZero", ",", "asString", "=", "getAll", "ifTrue:ifFalse:", ">"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateTasksMetric",
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
messageSends: ["ifTrue:", "updateDoneTasksMetric", "isDone", "updateToDoTasksMetric", "isToDo", "updateAllTasksMetric", "isAll"]}),
smalltalk.ListWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateToDoTasksMetric",
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
messageSends: ["size", "getToDo", "ifTrue:", "text:", "asJQuery", "isZero", "ifTrue:ifFalse:", ",", "asString", ">"]}),
smalltalk.ListWidget);



smalltalk.addClass('ReactiveWidget', smalltalk.RWidget, ['list', 'session'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "addTask:",
fn: function (anAddTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._list())._addTask_(anAddTask);
return self}, function($ctx1) {$ctx1.fill(self,"addTask:",{anAddTask:anAddTask},smalltalk.ReactiveWidget)});},
messageSends: ["addTask:", "list"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "changeClients:",
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
messageSends: ["clients", "ifTrue:ifFalse:", "text:", ",", "asString", "asJQuery", ">", "effect:op:duration:", "->"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "changeList:",
fn: function (aChageList) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._list())._changeList_(aChageList);
return self}, function($ctx1) {$ctx1.fill(self,"changeList:",{aChageList:aChageList},smalltalk.ReactiveWidget)});},
messageSends: ["changeList:", "list"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "changeTask:",
fn: function (aChangeTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._list())._changeTask_(aChangeTask);
return self}, function($ctx1) {$ctx1.fill(self,"changeTask:",{aChangeTask:aChangeTask},smalltalk.ReactiveWidget)});},
messageSends: ["changeTask:", "list"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "getListAt:",
fn: function (anId) {
var self=this;
function $MaplessModel(){return smalltalk.MaplessModel||(typeof MaplessModel=="undefined"?nil:MaplessModel)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($MaplessModel())._localLoadAt_(anId);
return $1;
}, function($ctx1) {$ctx1.fill(self,"getListAt:",{anId:anId},smalltalk.ReactiveWidget)});},
messageSends: ["localLoadAt:"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "hasTip",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(window)._localStorage())._getItem_("showTipAtStart"))._isNil();
return $1;
}, function($ctx1) {$ctx1.fill(self,"hasTip",{},smalltalk.ReactiveWidget)});},
messageSends: ["isNil", "getItem:", "localStorage"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeListDo:",
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
messageSends: ["new", "atId:do:", "model:", "do:", "localSave", "fromJsonString:", "stringify:", "tasks", "value", "on:do:", "onListChanged:", "onTaskRemoved:", "yourself"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeSession",
fn: function () {
var self=this;
function $RSession(){return smalltalk.RSession||(typeof RSession=="undefined"?nil:RSession)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@session"]=_st($RSession())._new();
$1=self["@session"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeSession",{},smalltalk.ReactiveWidget)});},
messageSends: ["new"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isLocalhost",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(window)._location())._hostname())._match_("localhost");
return $1;
}, function($ctx1) {$ctx1.fill(self,"isLocalhost",{},smalltalk.ReactiveWidget)});},
messageSends: ["match:", "hostname", "location"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isProduction",
fn: function () {
var self=this;
function $Browser(){return smalltalk.Browser||(typeof Browser=="undefined"?nil:Browser)}
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st($Browser())._isNil();
return $1;
}, function($ctx1) {$ctx1.fill(self,"isProduction",{},smalltalk.ReactiveWidget)});},
messageSends: ["isNil"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isPublished",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._isLocalhost())._not();
return $1;
}, function($ctx1) {$ctx1.fill(self,"isPublished",{},smalltalk.ReactiveWidget)});},
messageSends: ["not", "isLocalhost"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isSociallyLoggedIn",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
return false;
}, function($ctx1) {$ctx1.fill(self,"isSociallyLoggedIn",{},smalltalk.ReactiveWidget)});},
messageSends: []}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "list",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@list"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"list",{},smalltalk.ReactiveWidget)});},
messageSends: []}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "listDo:",
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
messageSends: ["ifNil:ifNotNil:", "initializeListDo:"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "loadFacebookSDK",
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
messageSends: []}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onGotIt",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._localStorage())._setItem_put_("showTipAtStart",false);
return self}, function($ctx1) {$ctx1.fill(self,"onGotIt",{},smalltalk.ReactiveWidget)});},
messageSends: ["setItem:put:", "localStorage"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onListChanged:",
fn: function (ann) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self}, function($ctx1) {$ctx1.fill(self,"onListChanged:",{ann:ann},smalltalk.ReactiveWidget)});},
messageSends: []}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onNay",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._asJQuery())._scrollTop_((0));
_st(_st("#nayWrapper")._asJQuery())._slideDown_(smalltalk.HashedCollection._fromPairs_([_st("complete").__minus_gt((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st(_st("#feedback")._asJQuery())._find_("textarea"))._focus();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"onNay",{},smalltalk.ReactiveWidget)});},
messageSends: ["scrollTop:", "asJQuery", "slideDown:", "->", "focus", "find:"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onNayKeyUp:",
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
messageSends: ["ifTrue:", "slideUp", "asJQuery", "=", "keyCode", "onOkNay"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onNayTextAreaKeyUp:",
fn: function (e) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(e)._keyCode()).__eq((27));
if(smalltalk.assert($1)){
_st(_st("#nayWrapper")._asJQuery())._slideUp();
};
return self}, function($ctx1) {$ctx1.fill(self,"onNayTextAreaKeyUp:",{e:e},smalltalk.ReactiveWidget)});},
messageSends: ["ifTrue:", "slideUp", "asJQuery", "=", "keyCode"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onOkNay",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st("#nayWrapper")._asJQuery())._slideUp();
_st(self)._sendFeedback();
return self}, function($ctx1) {$ctx1.fill(self,"onOkNay",{},smalltalk.ReactiveWidget)});},
messageSends: ["slideUp", "asJQuery", "sendFeedback"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onOkYay",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st("#yayWrapper")._asJQuery())._slideUp();
_st(self)._sendEmail();
return self}, function($ctx1) {$ctx1.fill(self,"onOkYay",{},smalltalk.ReactiveWidget)});},
messageSends: ["slideUp", "asJQuery", "sendEmail"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onOpen",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st("#loader")._asJQuery())._remove();
_st(_st(window)._document())._title_("Let's do this!");
_st(window)._at_put_("app",self);
_st(_st(self)._session())._onOpen();
_st(self)._model_(_st(_st(self)._session())._user());
return self}, function($ctx1) {$ctx1.fill(self,"onOpen",{},smalltalk.ReactiveWidget)});},
messageSends: ["remove", "asJQuery", "title:", "document", "at:put:", "onOpen", "session", "model:", "user"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onTaskRemoved:",
fn: function (ann) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self}, function($ctx1) {$ctx1.fill(self,"onTaskRemoved:",{ann:ann},smalltalk.ReactiveWidget)});},
messageSends: []}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onYay",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(window)._asJQuery())._scrollTop_((0));
_st(_st("#yayWrapper")._asJQuery())._slideDown_(smalltalk.HashedCollection._fromPairs_([_st("complete").__minus_gt((function(){
return smalltalk.withContext(function($ctx2) {
return _st(_st("#yayEmail")._asJQuery())._focus();
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}))]));
return self}, function($ctx1) {$ctx1.fill(self,"onYay",{},smalltalk.ReactiveWidget)});},
messageSends: ["scrollTop:", "asJQuery", "slideDown:", "->", "focus"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onYayKeyUp:",
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
messageSends: ["ifTrue:", "slideUp", "asJQuery", "=", "keyCode", "onOkYay"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "open",
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
messageSends: ["onJQuery:", "asJQuery", "onOpen", "onOpen:", "renderOn:", "valueWithTimeout:", "execute", "new", "api", "session", "open"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "openBrowser",
fn: function () {
var self=this;
function $Browser(){return smalltalk.Browser||(typeof Browser=="undefined"?nil:Browser)}
return smalltalk.withContext(function($ctx1) { 
_st($Browser())._open();
return self}, function($ctx1) {$ctx1.fill(self,"openBrowser",{},smalltalk.ReactiveWidget)});},
messageSends: ["open"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "removeTask:",
fn: function (aRemoveTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self)._list())._removeTask_(aRemoveTask);
return self}, function($ctx1) {$ctx1.fill(self,"removeTask:",{aRemoveTask:aRemoveTask},smalltalk.ReactiveWidget)});},
messageSends: ["removeTask:", "list"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderAmberOn:",
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
messageSends: ["onJQuery:", "asJQuery", "id:", "a", "onClick:", "openBrowser", "with:"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderFacebookLikeOn:",
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
messageSends: ["class:", "div", "at:put:", "yourself"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderFooterOn:",
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
messageSends: ["id:", "div", "with:", "renderSocialLoginOn:", "renderLikeOn:", "class:", "img", "src:", "p", "span:", "href:", "a", "target:"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderHeaderOn:",
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
messageSends: ["ifTrue:", "isProduction", "id:", "div", "with:", "renderAmberOn:"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderLikeOn:",
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
messageSends: ["class:", "p", "with:", "span:", "a", "onClick:", "onYay", "onNay"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderListFooterOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
return self}, function($ctx1) {$ctx1.fill(self,"renderListFooterOn:",{html:html},smalltalk.ReactiveWidget)});},
messageSends: []}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderListOn:",
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
messageSends: ["id:", "div", "with:", "src:", "img", "listDo:", "remove", "asJQuery", "text:", "render", "list", "renderListFooterOn:"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderNayOn:",
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
messageSends: ["id:", "div", "onClick:", "slideUp", "asJQuery", "with:", "src:", "img", "class:", "a", "target:", "href:", "h2", "span:", "p", "label", "onKeyUp:", "onNayTextAreaKeyUp:", "textarea", "at:put:", "yourself", "input", "onNayKeyUp:", "onOkNay"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderOn:",
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
messageSends: ["renderHeaderOn:", "renderTipOn:", "renderTitleOn:", "id:", "div", "with:", "renderListOn:", "renderFooterOn:", "renderYayOn:", "renderNayOn:"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderSocialLoginOn:",
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
messageSends: ["ifTrue:", "isSociallyLoggedIn", "id:", "div", "load", "facebook", "social", "session", "isPublished", "at:put:", "script", "with:", "renderLoadOn:", "linkedIn"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderTipOn:",
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
messageSends: ["ifTrue:", "valueWithTimeout:", "slideDown:", "->", "slideUp", "asJQuery", "hasTip", "id:", "div", "onClick:", "with:", "src:", "img", "h2", "span:", "p", "class:", "a", "onGotIt"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderTitleOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st("#mainTitle")._asJQuery())._text_("Let's do...");
return self}, function($ctx1) {$ctx1.fill(self,"renderTitleOn:",{html:html},smalltalk.ReactiveWidget)});},
messageSends: ["text:", "asJQuery"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderYayOn:",
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
messageSends: ["id:", "div", "onClick:", "slideUp", "asJQuery", "with:", "src:", "img", "class:", "a", "target:", "href:", "h2", "span:", "p", "p:", "strong:", "input", "at:put:", "onKeyUp:", "onYayKeyUp:", "yourself", "label", "onOkYay", "setGooglePlus"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "sendEmail",
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
messageSends: ["val", "asJQuery", "ifTrue:", "isEmpty", "email:", "new", "yourself", "ajax:", "->", "asJSONString"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "sendFeedback",
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
messageSends: ["val", "find:", "asJQuery", "ifTrue:", "isEmpty", "feedback:", "new", "email:", "yourself", "ajax:", "->", "asJSONString"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "session",
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
messageSends: ["ifNil:", "initializeSession"]}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "setFacebookPageLike",
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
messageSends: []}),
smalltalk.ReactiveWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "setGooglePlus",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
(function() {
    var po = document.createElement('script'); po.type = 'text/javascript'; po.async = true;
    po.src = 'https://apis.google.com/js/plusone.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(po, s);
  })();;
return self}, function($ctx1) {$ctx1.fill(self,"setGooglePlus",{},smalltalk.ReactiveWidget)});},
messageSends: []}),
smalltalk.ReactiveWidget);


smalltalk.addMethod(
smalltalk.method({
selector: "open",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._new())._open();
return $1;
}, function($ctx1) {$ctx1.fill(self,"open",{},smalltalk.ReactiveWidget.klass)});},
messageSends: ["open", "new"]}),
smalltalk.ReactiveWidget.klass);


smalltalk.addClass('TaskWidget', smalltalk.RWidget, ['listItem', 'view', 'editor'], 'Reactive');
smalltalk.addMethod(
smalltalk.method({
selector: "changedTask:",
fn: function (aTask) {
var self=this;
function $RChangeTask(){return smalltalk.RChangeTask||(typeof RChangeTask=="undefined"?nil:RChangeTask)}
return smalltalk.withContext(function($ctx1) { 
_st(self)._updateTask_(aTask);
_st(self)._model_(aTask);
_st(_st($RChangeTask())._for_(self["@model"]))._localSave();
return self}, function($ctx1) {$ctx1.fill(self,"changedTask:",{aTask:aTask},smalltalk.TaskWidget)});},
messageSends: ["updateTask:", "model:", "localSave", "for:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "editor",
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
messageSends: ["ifNil:", "initializeEditor"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "hide",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self["@listItem"])._asJQuery())._hide();
return self}, function($ctx1) {$ctx1.fill(self,"hide",{},smalltalk.TaskWidget)});},
messageSends: ["hide", "asJQuery"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeEditor",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@editor"]=_st(self)._makeEditor();
$1=self["@editor"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeEditor",{},smalltalk.TaskWidget)});},
messageSends: ["makeEditor"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "isCompleted",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(self["@listItem"])._asJQuery())._find_("input"))._is_(":checked");
return $1;
}, function($ctx1) {$ctx1.fill(self,"isCompleted",{},smalltalk.TaskWidget)});},
messageSends: ["is:", "find:", "asJQuery"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "makeEditor",
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
messageSends: ["onJQuery:", "asJQuery", "renderEditorOn:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onCancel",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(_st(self)._editor())._asJQuery())._hide();
_st(_st(self["@view"])._asJQuery())._show();
return self}, function($ctx1) {$ctx1.fill(self,"onCancel",{},smalltalk.TaskWidget)});},
messageSends: ["hide", "asJQuery", "editor", "show"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onCompletionCheckboxClicked:",
fn: function (anEvent) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(anEvent)._stopPropagation();
_st(self)._setCompletion();
return self}, function($ctx1) {$ctx1.fill(self,"onCompletionCheckboxClicked:",{anEvent:anEvent},smalltalk.TaskWidget)});},
messageSends: ["stopPropagation", "setCompletion"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onCompletionClicked:",
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
messageSends: ["ifTrue:ifFalse:", "prop:put:", "find:", "asJQuery", "attr:put:", "is:", "setCompletion"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onDescriptionClicked",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self["@view"])._asJQuery())._hide();
_st(_st(_st(_st(self)._editor())._asJQuery())._find_("input"))._val_(_st(self["@model"])._description());
_st(_st(_st(self)._editor())._asJQuery())._show();
_st(_st(_st(_st(self)._editor())._asJQuery())._find_("input"))._focus();
return self}, function($ctx1) {$ctx1.fill(self,"onDescriptionClicked",{},smalltalk.TaskWidget)});},
messageSends: ["hide", "asJQuery", "val:", "description", "find:", "editor", "show", "focus"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onDone",
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
messageSends: ["hide", "asJQuery", "editor", "description:", "val", "find:", "execute", "for:", "text:", "description", "show", "effect:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onKeyUp:",
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
messageSends: ["ifTrue:", "onDone", "=", "keyCode", "onCancel"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "onRemove:",
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
messageSends: ["stopPropagation", "remove", "execute", "for:list:", "model", "list", "app", "announce:", "for:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "remove",
fn: function () {
var self=this;
function $RStorage(){return smalltalk.RStorage||(typeof RStorage=="undefined"?nil:RStorage)}
return smalltalk.withContext(function($ctx1) { 
_st(_st(self["@listItem"])._asJQuery())._remove();
_st($RStorage())._delete_(self["@model"]);
return self}, function($ctx1) {$ctx1.fill(self,"remove",{},smalltalk.TaskWidget)});},
messageSends: ["remove", "asJQuery", "delete:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderCompletionOldOn:",
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
messageSends: ["class:", "div", "with:", "input", "type:", "at:put:", "id"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderCompletionOn:",
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
messageSends: ["class:", "div", "onClick:", "onCompletionClicked:", "with:", "onCompletionCheckboxClicked:", "input", "type:", "at:put:", "id"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderCompletionTableOn:",
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
messageSends: ["class:", "table", "with:", "type:", "input", "at:put:", "id", "td", "tr"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderCompletionWrappedOn:",
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
messageSends: ["class:", "div", "with:", "input", "type:", "at:put:", "id"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderEditorOn:",
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
messageSends: ["class:", "div", "with:", "input", "onKeyUp:", "description"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderOn:",
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
messageSends: ["id:", "id", "li", "class:", "with:", "renderCompletionOn:", "renderViewOn:", "updateTask:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "renderViewOn:",
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
messageSends: ["class:", "div", "with:", "onClick:", "onDescriptionClicked", "span", "description", "img", "onRemove:", "src:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "setCompletion",
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
messageSends: ["ifTrue:ifFalse:", "addClass:", "find:", "asJQuery", "removeClass:", "isCompleted", "isCompleted:", "execute", "for:", "announce:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "show",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(_st(self["@listItem"])._asJQuery())._show();
return self}, function($ctx1) {$ctx1.fill(self,"show",{},smalltalk.TaskWidget)});},
messageSends: ["show", "asJQuery"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "showIfDone",
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
messageSends: ["ifTrue:ifFalse:", "show", "hide", "isCompleted"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "showIfToDo",
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
messageSends: ["ifTrue:ifFalse:", "hide", "show", "isCompleted"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateTask:",
fn: function (aTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._updateTaskDescription_(aTask);
_st(self)._updateTaskCompletion_(aTask);
return self}, function($ctx1) {$ctx1.fill(self,"updateTask:",{aTask:aTask},smalltalk.TaskWidget)});},
messageSends: ["updateTaskDescription:", "updateTaskCompletion:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateTaskCompletion:",
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
messageSends: ["attr:put:", "isCompleted", "find:", "asJQuery", "ifTrue:ifFalse:", "addClass:", "removeClass:"]}),
smalltalk.TaskWidget);

smalltalk.addMethod(
smalltalk.method({
selector: "updateTaskDescription:",
fn: function (aTask) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self["@model"])._description_(_st(aTask)._description());
_st(_st(self["@view"])._asJQuery())._show();
_st(_st(self["@view"])._asJQuery())._effect_("highlight");
_st(_st(_st(self["@view"])._asJQuery())._find_(".taskDescription"))._text_(_st(self["@model"])._description());
return self}, function($ctx1) {$ctx1.fill(self,"updateTaskDescription:",{aTask:aTask},smalltalk.TaskWidget)});},
messageSends: ["description:", "description", "show", "asJQuery", "effect:", "text:", "find:"]}),
smalltalk.TaskWidget);



