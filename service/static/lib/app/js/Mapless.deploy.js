smalltalk.addPackage('Mapless');
smalltalk.addClass('MaplessModel', smalltalk.Object, ['data'], 'Mapless');
smalltalk.addMethod(
smalltalk.method({
selector: "=",
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
messageSends: ["and:", "=", "id", "respondsTo:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "asJSONString",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(self)._onAboutToJSON();
$1=_st((smalltalk.JSON || JSON))._stringify_(self["@data"]);
return $1;
}, function($ctx1) {$ctx1.fill(self,"asJSONString",{},smalltalk.MaplessModel)})},
messageSends: ["onAboutToJSON", "stringify:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "at:",
fn: function (aKey){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@data"])._at_(aKey);
return $1;
}, function($ctx1) {$ctx1.fill(self,"at:",{aKey:aKey},smalltalk.MaplessModel)})},
messageSends: ["at:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "at:ifAbsent:",
fn: function (aKey,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@data"])._at_ifAbsent_(aKey,aBlock);
return $1;
}, function($ctx1) {$ctx1.fill(self,"at:ifAbsent:",{aKey:aKey,aBlock:aBlock},smalltalk.MaplessModel)})},
messageSends: ["at:ifAbsent:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "at:put:",
fn: function (aKey,anObject){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
_st(self["@data"])._at_put_(aKey,anObject);
$1=anObject;
return $1;
}, function($ctx1) {$ctx1.fill(self,"at:put:",{aKey:aKey,anObject:anObject},smalltalk.MaplessModel)})},
messageSends: ["at:put:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "create",
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
messageSends: ["createDo:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "createDo:",
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
messageSends: ["ajax:", "->", "path", "asJSONString", "onAfterCreate:done:", "signal:", ",", "responseText", "name", "class"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "createdOn",
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
messageSends: ["at:", "asString", "ifNil:", "ifTrue:", "isKindOf:", "at:put:", "dateAndTimeAt:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "createdOn:",
fn: function (aDate){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._at_put_(smalltalk.symbolFor("createdOn"),aDate);
return self}, function($ctx1) {$ctx1.fill(self,"createdOn:",{aDate:aDate},smalltalk.MaplessModel)})},
messageSends: ["at:put:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "data",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@data"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"data",{},smalltalk.MaplessModel)})},
messageSends: []}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "dateAndTimeAt:",
fn: function (aSelector){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st((smalltalk.Date || Date))._fromString_(_st(self)._at_(aSelector));
return $1;
}, function($ctx1) {$ctx1.fill(self,"dateAndTimeAt:",{aSelector:aSelector},smalltalk.MaplessModel)})},
messageSends: ["fromString:", "at:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "delete",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._deleteDo_((function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"delete",{},smalltalk.MaplessModel)})},
messageSends: ["deleteDo:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "deleteDo:",
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
messageSends: ["ajax:", "->", ",", "asString", "id", "path", "asJSONString", "onAfterDelete:done:", "onDeleteFail:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "doesNotUnderstand:",
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
messageSends: ["asSymbol", "selector", "ifTrue:", "ifTrue:ifFalse:", "at:ifAbsent:", "asString", "at:", "isKindOf:", "ifNil:", "isUndefinedPart:", "isObjectPart:", "newJSObject", "on:", "at:put:", "isEmpty", "keys:", "get:from:", "current", "inspect", "error:", ",", "fromReified:", "isUnary:", "allButLast", "first", "arguments", "doesNotUnderstand:", "and:", "=", "occurrencesOf:", "isKeyword:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "get:from:",
fn: function (anAttribute,aPart){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return aPart[anAttribute];
return self}, function($ctx1) {$ctx1.fill(self,"get:from:",{anAttribute:anAttribute,aPart:aPart},smalltalk.MaplessModel)})},
messageSends: []}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "id",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self["@data"])._at_(smalltalk.symbolFor("id"));
return $1;
}, function($ctx1) {$ctx1.fill(self,"id",{},smalltalk.MaplessModel)})},
messageSends: ["at:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "id:",
fn: function (aString){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self["@data"])._at_put_(smalltalk.symbolFor("id"),aString);
return self}, function($ctx1) {$ctx1.fill(self,"id:",{aString:aString},smalltalk.MaplessModel)})},
messageSends: ["at:put:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "initialize",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
smalltalk.Object.fn.prototype._initialize.apply(_st(self), []);
self["@data"]=_st((smalltalk.HashedCollection || HashedCollection))._new();
_st(self)._modelClass_(_st(_st(self)._class())._name());
_st(self)._initializeInstanceVersion();
return self}, function($ctx1) {$ctx1.fill(self,"initialize",{},smalltalk.MaplessModel)})},
messageSends: ["initialize", "new", "modelClass:", "name", "class", "initializeInstanceVersion"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeInstanceVersion",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._v_((1));
return self}, function($ctx1) {$ctx1.fill(self,"initializeInstanceVersion",{},smalltalk.MaplessModel)})},
messageSends: ["v:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "isKeyword:",
fn: function (aSelector){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(aSelector)._asString())._includes_(":");
return $1;
}, function($ctx1) {$ctx1.fill(self,"isKeyword:",{aSelector:aSelector},smalltalk.MaplessModel)})},
messageSends: ["includes:", "asString"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "isObjectPart:",
fn: function (aPart){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return typeof part=='object';
return self}, function($ctx1) {$ctx1.fill(self,"isObjectPart:",{aPart:aPart},smalltalk.MaplessModel)})},
messageSends: []}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "isUnary:",
fn: function (aSelector){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(aSelector)._asString())._includes_(":"))._not();
return $1;
}, function($ctx1) {$ctx1.fill(self,"isUnary:",{aSelector:aSelector},smalltalk.MaplessModel)})},
messageSends: ["not", "includes:", "asString"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "isUndefinedPart:",
fn: function (aPart){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return aPart=='undefined';
return self}, function($ctx1) {$ctx1.fill(self,"isUndefinedPart:",{aPart:aPart},smalltalk.MaplessModel)})},
messageSends: []}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "newJSObject",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
return Object;
return self}, function($ctx1) {$ctx1.fill(self,"newJSObject",{},smalltalk.MaplessModel)})},
messageSends: []}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onAboutToJSON",
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
messageSends: ["newJSObject", "on:", "keys:", "do:", "at:", "perform:", "asSymbol", "ifTrue:", "onAboutToJSON", "data", "isKindOf:", "at:put:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onAfterCreate:done:",
fn: function (x,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@data"]=_st(_st(self)._class())._reify_(x);
_st(self)._announce_(_st((smalltalk.ModelCreated || ModelCreated))._for_(self));
_st(aBlock)._value_(self);
return self}, function($ctx1) {$ctx1.fill(self,"onAfterCreate:done:",{x:x,aBlock:aBlock},smalltalk.MaplessModel)})},
messageSends: ["reify:", "class", "announce:", "for:", "value:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onAfterDelete:done:",
fn: function (x,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._announce_(_st((smalltalk.ModelDeleted || ModelDeleted))._for_(self));
_st(aBlock)._value_(self);
return self}, function($ctx1) {$ctx1.fill(self,"onAfterDelete:done:",{x:x,aBlock:aBlock},smalltalk.MaplessModel)})},
messageSends: ["announce:", "for:", "value:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onAfterRefresh:done:",
fn: function (x,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._syncWith_(_st(_st(self)._class())._reify_(x));
_st(self)._announce_(_st((smalltalk.ModelRefreshed || ModelRefreshed))._for_(self));
_st(aBlock)._value_(self);
return self}, function($ctx1) {$ctx1.fill(self,"onAfterRefresh:done:",{x:x,aBlock:aBlock},smalltalk.MaplessModel)})},
messageSends: ["syncWith:", "reify:", "class", "announce:", "for:", "value:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onAfterSave:done:",
fn: function (x,aBlock){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._announce_(_st((smalltalk.ModelSaved || ModelSaved))._for_(self));
_st(aBlock)._value_(self);
return self}, function($ctx1) {$ctx1.fill(self,"onAfterSave:done:",{x:x,aBlock:aBlock},smalltalk.MaplessModel)})},
messageSends: ["announce:", "for:", "value:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onDeleteFail:",
fn: function (x){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st((smalltalk.ModelUpdateError || ModelUpdateError))._signal_(_st("Could not delete ").__comma(_st(_st(self)._class())._name()));
return self}, function($ctx1) {$ctx1.fill(self,"onDeleteFail:",{x:x},smalltalk.MaplessModel)})},
messageSends: ["signal:", ",", "name", "class"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onRefreshFail:",
fn: function (x){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st((smalltalk.ModelRefreshError || ModelRefreshError))._signal_(_st("Could not refresh ").__comma(_st(_st(self)._class())._name()));
return self}, function($ctx1) {$ctx1.fill(self,"onRefreshFail:",{x:x},smalltalk.MaplessModel)})},
messageSends: ["signal:", ",", "name", "class"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "onSaveFail:",
fn: function (x){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st((smalltalk.ModelUpdateError || ModelUpdateError))._signal_(_st("Could not save ").__comma(_st(_st(self)._class())._name()));
return self}, function($ctx1) {$ctx1.fill(self,"onSaveFail:",{x:x},smalltalk.MaplessModel)})},
messageSends: ["signal:", ",", "name", "class"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "path",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._class())._path();
return $1;
}, function($ctx1) {$ctx1.fill(self,"path",{},smalltalk.MaplessModel)})},
messageSends: ["path", "class"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "refresh",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._refreshDo_((function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"refresh",{},smalltalk.MaplessModel)})},
messageSends: ["refreshDo:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "refreshDo:",
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
messageSends: ["ajax:", "->", ",", "asString", "id", "path", "onAfterRefresh:done:", "onRefeshFail:", "onRefreshFail:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "save",
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
messageSends: ["saveDo:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "saveDo:",
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
return self}, function($ctx1) {$ctx1.fill(self,"saveDo:",{aBlock:aBlock},smalltalk.MaplessModel)})},
messageSends: ["ajax:", "->", "path", "asJSONString", "onAfterSave:done:", "onSaveFail:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "subModelAt:",
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
messageSends: ["at:", "ifNil:", "current", "signal:", ",", "asString", "fromReified:"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "syncFrom:",
fn: function (someJson){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._syncWith_(_st(_st(self)._class())._reify_(someJson));
return $1;
}, function($ctx1) {$ctx1.fill(self,"syncFrom:",{someJson:someJson},smalltalk.MaplessModel)})},
messageSends: ["syncWith:", "reify:", "class"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "syncWith:",
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
messageSends: ["ifNil:", "ifTrue:", "error:", "or:", "~=", "name", "class", "at:", "isNil"]}),
smalltalk.MaplessModel);

smalltalk.addMethod(
smalltalk.method({
selector: "url",
fn: function (){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(_st(self)._path()).__comma("/")).__comma(_st(_st(self)._id())._asString());
return $1;
}, function($ctx1) {$ctx1.fill(self,"url",{},smalltalk.MaplessModel)})},
messageSends: [",", "asString", "id", "path"]}),
smalltalk.MaplessModel);


smalltalk.addMethod(
smalltalk.method({
selector: "atId:do:",
fn: function (anId,onDone){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._read_do_(anId,onDone);
return $1;
}, function($ctx1) {$ctx1.fill(self,"atId:do:",{anId:anId,onDone:onDone},smalltalk.MaplessModel.klass)})},
messageSends: ["read:do:"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "create",
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
messageSends: ["createDo:"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "createDo:",
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
messageSends: ["initialize", "basicNew", "ajax:", "->", "path", "asJSONString", "onAfterCreate:done:", "signal:", ",", "responseText", "name"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "createdOnFrom:",
fn: function (aReifiedJSON){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st((smalltalk.Date || Date))._fromString_(aReifiedJSON);
return $1;
}, function($ctx1) {$ctx1.fill(self,"createdOnFrom:",{aReifiedJSON:aReifiedJSON},smalltalk.MaplessModel.klass)})},
messageSends: ["fromString:"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "fromJson:",
fn: function (someJson){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._new())._syncFrom_(someJson);
return $1;
}, function($ctx1) {$ctx1.fill(self,"fromJson:",{someJson:someJson},smalltalk.MaplessModel.klass)})},
messageSends: ["syncFrom:", "new"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "fromReified:",
fn: function (aReifiedJSON){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(_st(self)._new())._syncWith_(aReifiedJSON);
return $1;
}, function($ctx1) {$ctx1.fill(self,"fromReified:",{aReifiedJSON:aReifiedJSON},smalltalk.MaplessModel.klass)})},
messageSends: ["syncWith:", "new"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "manyFromJson:",
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
messageSends: ["collect:", "fromReified:", "parse:"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "onAfterRead:done:",
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
messageSends: ["fromJson:", "value:"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "read:",
fn: function (anId){
var self=this;
return smalltalk.withContext(function($ctx1) { 
_st(self)._read_do_(anId,(function(){
return smalltalk.withContext(function($ctx2) {
return nil;
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"read:",{anId:anId},smalltalk.MaplessModel.klass)})},
messageSends: ["read:do:"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "read:do:",
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
messageSends: ["ajax:", "->", ",", "path", "onAfterRead:done:", "signal:", "responseText", "name"]}),
smalltalk.MaplessModel.klass);

smalltalk.addMethod(
smalltalk.method({
selector: "reify:",
fn: function (someJson){
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st((smalltalk.JSON || JSON))._parse_(someJson);
return $1;
}, function($ctx1) {$ctx1.fill(self,"reify:",{someJson:someJson},smalltalk.MaplessModel.klass)})},
messageSends: ["parse:"]}),
smalltalk.MaplessModel.klass);


