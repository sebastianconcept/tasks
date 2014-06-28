smalltalk.addPackage('Flow-Models', {});
smalltalk.addClass('Model', smalltalk.Object, ['announcer'], 'Flow-Models');
smalltalk.Model.comment="Models are meant to be used as local-only (at the DOM) objects that should be transident in nature (not-persistent)"
smalltalk.addMethod(
"_announce_",
smalltalk.method({
selector: "announce:",
category: 'actions',
fn: function (anAnnouncement) {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_announcer", []), "_announce_", [anAnnouncement]);
    return self;
},
args: ["anAnnouncement"],
source: "announce: anAnnouncement\x0a\x0a\x09self announcer announce: anAnnouncement",
messageSends: ["announce:", "announcer"],
referencedClasses: []
}),
smalltalk.Model);

smalltalk.addMethod(
"_announcer",
smalltalk.method({
selector: "announcer",
category: 'accessing',
fn: function () {
    var self = this;
    return ($receiver = self['@announcer']) == nil || $receiver == undefined ? function () {return smalltalk.send(self, "_initializeAnnouncer", []);}() : $receiver;
    return self;
},
args: [],
source: "announcer\x0a\x0a\x09^ announcer ifNil:[self initializeAnnouncer]",
messageSends: ["ifNil:", "initializeAnnouncer"],
referencedClasses: []
}),
smalltalk.Model);

smalltalk.addMethod(
"_initializeAnnouncer",
smalltalk.method({
selector: "initializeAnnouncer",
category: 'initialization',
fn: function () {
    var self = this;
    return self['@announcer'] = smalltalk.send(smalltalk.Announcer || Announcer, "_new", []);
    return self;
},
args: [],
source: "initializeAnnouncer\x0a\x0a\x09^ announcer := Announcer new",
messageSends: ["new"],
referencedClasses: ["Announcer"]
}),
smalltalk.Model);

smalltalk.addMethod(
"_on_do_",
smalltalk.method({
selector: "on:do:",
category: 'actions',
fn: function (anAnnouncementClass, aReactionBlock) {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_announcer", []), "_on_do_", [anAnnouncementClass, aReactionBlock]);
    return self;
},
args: ["anAnnouncementClass", "aReactionBlock"],
source: "on: anAnnouncementClass do: aReactionBlock\x0a\x0a\x09^ self announcer on: anAnnouncementClass do: aReactionBlock",
messageSends: ["on:do:", "announcer"],
referencedClasses: []
}),
smalltalk.Model);


smalltalk.addMethod(
"_basePath",
smalltalk.method({
selector: "basePath",
category: 'accessing',
fn: function () {
    var self = this;
    return "api";
    return self;
},
args: [],
source: "basePath\x0a\x09\x22Answers the base path for the application's API.\x0a\x09Subclasses should override if appropriate.\x22\x0a\x0a\x09\x22By default we use the AmberMP's default\x22\x0a\x09^ 'api'",
messageSends: [],
referencedClasses: []
}),
smalltalk.Model.klass);

smalltalk.addMethod(
"_initialize",
smalltalk.method({
selector: "initialize",
category: 'initialization',
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.Package || Package, "_defaultCommitPathSt_", ["/flow/commit"]);
    smalltalk.send(smalltalk.Package || Package, "_defaultCommitPathJs_", ["/flow/commit"]);
    return self;
},
args: [],
source: "initialize\x0a\x0a\x09\x22Set amber to commit using the Flow's API\x22\x0a\x09Package defaultCommitPathSt: '/flow/commit'.\x0a\x09Package defaultCommitPathJs: '/flow/commit'\x0a",
messageSends: ["defaultCommitPathSt:", "defaultCommitPathJs:"],
referencedClasses: ["Package"]
}),
smalltalk.Model.klass);

smalltalk.addMethod(
"_jsonIgnoreInstanceVariables",
smalltalk.method({
selector: "jsonIgnoreInstanceVariables",
category: 'accessing',
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_jsonIgnoreInstanceVariables", [], smalltalk.Model.klass.superclass || nil), "__comma", [["announcer"]]);
    return self;
},
args: [],
source: "jsonIgnoreInstanceVariables\x0a\x0a\x09^ super jsonIgnoreInstanceVariables, #('announcer')",
messageSends: [",", "jsonIgnoreInstanceVariables"],
referencedClasses: []
}),
smalltalk.Model.klass);

smalltalk.addMethod(
"_modelPath",
smalltalk.method({
selector: "modelPath",
category: 'accessing',
fn: function () {
    var self = this;
    return smalltalk.send(self, "_name", []);
    return self;
},
args: [],
source: "modelPath\x0a\x09\x22Answers the path for posting instances of this model.\x0a\x09Subclasses should override if appropriate.\x22\x0a\x0a\x09^ self name",
messageSends: ["name"],
referencedClasses: []
}),
smalltalk.Model.klass);

smalltalk.addMethod(
"_path",
smalltalk.method({
selector: "path",
category: 'accessing',
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.send(smalltalk.send(self, "_basePath", []), "__comma", ["/"]), "__comma", [smalltalk.send(self, "_modelPath", [])]);
    return self;
},
args: [],
source: "path\x0a\x09^ self basePath,'/', self modelPath",
messageSends: [",", "basePath", "modelPath"],
referencedClasses: []
}),
smalltalk.Model.klass);


smalltalk.addClass('JsonModel', smalltalk.Model, ['data'], 'Flow-Models');


smalltalk.addClass('PersistentModel', smalltalk.Model, ['data'], 'Flow-Models');
smalltalk.addMethod(
"__eq",
smalltalk.method({
selector: "=",
category: 'reactions',
fn: function (anObject) {
    var self = this;
    return smalltalk.send(smalltalk.send(anObject, "_respondsTo_", [smalltalk.symbolFor("id")]), "_and_", [function () {return smalltalk.send(smalltalk.send(self, "_id", []), "__eq", [smalltalk.send(anObject, "_id", [])]);}]);
    return self;
},
args: ["anObject"],
source: "= anObject\x0a\x0a\x09^ (anObject respondsTo: #id) and:[\x0a\x09self id = anObject id]\x0a",
messageSends: ["and:", "respondsTo:", "=", "id"],
referencedClasses: []
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_asJSONString",
smalltalk.method({
selector: "asJSONString",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_onAboutToJSON", []);
    return smalltalk.send(smalltalk.JSON || JSON, "_stringify_", [self['@data']]);
    return self;
},
args: [],
source: "asJSONString\x0a\x0a\x09self onAboutToJSON.\x0a\x0a\x09^ JSON stringify: data",
messageSends: ["onAboutToJSON", "stringify:"],
referencedClasses: ["JSON"]
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_at_",
smalltalk.method({
selector: "at:",
category: 'accessing',
fn: function (aKey) {
    var self = this;
    return smalltalk.send(self['@data'], "_at_", [aKey]);
    return self;
},
args: ["aKey"],
source: "at: aKey\x0a\x0a\x09^ data at: aKey",
messageSends: ["at:"],
referencedClasses: []
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_at_put_",
smalltalk.method({
selector: "at:put:",
category: 'accessing',
fn: function (aKey, anObject) {
    var self = this;
    smalltalk.send(self['@data'], "_at_put_", [aKey, anObject]);
    return anObject;
    return self;
},
args: ["aKey", "anObject"],
source: "at: aKey put: anObject\x0a\x0a\x09data at: aKey put: anObject.\x0a\x0a\x09^ anObject",
messageSends: ["at:put:"],
referencedClasses: []
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_create",
smalltalk.method({
selector: "create",
category: 'actions',
fn: function () {
    var self = this;
    return smalltalk.send(self, "_createDo_", [function () {return nil;}]);
    return self;
},
args: [],
source: "create\x0a\x0a\x09^ self createDo:[nil]",
messageSends: ["createDo:"],
referencedClasses: []
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_createDo_",
smalltalk.method({
selector: "createDo:",
category: 'actions',
fn: function (aBlock) {
    var self = this;
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_", [{url: smalltalk.send(self, "_path", []), type: "POST", cache: false, data: smalltalk.send(self, "_asJSONString", []), success: function (x) {return smalltalk.send(self, "_onAfterCreate_done_", [x, aBlock]);}, fail: function (x) {return smalltalk.send(smalltalk.ModelCreateError || ModelCreateError, "_signal_", [smalltalk.send(smalltalk.send(smalltalk.send("Could not create ", "__comma", [smalltalk.send(smalltalk.send(self, "_class", []), "_name", [])]), "__comma", [":  "]), "__comma", [smalltalk.send(x, "_responseText", [])])]);}, error: function (x) {return smalltalk.send(smalltalk.ModelCreateError || ModelCreateError, "_signal_", [smalltalk.send(smalltalk.send(smalltalk.send("Could not create ", "__comma", [smalltalk.send(smalltalk.send(self, "_class", []), "_name", [])]), "__comma", [":  "]), "__comma", [smalltalk.send(x, "_responseText", [])])]);}}]);
    return self;
    return self;
},
args: ["aBlock"],
source: "createDo: aBlock\x0a\x0a\x09jQuery ajax: #{\x0a\x09\x09url: self path.\x0a\x09\x09type: 'POST'.\x0a\x09\x09cache: false.\x0a\x09\x09data: self asJSONString.\x0a\x09\x09success: [:x| self onAfterCreate: x done: aBlock].\x0a\x09\x09fail: [:x| ModelCreateError signal: 'Could not create ', self class name,':  ', x responseText].\x0a\x09\x09error: [:x| ModelCreateError signal: 'Could not create ', self class name,':  ', x responseText]}.\x0a\x0a\x09^ self\x0a",
messageSends: ["ajax:", "path", "asJSONString", "onAfterCreate:done:", "signal:", ",", "name", "class", "responseText"],
referencedClasses: ["ModelCreateError"]
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_createdOn",
smalltalk.method({
selector: "createdOn",
category: 'accessing',
fn: function () {
    var self = this;
    var $early = {};
    try {
        var selector = nil;
        var expects = nil;
        var object = nil;
        selector = smalltalk.symbolFor("createdOn");
        expects = smalltalk.Date || Date;
        object = smalltalk.send(self, "_at_", [smalltalk.send(selector, "_asString", [])]);
        ($receiver = object) == nil || $receiver == undefined ? function () {return function () {throw $early = [nil];}();}() : $receiver;
        ($receiver = smalltalk.send(object, "_isKindOf_", [expects])).klass === smalltalk.Boolean ? $receiver ? function () {return function () {throw $early = [object];}();}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return function () {throw $early = [object];}();}]);
        return smalltalk.send(self, "_at_put_", [smalltalk.send(selector, "_asString", []), smalltalk.send(self, "_dateAndTimeAt_", [selector])]);
        return self;
    } catch (e) {
        if (e === $early) {
            return e[0];
        }
        throw e;
    }
},
args: [],
source: "createdOn \x0a\x0a\x09| selector expects object|\x0a\x0a\x09selector := #createdOn.\x0a\x09expects := Date.\x0a\x0a\x09object := self at: selector asString.\x0a\x09object ifNil:[^nil].\x0a\x0a\x09(object isKindOf: expects) ifTrue:[^object].\x0a\x0a\x09^ self at: selector asString put: (self dateAndTimeAt: selector).",
messageSends: ["at:", "asString", "ifNil:", "ifTrue:", "isKindOf:", "at:put:", "dateAndTimeAt:"],
referencedClasses: ["Date"]
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_createdOn_",
smalltalk.method({
selector: "createdOn:",
category: 'accessing',
fn: function (aDate) {
    var self = this;
    smalltalk.send(self, "_at_put_", [smalltalk.symbolFor("createdOn"), aDate]);
    return self;
},
args: ["aDate"],
source: "createdOn: aDate\x0a\x0a\x09self at: #createdOn put: aDate",
messageSends: ["at:put:"],
referencedClasses: []
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_data",
smalltalk.method({
selector: "data",
category: 'accessing',
fn: function () {
    var self = this;
    return self['@data'];
    return self;
},
args: [],
source: "data\x0a\x0a\x09^ data",
messageSends: [],
referencedClasses: []
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_dateAndTimeAt_",
smalltalk.method({
selector: "dateAndTimeAt:",
category: 'accessing',
fn: function (aSelector) {
    var self = this;
    return smalltalk.send(smalltalk.Date || Date, "_fromString_", [smalltalk.send(self, "_at_", [aSelector])]);
    return self;
},
args: ["aSelector"],
source: "dateAndTimeAt: aSelector\x0a\x0a\x09^ Date fromString: (self at: aSelector)",
messageSends: ["fromString:", "at:"],
referencedClasses: ["Date"]
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_delete",
smalltalk.method({
selector: "delete",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_deleteDo_", [function () {return nil;}]);
    return self;
},
args: [],
source: "delete\x0a\x0a\x09self deleteDo:[nil]\x0a\x0a",
messageSends: ["deleteDo:"],
referencedClasses: []
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_deleteDo_",
smalltalk.method({
selector: "deleteDo:",
category: 'actions',
fn: function (aBlock) {
    var self = this;
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_", [{url: smalltalk.send(smalltalk.send(smalltalk.send(self, "_path", []), "__comma", ["?id="]), "__comma", [smalltalk.send(smalltalk.send(self, "_id", []), "_asString", [])]), type: "DELETE", cache: false, data: smalltalk.send(self, "_asJSONString", []), success: function (x) {return smalltalk.send(self, "_onAfterDelete_done_", [x, aBlock]);}, fail: function (x) {return smalltalk.send(self, "_onDeleteFail_", [x]);}, error: function (x) {return smalltalk.send(self, "_onDeleteFail_", [x]);}}]);
    return self;
},
args: ["aBlock"],
source: "deleteDo: aBlock\x0a\x0a\x09jQuery ajax: #{ \x0a\x09\x09url: (self path, '?id=',self id asString).\x0a\x09\x09type: 'DELETE'.\x0a\x09\x09cache: false.\x0a\x09\x09data: self asJSONString.\x0a\x09\x09success: [:x| self onAfterDelete: x done: aBlock].\x0a\x09\x09fail: [:x| self onDeleteFail: x].\x0a\x09\x09error: [:x| self onDeleteFail: x]}  \x0a",
messageSends: ["ajax:", ",", "path", "asString", "id", "asJSONString", "onAfterDelete:done:", "onDeleteFail:"],
referencedClasses: []
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_doesNotUnderstand_",
smalltalk.method({
selector: "doesNotUnderstand:",
category: 'actions',
fn: function (aMessage) {
    var self = this;
    var $early = {};
    try {
        var key = nil;
        var part = nil;
        var subModel = nil;
        var isUndefined = nil;
        var isObject = nil;
        var obj = nil;
        var keys = nil;
        key = smalltalk.send(smalltalk.send(aMessage, "_selector", []), "_asSymbol", []);
        ($receiver = smalltalk.send(key, "_isUnary", [])).klass === smalltalk.Boolean ? $receiver ? function () {($receiver = smalltalk.send(self['@data'], "_isKindOf_", [smalltalk.HashedCollection || HashedCollection])).klass === smalltalk.Boolean ? $receiver ? function () {return part = smalltalk.send(self['@data'], "_at_ifAbsent_", [smalltalk.send(key, "_asString", []), function () {return nil;}]);}() : function () {return part = smalltalk.send(self['@data'], "_at_", [smalltalk.send(key, "_asString", [])]);}() : smalltalk.send($receiver, "_ifTrue_ifFalse_", [function () {return part = smalltalk.send(self['@data'], "_at_ifAbsent_", [smalltalk.send(key, "_asString", []), function () {return nil;}]);}, function () {return part = smalltalk.send(self['@data'], "_at_", [smalltalk.send(key, "_asString", [])]);}]);($receiver = part) == nil || $receiver == undefined ? function () {return function () {throw $early = [nil];}();}() : $receiver;isUndefined = part == "undefined";($receiver = isUndefined).klass === smalltalk.Boolean ? $receiver ? function () {return function () {throw $early = [nil];}();}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return function () {throw $early = [nil];}();}]);isObject = typeof part == "object";($receiver = isObject).klass === smalltalk.Boolean ? $receiver ? function () {obj = Object;obj = smalltalk.send(smalltalk.JSObjectProxy || JSObjectProxy, "_on_", [obj]);return ($receiver = smalltalk.send(smalltalk.send(obj, "_keys_", [part]), "_isEmpty", [])).klass === smalltalk.Boolean ? $receiver ? function () {smalltalk.send(self['@data'], "_at_put_", [smalltalk.send(key, "_asString", []), nil]);return function () {throw $early = [nil];}();}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {smalltalk.send(self['@data'], "_at_put_", [smalltalk.send(key, "_asString", []), nil]);return function () {throw $early = [nil];}();}]);}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {obj = Object;obj = smalltalk.send(smalltalk.JSObjectProxy || JSObjectProxy, "_on_", [obj]);return ($receiver = smalltalk.send(smalltalk.send(obj, "_keys_", [part]), "_isEmpty", [])).klass === smalltalk.Boolean ? $receiver ? function () {smalltalk.send(self['@data'], "_at_put_", [smalltalk.send(key, "_asString", []), nil]);return function () {throw $early = [nil];}();}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {smalltalk.send(self['@data'], "_at_put_", [smalltalk.send(key, "_asString", []), nil]);return function () {throw $early = [nil];}();}]);}]);subModel = part.modelClass;($receiver = subModel) == nil || $receiver == undefined ? function () {return function () {throw $early = [part];}();}() : $receiver;subModel = smalltalk.send(smalltalk.send(smalltalk.Smalltalk || Smalltalk, "_current", []), "_at_", [subModel]);($receiver = subModel) == nil || $receiver == undefined ? function () {smalltalk.send(part, "_inspect", []);return smalltalk.send(self, "_error_", [smalltalk.send(smalltalk.send("this should have a ", "__comma", [subModel]), "__comma", [" modelClass no?"])]);}() : $receiver;subModel = smalltalk.send(subModel, "_fromReified_", [part]);smalltalk.send(self['@data'], "_at_put_", [smalltalk.send(key, "_asString", []), subModel]);return function () {throw $early = [subModel];}();}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {($receiver = smalltalk.send(self['@data'], "_isKindOf_", [smalltalk.HashedCollection || HashedCollection])).klass === smalltalk.Boolean ? $receiver ? function () {return part = smalltalk.send(self['@data'], "_at_ifAbsent_", [smalltalk.send(key, "_asString", []), function () {return nil;}]);}() : function () {return part = smalltalk.send(self['@data'], "_at_", [smalltalk.send(key, "_asString", [])]);}() : smalltalk.send($receiver, "_ifTrue_ifFalse_", [function () {return part = smalltalk.send(self['@data'], "_at_ifAbsent_", [smalltalk.send(key, "_asString", []), function () {return nil;}]);}, function () {return part = smalltalk.send(self['@data'], "_at_", [smalltalk.send(key, "_asString", [])]);}]);($receiver = part) == nil || $receiver == undefined ? function () {return function () {throw $early = [nil];}();}() : $receiver;isUndefined = part == "undefined";($receiver = isUndefined).klass === smalltalk.Boolean ? $receiver ? function () {return function () {throw $early = [nil];}();}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return function () {throw $early = [nil];}();}]);isObject = typeof part == "object";($receiver = isObject).klass === smalltalk.Boolean ? $receiver ? function () {obj = Object;obj = smalltalk.send(smalltalk.JSObjectProxy || JSObjectProxy, "_on_", [obj]);return ($receiver = smalltalk.send(smalltalk.send(obj, "_keys_", [part]), "_isEmpty", [])).klass === smalltalk.Boolean ? $receiver ? function () {smalltalk.send(self['@data'], "_at_put_", [smalltalk.send(key, "_asString", []), nil]);return function () {throw $early = [nil];}();}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {smalltalk.send(self['@data'], "_at_put_", [smalltalk.send(key, "_asString", []), nil]);return function () {throw $early = [nil];}();}]);}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {obj = Object;obj = smalltalk.send(smalltalk.JSObjectProxy || JSObjectProxy, "_on_", [obj]);return ($receiver = smalltalk.send(smalltalk.send(obj, "_keys_", [part]), "_isEmpty", [])).klass === smalltalk.Boolean ? $receiver ? function () {smalltalk.send(self['@data'], "_at_put_", [smalltalk.send(key, "_asString", []), nil]);return function () {throw $early = [nil];}();}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {smalltalk.send(self['@data'], "_at_put_", [smalltalk.send(key, "_asString", []), nil]);return function () {throw $early = [nil];}();}]);}]);subModel = part.modelClass;($receiver = subModel) == nil || $receiver == undefined ? function () {return function () {throw $early = [part];}();}() : $receiver;subModel = smalltalk.send(smalltalk.send(smalltalk.Smalltalk || Smalltalk, "_current", []), "_at_", [subModel]);($receiver = subModel) == nil || $receiver == undefined ? function () {smalltalk.send(part, "_inspect", []);return smalltalk.send(self, "_error_", [smalltalk.send(smalltalk.send("this should have a ", "__comma", [subModel]), "__comma", [" modelClass no?"])]);}() : $receiver;subModel = smalltalk.send(subModel, "_fromReified_", [part]);smalltalk.send(self['@data'], "_at_put_", [smalltalk.send(key, "_asString", []), subModel]);return function () {throw $early = [subModel];}();}]);
        return ($receiver = smalltalk.send(smalltalk.send(key, "_isKeyword", []), "_and_", [function () {return smalltalk.send(smalltalk.send(smalltalk.send(key, "_asString", []), "_occurrencesOf_", [":"]), "__eq", [1]);}])).klass === smalltalk.Boolean ? $receiver ? function () {key = smalltalk.send(key, "_allButLast", []);return smalltalk.send(self['@data'], "_at_put_", [smalltalk.send(key, "_asString", []), smalltalk.send(smalltalk.send(aMessage, "_arguments", []), "_first", [])]);}() : function () {return smalltalk.send(self, "_doesNotUnderstand_", [aMessage], smalltalk.PersistentModel.superclass || nil);}() : smalltalk.send($receiver, "_ifTrue_ifFalse_", [function () {key = smalltalk.send(key, "_allButLast", []);return smalltalk.send(self['@data'], "_at_put_", [smalltalk.send(key, "_asString", []), smalltalk.send(smalltalk.send(aMessage, "_arguments", []), "_first", [])]);}, function () {return smalltalk.send(self, "_doesNotUnderstand_", [aMessage], smalltalk.PersistentModel.superclass || nil);}]);
        return self;
    } catch (e) {
        if (e === $early) {
            return e[0];
        }
        throw e;
    }
},
args: ["aMessage"],
source: "doesNotUnderstand: aMessage\x0a\x09\x22The idea behind this DNU is to use the selector as setters or getter \x0a\x09delegating to data (aJsonObject)\x22\x0a\x09\x0a\x09| key part subModel isUndefined isObject obj keys |\x0a\x0a\x09key := aMessage selector asSymbol.\x0a\x09\x0a\x09key isUnary ifTrue: [\x0a\x09\x09(data isKindOf: HashedCollection)\x0a\x09\x09\x09ifTrue:[part := data at: key asString ifAbsent:[nil]]\x0a\x09\x09\x09ifFalse:[part := data at: key asString].\x0a\x09\x09part ifNil:[^nil].\x0a\x09\x09isUndefined := <part=='undefined'>.\x0a\x09\x09isUndefined ifTrue:[^nil].\x0a\x09\x09isObject := <typeof part=='object'>.\x0a\x09\x09isObject ifTrue:[\x0a\x09\x09\x09\x22is part an empty js object? (would be nil for JSON eyes)\x22\x0a\x09\x09\x09obj := <Object>.\x0a\x09\x09\x09obj := JSObjectProxy on: obj.\x0a\x09\x09\x09(obj keys: part) isEmpty ifTrue:[\x0a\x09\x09\x09\x09data at: key asString put: nil.\x0a\x09\x09\x09\x09^nil]].\x0a\x0a\x09\x09subModel := <part['modelClass']>.\x0a\x09\x09\x22If there is no modelClass in it, then is a direct value in the property\x22\x0a\x09\x09subModel ifNil:[ ^part].\x0a\x0a\x09\x09subModel := Smalltalk current at: subModel.\x0a\x09\x09subModel ifNil:[part inspect. self error: 'this should have a ',subModel,' modelClass no?'].\x0a\x09\x09subModel := subModel fromReified: part.\x0a\x09\x09data at: key asString put: subModel.\x0a\x09\x09^ subModel].\x0a\x0a\x09^ (key isKeyword and: [\x0a\x09(key asString occurrencesOf: ':') = 1])\x0a\x09\x09ifTrue: [key := key allButLast.\x0a\x09\x09\x09\x09data at: key asString put: aMessage arguments first]\x0a\x09\x09ifFalse: [super doesNotUnderstand: aMessage]",
messageSends: ["asSymbol", "selector", "ifTrue:", "isUnary", "ifTrue:ifFalse:", "isKindOf:", "at:ifAbsent:", "asString", "at:", "ifNil:", "on:", "isEmpty", "keys:", "at:put:", "current", "inspect", "error:", ",", "fromReified:", "and:", "isKeyword", "=", "occurrencesOf:", "allButLast", "first", "arguments", "doesNotUnderstand:"],
referencedClasses: ["HashedCollection", "JSObjectProxy", "Smalltalk"]
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_id",
smalltalk.method({
selector: "id",
category: 'accessing',
fn: function () {
    var self = this;
    return smalltalk.send(self['@data'], "_at_", [smalltalk.symbolFor("id")]);
    return self;
},
args: [],
source: "id\x0a\x0a\x09^ data at: #id ",
messageSends: ["at:"],
referencedClasses: []
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_id_",
smalltalk.method({
selector: "id:",
category: 'accessing',
fn: function (aString) {
    var self = this;
    smalltalk.send(self['@data'], "_at_put_", [smalltalk.symbolFor("id"), aString]);
    return self;
},
args: ["aString"],
source: "id: aString\x0a\x0a\x09data at: #id put: aString",
messageSends: ["at:put:"],
referencedClasses: []
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_initialize",
smalltalk.method({
selector: "initialize",
category: 'initialization',
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.PersistentModel.superclass || nil);
    self['@data'] = smalltalk.send(smalltalk.HashedCollection || HashedCollection, "_new", []);
    smalltalk.send(self, "_modelClass_", [smalltalk.send(smalltalk.send(self, "_class", []), "_name", [])]);
    return self;
},
args: [],
source: "initialize\x0a\x0a\x09super initialize.\x0a\x0a\x09data := HashedCollection new.\x0a\x09self modelClass: self class name.",
messageSends: ["initialize", "new", "modelClass:", "name", "class"],
referencedClasses: ["HashedCollection"]
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_onAboutToJSON",
smalltalk.method({
selector: "onAboutToJSON",
category: 'reactions',
fn: function () {
    var self = this;
    var obj = nil;
    var keys = nil;
    obj = Object;
    obj = smalltalk.send(smalltalk.JSObjectProxy || JSObjectProxy, "_on_", [obj]);
    keys = smalltalk.send(obj, "_keys_", [self['@data']]);
    smalltalk.send(keys, "_do_", [function (key) {var value = nil;value = smalltalk.send(self['@data'], "_at_", [key]);value = smalltalk.send(self, "_perform_", [smalltalk.send(key, "_asSymbol", [])]);($receiver = smalltalk.send(value, "_isKindOf_", [smalltalk.PersistentModel || PersistentModel])).klass === smalltalk.Boolean ? $receiver ? function () {smalltalk.send(value, "_onAboutToJSON", []);return value = smalltalk.send(value, "_data", []);}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {smalltalk.send(value, "_onAboutToJSON", []);return value = smalltalk.send(value, "_data", []);}]);return smalltalk.send(self['@data'], "_at_put_", [key, value]);}]);
    return self;
},
args: [],
source: "onAboutToJSON\x0a\x09\x22This model is about to be stringified in JSON.\x0a\x09All inst var values that are expected to be model objects, need to be stringify friendly after this.\x22\x0a\x09| obj keys |\x0a\x0a\x09obj := <Object>.\x0a\x09obj := JSObjectProxy on: obj.\x0a\x09keys := obj keys: data. \x0a\x0a\x09keys do:[:key| |value|\x0a\x09\x09value := data at: key.\x0a\x09\x09value := self perform: key asSymbol.\x0a\x0a\x09\x09(value isKindOf: PersistentModel) ifTrue:[\x0a\x09\x09\x09value onAboutToJSON.\x0a\x09\x09\x09value := value data].\x0a\x09\x09data at: key put: value].",
messageSends: ["on:", "keys:", "do:", "at:", "perform:", "asSymbol", "ifTrue:", "isKindOf:", "onAboutToJSON", "data", "at:put:"],
referencedClasses: ["JSObjectProxy", "PersistentModel"]
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_onAfterCreate_done_",
smalltalk.method({
selector: "onAfterCreate:done:",
category: 'reactions',
fn: function (x, aBlock) {
    var self = this;
    self['@data'] = smalltalk.send(smalltalk.send(self, "_class", []), "_reify_", [x]);
    smalltalk.send(self, "_announce_", [smalltalk.send(smalltalk.ModelCreated || ModelCreated, "_for_", [self])]);
    smalltalk.send(aBlock, "_value_", [self]);
    return self;
},
args: ["x", "aBlock"],
source: "onAfterCreate: x done: aBlock\x0a\x0a\x09data := self class reify: x.\x0a\x0a\x09self announce: (ModelCreated for: self).\x0a\x09aBlock value: self",
messageSends: ["reify:", "class", "announce:", "for:", "value:"],
referencedClasses: ["ModelCreated"]
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_onAfterDelete_done_",
smalltalk.method({
selector: "onAfterDelete:done:",
category: 'reactions',
fn: function (x, aBlock) {
    var self = this;
    smalltalk.send(self, "_announce_", [smalltalk.send(smalltalk.ModelDeleted || ModelDeleted, "_for_", [self])]);
    smalltalk.send(aBlock, "_value_", [self]);
    return self;
},
args: ["x", "aBlock"],
source: "onAfterDelete: x done: aBlock\x0a\x09\x0a\x09self announce: (ModelDeleted for: self).\x0a\x0a\x09aBlock value: self\x0a",
messageSends: ["announce:", "for:", "value:"],
referencedClasses: ["ModelDeleted"]
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_onAfterRefresh_done_",
smalltalk.method({
selector: "onAfterRefresh:done:",
category: 'reactions',
fn: function (x, aBlock) {
    var self = this;
    smalltalk.send(self, "_syncWith_", [smalltalk.send(smalltalk.send(self, "_class", []), "_reify_", [x])]);
    smalltalk.send(self, "_announce_", [smalltalk.send(smalltalk.ModelRefreshed || ModelRefreshed, "_for_", [self])]);
    smalltalk.send(aBlock, "_value_", [self]);
    return self;
},
args: ["x", "aBlock"],
source: "onAfterRefresh: x done: aBlock\x0a\x0a\x09self syncWith: (self class reify: x).\x0a\x09self announce: (ModelRefreshed for: self).\x0a\x09aBlock value: self\x0a",
messageSends: ["syncWith:", "reify:", "class", "announce:", "for:", "value:"],
referencedClasses: ["ModelRefreshed"]
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_onAfterSave_done_",
smalltalk.method({
selector: "onAfterSave:done:",
category: 'reactions',
fn: function (x, aBlock) {
    var self = this;
    smalltalk.send(self, "_announce_", [smalltalk.send(smalltalk.ModelSaved || ModelSaved, "_for_", [self])]);
    smalltalk.send(aBlock, "_value_", [self]);
    return self;
},
args: ["x", "aBlock"],
source: "onAfterSave: x done: aBlock\x0a\x09\x0a\x09self announce: (ModelSaved for: self).\x0a\x0a\x09aBlock value: self",
messageSends: ["announce:", "for:", "value:"],
referencedClasses: ["ModelSaved"]
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_onDeleteFail_",
smalltalk.method({
selector: "onDeleteFail:",
category: 'reactions',
fn: function (x) {
    var self = this;
    smalltalk.send(smalltalk.ModelUpdateError || ModelUpdateError, "_signal_", [smalltalk.send("Could not delete ", "__comma", [smalltalk.send(smalltalk.send(self, "_class", []), "_name", [])])]);
    return self;
},
args: ["x"],
source: "onDeleteFail: x\x0a\x0a\x09ModelUpdateError signal: 'Could not delete ', self class name\x09\x0a",
messageSends: ["signal:", ",", "name", "class"],
referencedClasses: ["ModelUpdateError"]
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_onRefreshFail_",
smalltalk.method({
selector: "onRefreshFail:",
category: 'reactions',
fn: function (x) {
    var self = this;
    smalltalk.send(smalltalk.ModelRefreshError || ModelRefreshError, "_signal_", [smalltalk.send("Could not refresh ", "__comma", [smalltalk.send(smalltalk.send(self, "_class", []), "_name", [])])]);
    return self;
},
args: ["x"],
source: "onRefreshFail: x\x0a\x0a\x09ModelRefreshError signal: 'Could not refresh ', self class name\x09\x0a",
messageSends: ["signal:", ",", "name", "class"],
referencedClasses: ["ModelRefreshError"]
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_onSaveFail_",
smalltalk.method({
selector: "onSaveFail:",
category: 'reactions',
fn: function (x) {
    var self = this;
    smalltalk.send(smalltalk.ModelUpdateError || ModelUpdateError, "_signal_", [smalltalk.send("Could not save ", "__comma", [smalltalk.send(smalltalk.send(self, "_class", []), "_name", [])])]);
    return self;
},
args: ["x"],
source: "onSaveFail: x\x0a\x0a\x09ModelUpdateError signal: 'Could not save ', self class name\x09\x0a",
messageSends: ["signal:", ",", "name", "class"],
referencedClasses: ["ModelUpdateError"]
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_path",
smalltalk.method({
selector: "path",
category: 'accessing',
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_class", []), "_path", []);
    return self;
},
args: [],
source: "path \x0a\x0a\x09^ self class path",
messageSends: ["path", "class"],
referencedClasses: []
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_refresh",
smalltalk.method({
selector: "refresh",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_refreshDo_", [function () {return nil;}]);
    return self;
},
args: [],
source: "refresh\x0a\x09\x22Re-read this model's state.\x22\x0a\x0a\x09self refreshDo:[nil]",
messageSends: ["refreshDo:"],
referencedClasses: []
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_refreshDo_",
smalltalk.method({
selector: "refreshDo:",
category: 'actions',
fn: function (aBlock) {
    var self = this;
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_", [{url: smalltalk.send(smalltalk.send(smalltalk.send(self, "_path", []), "__comma", ["?id="]), "__comma", [smalltalk.send(smalltalk.send(self, "_id", []), "_asString", [])]), type: "GET", cache: false, success: function (x) {return smalltalk.send(self, "_onAfterRefresh_done_", [x, aBlock]);}, fail: function (x) {return smalltalk.send(self, "_onRefeshFail_", [x]);}, error: function (x) {return smalltalk.send(self, "_onRefreshFail_", [x]);}}]);
    return self;
},
args: ["aBlock"],
source: "refreshDo: aBlock\x0a\x09\x22Re-read this model's state.\x22\x0a\x0a\x09jQuery ajax: #{ \x0a\x09\x09url: (self path, '?id=',self id asString).\x0a\x09\x09type: 'GET'.\x0a\x09\x09cache: false.\x0a\x09\x09success: [:x| self onAfterRefresh: x done: aBlock].\x0a\x09\x09fail: [:x| self onRefeshFail: x].\x0a\x09\x09error: [:x| self onRefreshFail: x]}\x09\x0a",
messageSends: ["ajax:", ",", "path", "asString", "id", "onAfterRefresh:done:", "onRefeshFail:", "onRefreshFail:"],
referencedClasses: []
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_save",
smalltalk.method({
selector: "save",
category: 'actions',
fn: function () {
    var self = this;
    return smalltalk.send(self, "_saveDo_", [function () {return nil;}]);
    return self;
},
args: [],
source: "save\x0a\x09^ self saveDo:[nil]",
messageSends: ["saveDo:"],
referencedClasses: []
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_saveDo_",
smalltalk.method({
selector: "saveDo:",
category: 'actions',
fn: function (aBlock) {
    var self = this;
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_", [{url: smalltalk.send(self, "_path", []), type: "PUT", cache: false, data: smalltalk.send(self, "_asJSONString", []), success: function (x) {return smalltalk.send(self, "_onAfterSave_done_", [x, aBlock]);}, fail: function (x) {return smalltalk.send(self, "_onSaveFail_", [x]);}, error: function (x) {return smalltalk.send(self, "_onSaveFail_", [x]);}}]);
    return self;
},
args: ["aBlock"],
source: "saveDo: aBlock\x0a\x0a\x09jQuery ajax: #{ \x0a\x09\x09url: self path.\x0a\x09\x09type: 'PUT'.\x0a\x09\x09cache: false.\x0a\x09\x09data: self asJSONString.\x0a\x09\x09success: [:x| self onAfterSave: x done: aBlock].\x0a\x09\x09fail: [:x| self onSaveFail: x].\x0a\x09\x09error: [:x| self onSaveFail: x]}  \x0a\x0a\x0a",
messageSends: ["ajax:", "path", "asJSONString", "onAfterSave:done:", "onSaveFail:"],
referencedClasses: []
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_subModelAt_",
smalltalk.method({
selector: "subModelAt:",
category: 'accessing',
fn: function (aSelector) {
    var self = this;
    var $early = {};
    try {
        var subModelData = nil;
        var modelClass = nil;
        subModelData = smalltalk.send(self['@data'], "_at_", [aSelector]);
        ($receiver = subModelData) == nil || $receiver == undefined ? function () {return function () {throw $early = [nil];}();}() : $receiver;
        modelClass = smalltalk.send(subModelData, "_at_", ["modelClass"]);
        ($receiver = modelClass) == nil || $receiver == undefined ? function () {return function () {throw $early = [nil];}();}() : $receiver;
        modelClass = smalltalk.send(smalltalk.send(smalltalk.Smalltalk || Smalltalk, "_current", []), "_at_", [modelClass]);
        ($receiver = modelClass) == nil || $receiver == undefined ? function () {return function () {throw $early = [smalltalk.send(smalltalk.ModelMetadataError || ModelMetadataError, "_signal_", [smalltalk.send(smalltalk.send("Cannot find ", "__comma", [smalltalk.send(aSelector, "_asString", [])]), "__comma", ["'s class for this metadata"])])];}();}() : $receiver;
        return smalltalk.send(modelClass, "_fromReified_", [subModelData]);
        return self;
    } catch (e) {
        if (e === $early) {
            return e[0];
        }
        throw e;
    }
},
args: ["aSelector"],
source: "subModelAt: aSelector\x0a\x09\x22Answers the reified submodel (instantiating if necessary).\x22\x0a\x09\x0a\x09| subModelData modelClass |\x0a\x0a\x09subModelData := data at: aSelector.\x0a\x09subModelData ifNil:[^nil].\x0a\x09\x0a\x09modelClass := subModelData at: 'modelClass'.\x0a\x09modelClass ifNil:[^nil].\x0a\x09modelClass := Smalltalk current at: modelClass.\x0a\x0a\x09modelClass ifNil:[^ModelMetadataError signal: 'Cannot find ',aSelector asString,'''s class for this metadata'].\x0a\x09\x0a\x09^ modelClass fromReified: subModelData",
messageSends: ["at:", "ifNil:", "current", "signal:", ",", "asString", "fromReified:"],
referencedClasses: ["Smalltalk", "ModelMetadataError"]
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_syncFrom_",
smalltalk.method({
selector: "syncFrom:",
category: 'actions',
fn: function (someJson) {
    var self = this;
    return smalltalk.send(self, "_syncWith_", [smalltalk.send(smalltalk.send(self, "_class", []), "_reify_", [someJson])]);
    return self;
},
args: ["someJson"],
source: "syncFrom: someJson\x0a\x0a\x09^ self syncWith: (self class reify: someJson)",
messageSends: ["syncWith:", "reify:", "class"],
referencedClasses: []
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_syncWith_",
smalltalk.method({
selector: "syncWith:",
category: 'actions',
fn: function (aReifiedJSON) {
    var self = this;
    var $early = {};
    try {
        ($receiver = aReifiedJSON) == nil || $receiver == undefined ? function () {return function () {throw $early = [nil];}();}() : $receiver;
        ($receiver = smalltalk.send(smalltalk.send(smalltalk.send(aReifiedJSON, "_at_", ["modelClass"]), "_isNil", []), "_or_", [function () {return smalltalk.send(smalltalk.send(aReifiedJSON, "_at_", ["modelClass"]), "_~_eq", [smalltalk.send(smalltalk.send(self, "_class", []), "_name", [])]);}])).klass === smalltalk.Boolean ? $receiver ? function () {return smalltalk.send(self, "_error_", ["this JSON does not fit here"]);}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return smalltalk.send(self, "_error_", ["this JSON does not fit here"]);}]);
        self['@data'] = aReifiedJSON;
        return self;
    } catch (e) {
        if (e === $early) {
            return e[0];
        }
        throw e;
    }
},
args: ["aReifiedJSON"],
source: "syncWith: aReifiedJSON\x0a\x09\x22Sync the current values in this model \x0a\x09with the ones coming in aReifiedJSON.\x22\x0a\x0a\x09aReifiedJSON ifNil:[^ nil].\x0a\x0a\x09((aReifiedJSON at: 'modelClass') isNil or:[\x0a\x09(aReifiedJSON at: 'modelClass') ~= self class name]) ifTrue:[\x0a\x09\x09self error: 'this JSON does not fit here'].\x0a\x0a\x09data := aReifiedJSON",
messageSends: ["ifNil:", "ifTrue:", "or:", "isNil", "at:", "~=", "name", "class", "error:"],
referencedClasses: []
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_url",
smalltalk.method({
selector: "url",
category: 'accessing',
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.send(smalltalk.send(self, "_path", []), "__comma", ["/"]), "__comma", [smalltalk.send(smalltalk.send(self, "_id", []), "_asString", [])]);
    return self;
},
args: [],
source: "url\x0a\x0a\x09^ self path,'/',self id asString\x0a",
messageSends: [",", "path", "asString", "id"],
referencedClasses: []
}),
smalltalk.PersistentModel);


smalltalk.addMethod(
"_atId_do_",
smalltalk.method({
selector: "atId:do:",
category: 'accessing',
fn: function (anId, onDone) {
    var self = this;
    return smalltalk.send(self, "_read_do_", [anId, onDone]);
    return self;
},
args: ["anId", "onDone"],
source: "atId: anId do: onDone\x0a\x09\x22Answers the instance of this model found at anId (or nil)\x0a\x09using th answer in the onDone callback\x22\x0a \x0a\x09^ self read: anId do: onDone\x0a",
messageSends: ["read:do:"],
referencedClasses: []
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_create",
smalltalk.method({
selector: "create",
category: 'actions',
fn: function () {
    var self = this;
    return smalltalk.send(self, "_createDo_", [function () {return nil;}]);
    return self;
},
args: [],
source: "create\x0a\x0a\x09^ self createDo: [nil]",
messageSends: ["createDo:"],
referencedClasses: []
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_createDo_",
smalltalk.method({
selector: "createDo:",
category: 'actions',
fn: function (aBlock) {
    var self = this;
    var newInstance = nil;
    newInstance = smalltalk.send(smalltalk.send(self, "_basicNew", []), "_initialize", []);
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_", [{url: smalltalk.send(self, "_path", []), type: "POST", cache: false, data: smalltalk.send(newInstance, "_asJSONString", []), success: function (x) {return smalltalk.send(newInstance, "_onAfterCreate_done_", [x, aBlock]);}, fail: function (x) {return smalltalk.send(smalltalk.ModelCreateError || ModelCreateError, "_signal_", [smalltalk.send(smalltalk.send(smalltalk.send("Could not create ", "__comma", [smalltalk.send(self, "_name", [])]), "__comma", [":  "]), "__comma", [smalltalk.send(x, "_responseText", [])])]);}, error: function (x) {return smalltalk.send(smalltalk.ModelCreateError || ModelCreateError, "_signal_", [smalltalk.send(smalltalk.send(smalltalk.send("Could not create ", "__comma", [smalltalk.send(self, "_name", [])]), "__comma", [":  "]), "__comma", [smalltalk.send(x, "_responseText", [])])]);}}]);
    return newInstance;
    return self;
},
args: ["aBlock"],
source: "createDo: aBlock\x0a\x0a\x09| newInstance |\x0a\x0a\x09newInstance := self basicNew initialize.\x0a\x0a\x09jQuery ajax: #{\x0a\x09\x09url: self path.\x0a\x09\x09type: 'POST'.\x0a\x09\x09cache: false.\x0a\x09\x09data: newInstance asJSONString.\x0a\x09\x09success: [:x| newInstance onAfterCreate: x done: aBlock].\x0a\x09\x09fail: [:x| ModelCreateError signal: 'Could not create ', self name,':  ', x responseText].\x0a\x09\x09error: [:x| ModelCreateError signal: 'Could not create ', self name,':  ', x responseText]}.\x0a\x0a\x09^ newInstance\x0a",
messageSends: ["initialize", "basicNew", "ajax:", "path", "asJSONString", "onAfterCreate:done:", "signal:", ",", "name", "responseText"],
referencedClasses: ["ModelCreateError"]
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_createdOnFrom_",
smalltalk.method({
selector: "createdOnFrom:",
category: 'actions',
fn: function (aReifiedJSON) {
    var self = this;
    return smalltalk.send(smalltalk.Date || Date, "_fromString_", [aReifiedJSON]);
    return self;
},
args: ["aReifiedJSON"],
source: "createdOnFrom: aReifiedJSON\x0a\x0a\x09^ Date fromString: aReifiedJSON",
messageSends: ["fromString:"],
referencedClasses: ["Date"]
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_fromJson_",
smalltalk.method({
selector: "fromJson:",
category: 'actions',
fn: function (someJson) {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_new", []), "_syncFrom_", [someJson]);
    return self;
},
args: ["someJson"],
source: "fromJson: someJson\x0a\x09\x22Answers a new instance of this model and returns it\x0a\x09in the state dictated by someJson.\x22\x0a\x0a\x09^ self new syncFrom: someJson",
messageSends: ["syncFrom:", "new"],
referencedClasses: []
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_fromReified_",
smalltalk.method({
selector: "fromReified:",
category: 'actions',
fn: function (aReifiedJSON) {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_new", []), "_syncWith_", [aReifiedJSON]);
    return self;
},
args: ["aReifiedJSON"],
source: " fromReified: aReifiedJSON\x0a\x09\x22Answers a new instance of this model and returns it\x0a\x09in sync with aReifiedJSON.\x22\x0a\x0a\x09^ self new syncWith: aReifiedJSON",
messageSends: ["syncWith:", "new"],
referencedClasses: []
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_manyFromJson_",
smalltalk.method({
selector: "manyFromJson:",
category: 'actions',
fn: function (someJson) {
    var self = this;
    return smalltalk.send(smalltalk.send(smalltalk.JSON || JSON, "_parse_", [someJson]), "_collect_", [function (each) {return smalltalk.send(self, "_fromReified_", [each]);}]);
    return self;
},
args: ["someJson"],
source: "manyFromJson: someJson\x0a\x0a\x09^ (JSON parse: someJson) collect:[:each|\x0a\x09\x09 self fromReified: each ]",
messageSends: ["collect:", "parse:", "fromReified:"],
referencedClasses: ["JSON"]
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_onAfterRead_done_",
smalltalk.method({
selector: "onAfterRead:done:",
category: 'reactions',
fn: function (someJson, aBlock) {
    var self = this;
    var reified = nil;
    reified = smalltalk.send(self, "_fromJson_", [someJson]);
    smalltalk.send(aBlock, "_value_", [reified]);
    return reified;
    return self;
},
args: ["someJson", "aBlock"],
source: "onAfterRead: someJson done: aBlock\x0a\x0a\x09| reified |\x0a\x09\x0a\x09reified := self fromJson: someJson.\x0a\x09aBlock value: reified.\x0a\x09^ reified",
messageSends: ["fromJson:", "value:"],
referencedClasses: []
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_read_",
smalltalk.method({
selector: "read:",
category: 'actions',
fn: function (anId) {
    var self = this;
    smalltalk.send(self, "_read_do_", [anId, function () {return nil;}]);
    return self;
},
args: ["anId"],
source: "read: anId\x0a\x0a\x09self read: anId do: [nil]",
messageSends: ["read:do:"],
referencedClasses: []
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_read_do_",
smalltalk.method({
selector: "read:do:",
category: 'actions',
fn: function (anId, aBlock){
var self=this;
smalltalk.send((typeof jQuery == 'undefined' ? nil : jQuery), "_ajax_", [{url: smalltalk.send(smalltalk.send(smalltalk.send(self, "_path", []), "__comma", ["?id="]), "__comma", [anId]),type: "GET",cache: false,success: (function(x){return smalltalk.send(self, "_onAfterRead_done_", [x, aBlock]);}),fail: (function(x){return smalltalk.send((smalltalk.ModelReadError || ModelReadError), "_signal_", [smalltalk.send(smalltalk.send(smalltalk.send("Could not read ", "__comma", [smalltalk.send(self, "_name", [])]), "__comma", [":  "]), "__comma", [smalltalk.send(x, "_responseText", [])])]);}),error: (function(x){return smalltalk.send((smalltalk.ModelReadError || ModelReadError), "_signal_", [smalltalk.send(smalltalk.send(smalltalk.send("Could not read ", "__comma", [smalltalk.send(self, "_name", [])]), "__comma", [":  "]), "__comma", [smalltalk.send(x, "_responseText", [])])]);})}]);
return self;},
args: ["anId", "aBlock"],
source: "read: anId do: aBlock\x0a\x0a\x09jQuery ajax: #{ \x0a\x09\x09url: (self path, '?id=',anId).\x0a\x09\x09type: 'GET'.\x0a\x09\x09cache: false.\x0a\x09\x09success: [:x| self onAfterRead: x done: aBlock].\x0a\x09\x09fail: [:x| ModelReadError signal: 'Could not read ', self name,':  ', x responseText].\x0a\x09\x09error: [:x| ModelReadError signal: 'Could not read ', self name,':  ', x responseText]}",
messageSends: ["ajax:", ",", "path", "onAfterRead:done:", "signal:", "name", "responseText"],
referencedClasses: ["ModelReadError"]
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_reify_",
smalltalk.method({
selector: "reify:",
category: 'actions',
fn: function (someJson) {
    var self = this;
    return smalltalk.send(smalltalk.JSON || JSON, "_parse_", [someJson]);
    return self;
},
args: ["someJson"],
source: "reify: someJson\x0a\x09\x22Returns a simple javascript object with\x0a\x09the attributes meant for the matching instance variable content of this model.\x22\x0a\x0a\x09^ JSON parse: someJson",
messageSends: ["parse:"],
referencedClasses: ["JSON"]
}),
smalltalk.PersistentModel.klass);


