smalltalk.addPackage('Flow-Models', {});
smalltalk.addClass('Model', smalltalk.Object, ['announcer'], 'Flow-Models');
smalltalk.addMethod(
"_announce_",
smalltalk.method({
selector: "announce:",
fn: function (anAnnouncement) {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_announcer", []), "_announce_", [anAnnouncement]);
    return self;
}
}),
smalltalk.Model);

smalltalk.addMethod(
"_announcer",
smalltalk.method({
selector: "announcer",
fn: function () {
    var self = this;
    return ($receiver = self['@announcer']) == nil || $receiver == undefined ? function () {return smalltalk.send(self, "_initializeAnnouncer", []);}() : $receiver;
    return self;
}
}),
smalltalk.Model);

smalltalk.addMethod(
"_initializeAnnouncer",
smalltalk.method({
selector: "initializeAnnouncer",
fn: function () {
    var self = this;
    return self['@announcer'] = smalltalk.send(smalltalk.Announcer || Announcer, "_new", []);
    return self;
}
}),
smalltalk.Model);

smalltalk.addMethod(
"_on_do_",
smalltalk.method({
selector: "on:do:",
fn: function (anAnnouncementClass, aReactionBlock) {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_announcer", []), "_on_do_", [anAnnouncementClass, aReactionBlock]);
    return self;
}
}),
smalltalk.Model);


smalltalk.addMethod(
"_basePath",
smalltalk.method({
selector: "basePath",
fn: function () {
    var self = this;
    return "api";
    return self;
}
}),
smalltalk.Model.klass);

smalltalk.addMethod(
"_initialize",
smalltalk.method({
selector: "initialize",
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.Package || Package, "_defaultCommitPathSt_", ["/flow/commit"]);
    smalltalk.send(smalltalk.Package || Package, "_defaultCommitPathJs_", ["/flow/commit"]);
    return self;
}
}),
smalltalk.Model.klass);

smalltalk.addMethod(
"_jsonIgnoreInstanceVariables",
smalltalk.method({
selector: "jsonIgnoreInstanceVariables",
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_jsonIgnoreInstanceVariables", [], smalltalk.Model.klass.superclass || nil), "__comma", [["announcer"]]);
    return self;
}
}),
smalltalk.Model.klass);

smalltalk.addMethod(
"_modelPath",
smalltalk.method({
selector: "modelPath",
fn: function () {
    var self = this;
    return smalltalk.send(self, "_name", []);
    return self;
}
}),
smalltalk.Model.klass);

smalltalk.addMethod(
"_path",
smalltalk.method({
selector: "path",
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.send(smalltalk.send(self, "_basePath", []), "__comma", ["/"]), "__comma", [smalltalk.send(self, "_modelPath", [])]);
    return self;
}
}),
smalltalk.Model.klass);


smalltalk.addClass('JsonModel', smalltalk.Model, ['data'], 'Flow-Models');


smalltalk.addClass('PersistentModel', smalltalk.Model, ['data'], 'Flow-Models');
smalltalk.addMethod(
"__eq",
smalltalk.method({
selector: "=",
fn: function (anObject) {
    var self = this;
    return smalltalk.send(smalltalk.send(anObject, "_respondsTo_", [smalltalk.symbolFor("id")]), "_and_", [function () {return smalltalk.send(smalltalk.send(self, "_id", []), "__eq", [smalltalk.send(anObject, "_id", [])]);}]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_asJSONString",
smalltalk.method({
selector: "asJSONString",
fn: function () {
    var self = this;
    smalltalk.send(self, "_onAboutToJSON", []);
    return smalltalk.send(smalltalk.JSON || JSON, "_stringify_", [self['@data']]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_at_",
smalltalk.method({
selector: "at:",
fn: function (aKey) {
    var self = this;
    return smalltalk.send(self['@data'], "_at_", [aKey]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_at_put_",
smalltalk.method({
selector: "at:put:",
fn: function (aKey, anObject) {
    var self = this;
    smalltalk.send(self['@data'], "_at_put_", [aKey, anObject]);
    return anObject;
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_create",
smalltalk.method({
selector: "create",
fn: function () {
    var self = this;
    return smalltalk.send(self, "_createDo_", [function () {return nil;}]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_createDo_",
smalltalk.method({
selector: "createDo:",
fn: function (aBlock) {
    var self = this;
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_", [{url: smalltalk.send(self, "_path", []), type: "POST", cache: false, data: smalltalk.send(self, "_asJSONString", []), success: function (x) {return smalltalk.send(self, "_onAfterCreate_done_", [x, aBlock]);}, fail: function (x) {return smalltalk.send(smalltalk.ModelCreateError || ModelCreateError, "_signal_", [smalltalk.send(smalltalk.send(smalltalk.send("Could not create ", "__comma", [smalltalk.send(smalltalk.send(self, "_class", []), "_name", [])]), "__comma", [":  "]), "__comma", [smalltalk.send(x, "_responseText", [])])]);}, error: function (x) {return smalltalk.send(smalltalk.ModelCreateError || ModelCreateError, "_signal_", [smalltalk.send(smalltalk.send(smalltalk.send("Could not create ", "__comma", [smalltalk.send(smalltalk.send(self, "_class", []), "_name", [])]), "__comma", [":  "]), "__comma", [smalltalk.send(x, "_responseText", [])])]);}}]);
    return self;
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_createdOn",
smalltalk.method({
selector: "createdOn",
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
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_createdOn_",
smalltalk.method({
selector: "createdOn:",
fn: function (aDate) {
    var self = this;
    smalltalk.send(self, "_at_put_", [smalltalk.symbolFor("createdOn"), aDate]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_data",
smalltalk.method({
selector: "data",
fn: function () {
    var self = this;
    return self['@data'];
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_dateAndTimeAt_",
smalltalk.method({
selector: "dateAndTimeAt:",
fn: function (aSelector) {
    var self = this;
    return smalltalk.send(smalltalk.Date || Date, "_fromString_", [smalltalk.send(self, "_at_", [aSelector])]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_delete",
smalltalk.method({
selector: "delete",
fn: function () {
    var self = this;
    smalltalk.send(self, "_deleteDo_", [function () {return nil;}]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_deleteDo_",
smalltalk.method({
selector: "deleteDo:",
fn: function (aBlock) {
    var self = this;
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_", [{url: smalltalk.send(smalltalk.send(smalltalk.send(self, "_path", []), "__comma", ["?id="]), "__comma", [smalltalk.send(smalltalk.send(self, "_id", []), "_asString", [])]), type: "DELETE", cache: false, data: smalltalk.send(self, "_asJSONString", []), success: function (x) {return smalltalk.send(self, "_onAfterDelete_done_", [x, aBlock]);}, fail: function (x) {return smalltalk.send(self, "_onDeleteFail_", [x]);}, error: function (x) {return smalltalk.send(self, "_onDeleteFail_", [x]);}}]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_doesNotUnderstand_",
smalltalk.method({
selector: "doesNotUnderstand:",
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
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_id",
smalltalk.method({
selector: "id",
fn: function () {
    var self = this;
    return smalltalk.send(self['@data'], "_at_", [smalltalk.symbolFor("id")]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_id_",
smalltalk.method({
selector: "id:",
fn: function (aString) {
    var self = this;
    smalltalk.send(self['@data'], "_at_put_", [smalltalk.symbolFor("id"), aString]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_initialize",
smalltalk.method({
selector: "initialize",
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.PersistentModel.superclass || nil);
    self['@data'] = smalltalk.send(smalltalk.HashedCollection || HashedCollection, "_new", []);
    smalltalk.send(self, "_modelClass_", [smalltalk.send(smalltalk.send(self, "_class", []), "_name", [])]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_onAboutToJSON",
smalltalk.method({
selector: "onAboutToJSON",
fn: function () {
    var self = this;
    var obj = nil;
    var keys = nil;
    obj = Object;
    obj = smalltalk.send(smalltalk.JSObjectProxy || JSObjectProxy, "_on_", [obj]);
    keys = smalltalk.send(obj, "_keys_", [self['@data']]);
    smalltalk.send(keys, "_do_", [function (key) {var value = nil;value = smalltalk.send(self['@data'], "_at_", [key]);value = smalltalk.send(self, "_perform_", [smalltalk.send(key, "_asSymbol", [])]);($receiver = smalltalk.send(value, "_isKindOf_", [smalltalk.PersistentModel || PersistentModel])).klass === smalltalk.Boolean ? $receiver ? function () {smalltalk.send(value, "_onAboutToJSON", []);return value = smalltalk.send(value, "_data", []);}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {smalltalk.send(value, "_onAboutToJSON", []);return value = smalltalk.send(value, "_data", []);}]);return smalltalk.send(self['@data'], "_at_put_", [key, value]);}]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_onAfterCreate_done_",
smalltalk.method({
selector: "onAfterCreate:done:",
fn: function (x, aBlock) {
    var self = this;
    self['@data'] = smalltalk.send(smalltalk.send(self, "_class", []), "_reify_", [x]);
    smalltalk.send(self, "_announce_", [smalltalk.send(smalltalk.ModelCreated || ModelCreated, "_for_", [self])]);
    smalltalk.send(aBlock, "_value_", [self]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_onAfterDelete_done_",
smalltalk.method({
selector: "onAfterDelete:done:",
fn: function (x, aBlock) {
    var self = this;
    smalltalk.send(self, "_announce_", [smalltalk.send(smalltalk.ModelDeleted || ModelDeleted, "_for_", [self])]);
    smalltalk.send(aBlock, "_value_", [self]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_onAfterRefresh_done_",
smalltalk.method({
selector: "onAfterRefresh:done:",
fn: function (x, aBlock) {
    var self = this;
    smalltalk.send(self, "_syncWith_", [smalltalk.send(smalltalk.send(self, "_class", []), "_reify_", [x])]);
    smalltalk.send(self, "_announce_", [smalltalk.send(smalltalk.ModelRefreshed || ModelRefreshed, "_for_", [self])]);
    smalltalk.send(aBlock, "_value_", [self]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_onAfterSave_done_",
smalltalk.method({
selector: "onAfterSave:done:",
fn: function (x, aBlock) {
    var self = this;
    smalltalk.send(self, "_announce_", [smalltalk.send(smalltalk.ModelSaved || ModelSaved, "_for_", [self])]);
    smalltalk.send(aBlock, "_value_", [self]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_onDeleteFail_",
smalltalk.method({
selector: "onDeleteFail:",
fn: function (x) {
    var self = this;
    smalltalk.send(smalltalk.ModelUpdateError || ModelUpdateError, "_signal_", [smalltalk.send("Could not delete ", "__comma", [smalltalk.send(smalltalk.send(self, "_class", []), "_name", [])])]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_onRefreshFail_",
smalltalk.method({
selector: "onRefreshFail:",
fn: function (x) {
    var self = this;
    smalltalk.send(smalltalk.ModelRefreshError || ModelRefreshError, "_signal_", [smalltalk.send("Could not refresh ", "__comma", [smalltalk.send(smalltalk.send(self, "_class", []), "_name", [])])]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_onSaveFail_",
smalltalk.method({
selector: "onSaveFail:",
fn: function (x) {
    var self = this;
    smalltalk.send(smalltalk.ModelUpdateError || ModelUpdateError, "_signal_", [smalltalk.send("Could not save ", "__comma", [smalltalk.send(smalltalk.send(self, "_class", []), "_name", [])])]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_path",
smalltalk.method({
selector: "path",
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_class", []), "_path", []);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_refresh",
smalltalk.method({
selector: "refresh",
fn: function () {
    var self = this;
    smalltalk.send(self, "_refreshDo_", [function () {return nil;}]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_refreshDo_",
smalltalk.method({
selector: "refreshDo:",
fn: function (aBlock) {
    var self = this;
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_", [{url: smalltalk.send(smalltalk.send(smalltalk.send(self, "_path", []), "__comma", ["?id="]), "__comma", [smalltalk.send(smalltalk.send(self, "_id", []), "_asString", [])]), type: "GET", cache: false, success: function (x) {return smalltalk.send(self, "_onAfterRefresh_done_", [x, aBlock]);}, fail: function (x) {return smalltalk.send(self, "_onRefeshFail_", [x]);}, error: function (x) {return smalltalk.send(self, "_onRefreshFail_", [x]);}}]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_save",
smalltalk.method({
selector: "save",
fn: function () {
    var self = this;
    return smalltalk.send(self, "_saveDo_", [function () {return nil;}]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_saveDo_",
smalltalk.method({
selector: "saveDo:",
fn: function (aBlock) {
    var self = this;
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_", [{url: smalltalk.send(self, "_path", []), type: "PUT", cache: false, data: smalltalk.send(self, "_asJSONString", []), success: function (x) {return smalltalk.send(self, "_onAfterSave_done_", [x, aBlock]);}, fail: function (x) {return smalltalk.send(self, "_onSaveFail_", [x]);}, error: function (x) {return smalltalk.send(self, "_onSaveFail_", [x]);}}]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_subModelAt_",
smalltalk.method({
selector: "subModelAt:",
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
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_syncFrom_",
smalltalk.method({
selector: "syncFrom:",
fn: function (someJson) {
    var self = this;
    return smalltalk.send(self, "_syncWith_", [smalltalk.send(smalltalk.send(self, "_class", []), "_reify_", [someJson])]);
    return self;
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_syncWith_",
smalltalk.method({
selector: "syncWith:",
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
}
}),
smalltalk.PersistentModel);

smalltalk.addMethod(
"_url",
smalltalk.method({
selector: "url",
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.send(smalltalk.send(self, "_path", []), "__comma", ["/"]), "__comma", [smalltalk.send(smalltalk.send(self, "_id", []), "_asString", [])]);
    return self;
}
}),
smalltalk.PersistentModel);


smalltalk.addMethod(
"_atId_do_",
smalltalk.method({
selector: "atId:do:",
fn: function (anId, onDone) {
    var self = this;
    return smalltalk.send(self, "_read_do_", [anId, onDone]);
    return self;
}
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_create",
smalltalk.method({
selector: "create",
fn: function () {
    var self = this;
    return smalltalk.send(self, "_createDo_", [function () {return nil;}]);
    return self;
}
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_createDo_",
smalltalk.method({
selector: "createDo:",
fn: function (aBlock) {
    var self = this;
    var newInstance = nil;
    newInstance = smalltalk.send(smalltalk.send(self, "_basicNew", []), "_initialize", []);
    smalltalk.send(typeof jQuery == "undefined" ? nil : jQuery, "_ajax_", [{url: smalltalk.send(self, "_path", []), type: "POST", cache: false, data: smalltalk.send(newInstance, "_asJSONString", []), success: function (x) {return smalltalk.send(newInstance, "_onAfterCreate_done_", [x, aBlock]);}, fail: function (x) {return smalltalk.send(smalltalk.ModelCreateError || ModelCreateError, "_signal_", [smalltalk.send(smalltalk.send(smalltalk.send("Could not create ", "__comma", [smalltalk.send(self, "_name", [])]), "__comma", [":  "]), "__comma", [smalltalk.send(x, "_responseText", [])])]);}, error: function (x) {return smalltalk.send(smalltalk.ModelCreateError || ModelCreateError, "_signal_", [smalltalk.send(smalltalk.send(smalltalk.send("Could not create ", "__comma", [smalltalk.send(self, "_name", [])]), "__comma", [":  "]), "__comma", [smalltalk.send(x, "_responseText", [])])]);}}]);
    return newInstance;
    return self;
}
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_createdOnFrom_",
smalltalk.method({
selector: "createdOnFrom:",
fn: function (aReifiedJSON) {
    var self = this;
    return smalltalk.send(smalltalk.Date || Date, "_fromString_", [aReifiedJSON]);
    return self;
}
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_fromJson_",
smalltalk.method({
selector: "fromJson:",
fn: function (someJson) {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_new", []), "_syncFrom_", [someJson]);
    return self;
}
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_fromReified_",
smalltalk.method({
selector: "fromReified:",
fn: function (aReifiedJSON) {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_new", []), "_syncWith_", [aReifiedJSON]);
    return self;
}
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_manyFromJson_",
smalltalk.method({
selector: "manyFromJson:",
fn: function (someJson) {
    var self = this;
    return smalltalk.send(smalltalk.send(smalltalk.JSON || JSON, "_parse_", [someJson]), "_collect_", [function (each) {return smalltalk.send(self, "_fromReified_", [each]);}]);
    return self;
}
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_onAfterRead_done_",
smalltalk.method({
selector: "onAfterRead:done:",
fn: function (someJson, aBlock) {
    var self = this;
    var reified = nil;
    reified = smalltalk.send(self, "_fromJson_", [someJson]);
    smalltalk.send(aBlock, "_value_", [reified]);
    return reified;
    return self;
}
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_read_",
smalltalk.method({
selector: "read:",
fn: function (anId) {
    var self = this;
    smalltalk.send(self, "_read_do_", [anId, function () {return nil;}]);
    return self;
}
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_read_do_",
smalltalk.method({
selector: "read:do:",
fn: function (anId, aBlock){
var self=this;
smalltalk.send((typeof jQuery == 'undefined' ? nil : jQuery), "_ajax_", [{url: smalltalk.send(smalltalk.send(smalltalk.send(self, "_path", []), "__comma", ["?id="]), "__comma", [anId]),type: "GET",cache: false,success: (function(x){return smalltalk.send(self, "_onAfterRead_done_", [x, aBlock]);}),fail: (function(x){return smalltalk.send((smalltalk.ModelReadError || ModelReadError), "_signal_", [smalltalk.send(smalltalk.send(smalltalk.send("Could not read ", "__comma", [smalltalk.send(self, "_name", [])]), "__comma", [":  "]), "__comma", [smalltalk.send(x, "_responseText", [])])]);}),error: (function(x){return smalltalk.send((smalltalk.ModelReadError || ModelReadError), "_signal_", [smalltalk.send(smalltalk.send(smalltalk.send("Could not read ", "__comma", [smalltalk.send(self, "_name", [])]), "__comma", [":  "]), "__comma", [smalltalk.send(x, "_responseText", [])])]);})}]);
return self;}
}),
smalltalk.PersistentModel.klass);

smalltalk.addMethod(
"_reify_",
smalltalk.method({
selector: "reify:",
fn: function (someJson) {
    var self = this;
    return smalltalk.send(smalltalk.JSON || JSON, "_parse_", [someJson]);
    return self;
}
}),
smalltalk.PersistentModel.klass);


