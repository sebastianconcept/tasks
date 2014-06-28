smalltalk.addPackage('Flow-Presenters', {});
smalltalk.addClass('Presenter', smalltalk.Widget, ['children', 'model', 'announcer', 'parent', 'wrapper'], 'Flow-Presenters');
smalltalk.addMethod(
"_addAllSubPresentersTo_",
smalltalk.method({
selector: "addAllSubPresentersTo:",
fn: function (aSet) {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_subPpresenters", []), "_do_", [function (aPresenter) {return ($receiver = aPresenter) != nil && $receiver != undefined ? function () {smalltalk.send(aSet, "_add_", [aPresenter]);return smalltalk.send(aPresenter, "_addAllSubPresentersTo_", [aSet]);}() : nil;}]);
    return aSet;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_allSubPresenters",
smalltalk.method({
selector: "allSubPresenters",
fn: function () {
    var self = this;
    return smalltalk.send(self, "_addAllSubPresentersTo_", [smalltalk.send(smalltalk.Set || Set, "_new", [])]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_announce_",
smalltalk.method({
selector: "announce:",
fn: function (anAnnouncement) {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_announcer", []), "_announce_", [anAnnouncement]);
    ($receiver = smalltalk.send(anAnnouncement, "_isBubbling", [])).klass === smalltalk.Boolean ? $receiver ? function () {return ($receiver = smalltalk.send(self, "_parent", [])) != nil && $receiver != undefined ? function () {return smalltalk.send(smalltalk.send(self, "_parent", []), "_announce_", [anAnnouncement]);}() : nil;}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return ($receiver = smalltalk.send(self, "_parent", [])) != nil && $receiver != undefined ? function () {return smalltalk.send(smalltalk.send(self, "_parent", []), "_announce_", [anAnnouncement]);}() : nil;}]);
    return self;
}
}),
smalltalk.Presenter);

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
smalltalk.Presenter);

smalltalk.addMethod(
"_append_",
smalltalk.method({
selector: "append:",
fn: function (aPresenter) {
    var self = this;
    smalltalk.send(self['@wrapper'], "_append_", [smalltalk.send(aPresenter, "_asJQuery", [])]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_asJQuery",
smalltalk.method({
selector: "asJQuery",
fn: function () {
    var self = this;
    return smalltalk.send(self['@wrapper'], "_asJQuery", []);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_at_",
smalltalk.method({
selector: "at:",
fn: function (aKey) {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_children", []), "_at_", [aKey]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_at_ifAbsent_",
smalltalk.method({
selector: "at:ifAbsent:",
fn: function (aKey, aBlock) {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_children", []), "_at_ifAbsent_", [aKey, aBlock]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_at_put_",
smalltalk.method({
selector: "at:put:",
fn: function (aKey, aPresenter) {
    var self = this;
    smalltalk.send(self, "_childrenAt_put_", [aKey, aPresenter]);
    return aPresenter;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_children",
smalltalk.method({
selector: "children",
fn: function () {
    var self = this;
    return ($receiver = self['@children']) == nil || $receiver == undefined ? function () {return smalltalk.send(self, "_initializeChildren", []);}() : $receiver;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_childrenAt_",
smalltalk.method({
selector: "childrenAt:",
fn: function (aKey) {
    var self = this;
    return smalltalk.send(self, "_at_", [aKey]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_childrenAt_put_",
smalltalk.method({
selector: "childrenAt:put:",
fn: function (aKey, aPresenter) {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_children", []), "_at_put_", [aKey, aPresenter]);
    ($receiver = smalltalk.send(aPresenter, "_isPresenter", [])).klass === smalltalk.Boolean ? $receiver ? function () {return smalltalk.send(aPresenter, "_parent_", [self]);}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return smalltalk.send(aPresenter, "_parent_", [self]);}]);
    return aPresenter;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_componentsAt_put_",
smalltalk.method({
selector: "componentsAt:put:",
fn: function (aKey, aPresenter) {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_children", []), "_at_put_", [aKey, aPresenter]);
    ($receiver = smalltalk.send(aPresenter, "_isPresenter", [])).klass === smalltalk.Boolean ? $receiver ? function () {return smalltalk.send(aPresenter, "_parent_", [self]);}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return smalltalk.send(aPresenter, "_parent_", [self]);}]);
    return aPresenter;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_doesNotUnderstand_",
smalltalk.method({
selector: "doesNotUnderstand:",
fn: function (aMessage) {
    var self = this;
    return ($receiver = smalltalk.send(self, "_hasPresenterAt_", [smalltalk.send(smalltalk.send(aMessage, "_selector", []), "_asSymbol", [])])).klass === smalltalk.Boolean ? $receiver ? function () {return smalltalk.send(self, "_at_", [smalltalk.send(smalltalk.send(aMessage, "_selector", []), "_asSymbol", [])]);}() : function () {return smalltalk.send(self, "_doesNotUnderstand_", [aMessage], smalltalk.Presenter.superclass || nil);}() : smalltalk.send($receiver, "_ifTrue_ifFalse_", [function () {return smalltalk.send(self, "_at_", [smalltalk.send(smalltalk.send(aMessage, "_selector", []), "_asSymbol", [])]);}, function () {return smalltalk.send(self, "_doesNotUnderstand_", [aMessage], smalltalk.Presenter.superclass || nil);}]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_feedbackText_",
smalltalk.method({
selector: "feedbackText:",
fn: function (aString) {
    var self = this;
    smalltalk.send(self, "_neutralFeedbackText_", [aString]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_hasParent",
smalltalk.method({
selector: "hasParent",
fn: function () {
    var self = this;
    return smalltalk.send(self['@parent'], "_notNil", []);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_hasPresenterAt_",
smalltalk.method({
selector: "hasPresenterAt:",
fn: function (aKey) {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_children", []), "_includesKey_", [aKey]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_hide",
smalltalk.method({
selector: "hide",
fn: function () {
    var self = this;
    ($receiver = smalltalk.send(self, "_isPainted", [])).klass === smalltalk.Boolean ? $receiver ? function () {return smalltalk.send(smalltalk.send(self['@wrapper'], "_asJQuery", []), "_hide", []);}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return smalltalk.send(smalltalk.send(self['@wrapper'], "_asJQuery", []), "_hide", []);}]);
    smalltalk.send(self, "_onHide", []);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_hideAll",
smalltalk.method({
selector: "hideAll",
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(smalltalk.send(self, "_children", []), "_keys", []), "_do_", [function (each) {var child = nil;child = smalltalk.send(self, "_at_ifAbsent_", [each, function () {return nil;}]);return ($receiver = child) != nil && $receiver != undefined ? function () {return smalltalk.send(child, "_hide", []);}() : nil;}]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_ifAbsentAt_put_",
smalltalk.method({
selector: "ifAbsentAt:put:",
fn: function (aKey, aBlock) {
    var self = this;
    ($receiver = smalltalk.send(self, "_hasPresenterAt_", [aKey])).klass === smalltalk.Boolean ? !$receiver ? function () {return smalltalk.send(self, "_at_put_", [aKey, smalltalk.send(aBlock, "_value", [])]);}() : nil : smalltalk.send($receiver, "_ifFalse_", [function () {return smalltalk.send(self, "_at_put_", [aKey, smalltalk.send(aBlock, "_value", [])]);}]);
    return smalltalk.send(self, "_at_", [aKey]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_ifAbsentAt_put_andDo_",
smalltalk.method({
selector: "ifAbsentAt:put:andDo:",
fn: function (aKey, aBlock, anotherBlock) {
    var self = this;
    ($receiver = smalltalk.send(self, "_hasPresenterAt_", [aKey])).klass === smalltalk.Boolean ? !$receiver ? function () {smalltalk.send(self, "_at_put_", [aKey, smalltalk.send(aBlock, "_value", [])]);return smalltalk.send(anotherBlock, "_value", []);}() : nil : smalltalk.send($receiver, "_ifFalse_", [function () {smalltalk.send(self, "_at_put_", [aKey, smalltalk.send(aBlock, "_value", [])]);return smalltalk.send(anotherBlock, "_value", []);}]);
    return smalltalk.send(self, "_at_", [aKey]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_initialize",
smalltalk.method({
selector: "initialize",
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.Presenter.superclass || nil);
    painted = false;
    return self;
}
}),
smalltalk.Presenter);

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
smalltalk.Presenter);

smalltalk.addMethod(
"_initializeChildren",
smalltalk.method({
selector: "initializeChildren",
fn: function () {
    var self = this;
    return self['@children'] = smalltalk.send(smalltalk.Dictionary || Dictionary, "_new", []);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_initializeVisible",
smalltalk.method({
selector: "initializeVisible",
fn: function () {
    var self = this;
    return visible = false;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_isPainted",
smalltalk.method({
selector: "isPainted",
fn: function () {
    var self = this;
    return typeof painted == "undefined" ? nil : painted;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_isPresenter",
smalltalk.method({
selector: "isPresenter",
fn: function () {
    var self = this;
    return true;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_model",
smalltalk.method({
selector: "model",
fn: function () {
    var self = this;
    return self['@model'];
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_model_",
smalltalk.method({
selector: "model:",
fn: function (aModel) {
    var self = this;
    smalltalk.send(self, "_setModel_", [aModel]);
    smalltalk.send(self, "_onModelUpdated", []);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_negativeFeedbackText_",
smalltalk.method({
selector: "negativeFeedbackText:",
fn: function (aString) {
    var self = this;
    smalltalk.send(self, "_announce_", [smalltalk.send(smalltalk.send(smalltalk.FeedbackRequest || FeedbackRequest, "_for_", [aString]), "_beNegative", [])]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_neutralFeedbackText_",
smalltalk.method({
selector: "neutralFeedbackText:",
fn: function (aString) {
    var self = this;
    smalltalk.send(self, "_announce_", [smalltalk.send(smalltalk.send(smalltalk.FeedbackRequest || FeedbackRequest, "_for_", [aString]), "_beNeutral", [])]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_newCanvas",
smalltalk.method({
selector: "newCanvas",
fn: function () {
    var self = this;
    return smalltalk.send(self, "_newCanvasOn_", [smalltalk.send(self, "_asJQuery", [])]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_newCanvasOn_",
smalltalk.method({
selector: "newCanvasOn:",
fn: function (aJQuery) {
    var self = this;
    return smalltalk.send(smalltalk.HTMLCanvas || HTMLCanvas, "_onJQuery_", [aJQuery]);
    return self;
}
}),
smalltalk.Presenter);

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
smalltalk.Presenter);

smalltalk.addMethod(
"_onAboutToOpen",
smalltalk.method({
selector: "onAboutToOpen",
fn: function () {
    var self = this;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_onAboutToUpdateModel_",
smalltalk.method({
selector: "onAboutToUpdateModel:",
fn: function (aModel) {
    var self = this;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_onAfterOpen",
smalltalk.method({
selector: "onAfterOpen",
fn: function () {
    var self = this;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_onAfterPainted",
smalltalk.method({
selector: "onAfterPainted",
fn: function () {
    var self = this;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_onHide",
smalltalk.method({
selector: "onHide",
fn: function () {
    var self = this;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_onModelUpdated",
smalltalk.method({
selector: "onModelUpdated",
fn: function () {
    var self = this;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_onShow",
smalltalk.method({
selector: "onShow",
fn: function () {
    var self = this;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_open",
smalltalk.method({
selector: "open",
fn: function () {
    var self = this;
    smalltalk.send(self, "_onAboutToOpen", []);
    smalltalk.send(self, "_paintOnJQuery_", [smalltalk.send("body", "_asJQuery", [])]);
    smalltalk.send(self, "_onAfterOpen", []);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_paint_",
smalltalk.method({
selector: "paint:",
fn: function (aPresenter) {
    var self = this;
    smalltalk.send(function () {return smalltalk.send(aPresenter, "_paintOnJQuery_", [smalltalk.send(self, "_asJQuery", [])]);}, "_on_do_", [smalltalk.Error || Error, function (x) {return smalltalk.send(self, "_halt", []);}]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_paint_on_",
smalltalk.method({
selector: "paint:on:",
fn: function (aPresenter, html) {
    var self = this;
    smalltalk.send(aPresenter, "_renderOn_", [html]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_paintOn_",
smalltalk.method({
selector: "paintOn:",
fn: function (html) {
    var self = this;
    return self;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_paintOnJQuery_",
smalltalk.method({
selector: "paintOnJQuery:",
fn: function (aJQuery) {
    var self = this;
    smalltalk.send(self, "_appendToJQuery_", [aJQuery]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_parent",
smalltalk.method({
selector: "parent",
fn: function () {
    var self = this;
    return self['@parent'];
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_parent_",
smalltalk.method({
selector: "parent:",
fn: function (aPresenter) {
    var self = this;
    self['@parent'] = aPresenter;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_positiveFeedbackText_",
smalltalk.method({
selector: "positiveFeedbackText:",
fn: function (aString) {
    var self = this;
    smalltalk.send(self, "_announce_", [smalltalk.send(smalltalk.send(smalltalk.FeedbackRequest || FeedbackRequest, "_for_", [aString]), "_bePositive", [])]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_remove",
smalltalk.method({
selector: "remove",
fn: function () {
    var self = this;
    smalltalk.send(self, "_removeAll", []);
    ($receiver = self['@parent']) == nil || $receiver == undefined ? function () {return smalltalk.send(smalltalk.send(self, "_asJQuery", []), "_remove", []);}() : function () {return smalltalk.send(self['@parent'], "_remove_", [self]);}();
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_remove_",
smalltalk.method({
selector: "remove:",
fn: function (aPresenter) {
    var self = this;
    var target = nil;
    smalltalk.send(smalltalk.send(self, "_children", []), "_keysAndValuesDo_", [function (k, v) {return ($receiver = smalltalk.send(v, "__eq_eq", [aPresenter])).klass === smalltalk.Boolean ? $receiver ? function () {return target = k;}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return target = k;}]);}]);
    smalltalk.send(smalltalk.send(aPresenter, "_asJQuery", []), "_remove", []);
    smalltalk.send(smalltalk.send(self, "_children", []), "_removeKey_ifAbsent_", [target, function () {return nil;}]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_removeAll",
smalltalk.method({
selector: "removeAll",
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_subPresenters", []), "_do_", [function (e) {return smalltalk.send(e, "_remove", []);}]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_removeAt_",
smalltalk.method({
selector: "removeAt:",
fn: function (aKey) {
    var self = this;
    var target = nil;
    smalltalk.send(smalltalk.send(self, "_children", []), "_keysAndValuesDo_", [function (k, v) {return ($receiver = smalltalk.send(k, "__eq_eq", [aKey])).klass === smalltalk.Boolean ? $receiver ? function () {return target = v;}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return target = v;}]);}]);
    ($receiver = target) != nil && $receiver != undefined ? function () {return smalltalk.send(target, "_remove", []);}() : nil;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_renderOn_",
smalltalk.method({
selector: "renderOn:",
fn: function (html) {
    var self = this;
    self['@wrapper'] = smalltalk.send(html, "_div", []);
    (function ($rec) {smalltalk.send($rec, "_class_", [smalltalk.send(smalltalk.send(self, "_class", []), "_name", [])]);return smalltalk.send($rec, "_with_", [function () {return smalltalk.send(self, "_paintOn_", [html]);}]);}(self['@wrapper']));
    smalltalk.send(self, "_onAfterPainted", []);
    painted = true;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_setId_",
smalltalk.method({
selector: "setId:",
fn: function (aString) {
    var self = this;
    smalltalk.send(self['@wrapper'], "_id_", [aString]);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_setModel_",
smalltalk.method({
selector: "setModel:",
fn: function (aModel) {
    var self = this;
    smalltalk.send(self, "_onAboutToUpdateModel_", [aModel]);
    self['@model'] = aModel;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_show",
smalltalk.method({
selector: "show",
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(self['@wrapper'], "_asJQuery", []), "_show", []);
    smalltalk.send(self, "_onShow", []);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_subPresenters",
smalltalk.method({
selector: "subPresenters",
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_children", []), "_values", []);
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_tr",
smalltalk.method({
selector: "tr",
fn: function () {
    var self = this;
    return typeof tr == "undefined" ? nil : tr;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_visible",
smalltalk.method({
selector: "visible",
fn: function () {
    var self = this;
    return ($receiver = typeof visible == "undefined" ? nil : visible) == nil ||
        $receiver == undefined ? function () {return smalltalk.send(self, "_initializeVisible", []);}() : $receiver;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_visible_",
smalltalk.method({
selector: "visible:",
fn: function (aBoolean) {
    var self = this;
    visible = aBoolean;
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_wrapper",
smalltalk.method({
selector: "wrapper",
fn: function () {
    var self = this;
    return self['@wrapper'];
    return self;
}
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_wrapper_",
smalltalk.method({
selector: "wrapper:",
fn: function (aTagBrush) {
    var self = this;
    self['@wrapper'] = aTagBrush;
    return self;
}
}),
smalltalk.Presenter);


smalltalk.addMethod(
"_defaultModel",
smalltalk.method({
selector: "defaultModel",
fn: function () {
    var self = this;
    return nil;
    return self;
}
}),
smalltalk.Presenter.klass);

smalltalk.addMethod(
"_for_",
smalltalk.method({
selector: "for:",
fn: function (aModel) {
    var self = this;
    return function ($rec) {smalltalk.send($rec, "_model_", [aModel]);return smalltalk.send($rec, "_yourself", []);}(smalltalk.send(self, "_new", []));
    return self;
}
}),
smalltalk.Presenter.klass);

smalltalk.addMethod(
"_open",
smalltalk.method({
selector: "open",
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_for_", [smalltalk.send(self, "_defaultModel", [])]), "_open", []);
    return self;
}
}),
smalltalk.Presenter.klass);


smalltalk.addClass('Application', smalltalk.Presenter, ['feedback'], 'Flow-Presenters');
smalltalk.addMethod(
"_feedback",
smalltalk.method({
selector: "feedback",
fn: function () {
    var self = this;
    return ($receiver = self['@feedback']) == nil || $receiver == undefined ? function () {return smalltalk.send(self, "_initializeFeedback", []);}() : $receiver;
    return self;
}
}),
smalltalk.Application);

smalltalk.addMethod(
"_feedbackText_",
smalltalk.method({
selector: "feedbackText:",
fn: function (aString) {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_feedback", []), "_text_", [aString]);
    return self;
}
}),
smalltalk.Application);

smalltalk.addMethod(
"_initialize",
smalltalk.method({
selector: "initialize",
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.Application.superclass || nil);
    smalltalk.send(smalltalk.send(typeof window == "undefined" ? nil : window, "_jQuery_", [typeof window == "undefined" ? nil : window]), "_bind_do_", ["hashchange", function () {return smalltalk.send(self, "_onHash", []);}]);
    return self;
}
}),
smalltalk.Application);

smalltalk.addMethod(
"_initializeFeedback",
smalltalk.method({
selector: "initializeFeedback",
fn: function () {
    var self = this;
    return self['@feedback'] = smalltalk.send(self, "_makeFeedback", []);
    return self;
}
}),
smalltalk.Application);

smalltalk.addMethod(
"_makeFeedback",
smalltalk.method({
selector: "makeFeedback",
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.Feedback || Feedback, "_new", []);
    return self;
}
}),
smalltalk.Application);

smalltalk.addMethod(
"_negativeFeedbackText_",
smalltalk.method({
selector: "negativeFeedbackText:",
fn: function (aString) {
    var self = this;
    (function ($rec) {smalltalk.send($rec, "_beNegative", []);return smalltalk.send($rec, "_text_", [aString]);}(smalltalk.send(self, "_feedback", [])));
    return self;
}
}),
smalltalk.Application);

smalltalk.addMethod(
"_neutralFeedbackText_",
smalltalk.method({
selector: "neutralFeedbackText:",
fn: function (aString) {
    var self = this;
    (function ($rec) {smalltalk.send($rec, "_beNeutral", []);return smalltalk.send($rec, "_text_", [aString]);}(smalltalk.send(self, "_feedback", [])));
    return self;
}
}),
smalltalk.Application);

smalltalk.addMethod(
"_onAfterOpen",
smalltalk.method({
selector: "onAfterOpen",
fn: function () {
    var self = this;
    smalltalk.send(self, "_onAfterOpen", [], smalltalk.Application.superclass || nil);
    smalltalk.send(typeof window == "undefined" ? nil : window, "_at_put_", ["app", self]);
    return self;
}
}),
smalltalk.Application);

smalltalk.addMethod(
"_onFeedback_",
smalltalk.method({
selector: "onFeedback:",
fn: function (anAnnouncement) {
    var self = this;
    var polarity = nil;
    polarity = smalltalk.send(anAnnouncement, "_polarity", []);
    smalltalk.send(self, "_perform_withArguments_", [smalltalk.send(polarity, "__comma", ["FeedbackText:"]), smalltalk.send(smalltalk.Array || Array, "_with_", [smalltalk.send(anAnnouncement, "_subject", [])])]);
    return self;
}
}),
smalltalk.Application);

smalltalk.addMethod(
"_onHash",
smalltalk.method({
selector: "onHash",
fn: function () {
    var self = this;
    return self;
}
}),
smalltalk.Application);

smalltalk.addMethod(
"_positiveFeedbackText_",
smalltalk.method({
selector: "positiveFeedbackText:",
fn: function (aString) {
    var self = this;
    (function ($rec) {smalltalk.send($rec, "_bePositive", []);return smalltalk.send($rec, "_text_", [aString]);}(smalltalk.send(self, "_feedback", [])));
    return self;
}
}),
smalltalk.Application);


smalltalk.Application.klass.iVarNames = ['login'];
smalltalk.addMethod(
"_login",
smalltalk.method({
selector: "login",
fn: function () {
    var self = this;
    return self['@login'];
    return self;
}
}),
smalltalk.Application.klass);

smalltalk.addMethod(
"_login_",
smalltalk.method({
selector: "login:",
fn: function (aLogin) {
    var self = this;
    self['@login'] = aLogin;
    return self;
}
}),
smalltalk.Application.klass);


smalltalk.addClass('Feedback', smalltalk.Presenter, ['content', 'text'], 'Flow-Presenters');
smalltalk.addMethod(
"_beNegative",
smalltalk.method({
selector: "beNegative",
fn: function () {
    var self = this;
    smalltalk.send(self, "_removePolarity", []);
    smalltalk.send(smalltalk.send("#feedbackWrapper", "_asJQuery", []), "_addClass_", ["negativeFeedback"]);
    return self;
}
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_beNeutral",
smalltalk.method({
selector: "beNeutral",
fn: function () {
    var self = this;
    smalltalk.send(self, "_removePolarity", []);
    smalltalk.send(smalltalk.send("#feedbackWrapper", "_asJQuery", []), "_addClass_", ["neutralFeedback"]);
    return self;
}
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_bePositive",
smalltalk.method({
selector: "bePositive",
fn: function () {
    var self = this;
    smalltalk.send(self, "_removePolarity", []);
    smalltalk.send(smalltalk.send("#feedbackWrapper", "_asJQuery", []), "_addClass_", ["positiveFeedback"]);
    return self;
}
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_initialize",
smalltalk.method({
selector: "initialize",
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.Feedback.superclass || nil);
    self['@text'] = smalltalk.send(smalltalk.String || String, "_new", []);
    return self;
}
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_onClose",
smalltalk.method({
selector: "onClose",
fn: function () {
    var self = this;
    smalltalk.send(self, "_hide", []);
    return self;
}
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_paintButtonOn_",
smalltalk.method({
selector: "paintButtonOn:",
fn: function (html) {
    var self = this;
    (function ($rec) {smalltalk.send($rec, "_id_", ["feedbackClose"]);smalltalk.send($rec, "_with_", ["Close"]);return smalltalk.send($rec, "_onClick_", [function () {return smalltalk.send(self, "_onClose", []);}]);}(smalltalk.send(html, "_button", [])));
    return self;
}
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_paintContentOn_",
smalltalk.method({
selector: "paintContentOn:",
fn: function (html) {
    var self = this;
    self['@content'] = function ($rec) {smalltalk.send($rec, "_id_", ["feedbackContentWrapper"]);return smalltalk.send($rec, "_yourself", []);}(smalltalk.send(html, "_div", []));
    return self;
}
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_paintOn_",
smalltalk.method({
selector: "paintOn:",
fn: function (html) {
    var self = this;
    smalltalk.send(self['@wrapper'], "_id_", ["feedback"]);
    smalltalk.send(smalltalk.send(self['@wrapper'], "_asJQuery", []), "_hide", []);
    (function ($rec) {smalltalk.send($rec, "_id_", ["feedbackWrapper"]);return smalltalk.send($rec, "_with_", [function () {return function ($rec) {smalltalk.send($rec, "_id_", ["feedbackContent"]);return smalltalk.send($rec, "_with_", [function () {smalltalk.send(self, "_paintContentOn_", [html]);return smalltalk.send(self, "_paintButtonOn_", [html]);}]);}(smalltalk.send(html, "_div", []));}]);}(smalltalk.send(html, "_div", [])));
    return self;
}
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_refresh",
smalltalk.method({
selector: "refresh",
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(self['@content'], "_asJQuery", []), "_html_", [self['@text']]);
    return self;
}
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_removePolarity",
smalltalk.method({
selector: "removePolarity",
fn: function () {
    var self = this;
    smalltalk.send(["neutralFeedback", "neutralFeedback", "neutralFeedback"], "_do_", [function (cssClass) {return smalltalk.send(smalltalk.send(self['@wrapper'], "_asJQuery", []), "_removeClass_", [cssClass]);}]);
    return self;
}
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_show",
smalltalk.method({
selector: "show",
fn: function () {
    var self = this;
    (function ($rec) {smalltalk.send($rec, "_slideDown_", [500]);return smalltalk.send($rec, "_fadeIn_do_", [500, function () {return smalltalk.send(typeof window == "undefined" ? nil : window, "_setTimeout_delay_", [function () {return function ($rec) {smalltalk.send($rec, "_slideUp_", [500]);return smalltalk.send($rec, "_fadeOut_", [500]);}(smalltalk.send(self['@wrapper'], "_asJQuery", []));}, 3000]);}]);}(smalltalk.send(self['@wrapper'], "_asJQuery", [])));
    return self;
}
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_text",
smalltalk.method({
selector: "text",
fn: function () {
    var self = this;
    return self['@text'];
    return self;
}
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_text_",
smalltalk.method({
selector: "text:",
fn: function (aString) {
    var self = this;
    self['@text'] = aString;
    smalltalk.send(self, "_refresh", []);
    smalltalk.send(self, "_show", []);
    return self;
}
}),
smalltalk.Feedback);



smalltalk.addClass('ItemsPresenter', smalltalk.Presenter, ['items', 'itemsGetter', 'loader'], 'Flow-Presenters');
smalltalk.addMethod(
"_atItemId_",
smalltalk.method({
selector: "atItemId:",
fn: function (anId) {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_items", []), "_detect_", [function (e) {return smalltalk.send(smalltalk.send(e, "_id", []), "__eq", [anId]);}]);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_getItemsDo_",
smalltalk.method({
selector: "getItemsDo:",
fn: function (aBlock) {
    var self = this;
    return smalltalk.send(self, "_subclassResponsibility", []);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_hasItems",
smalltalk.method({
selector: "hasItems",
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.send(self['@items'], "_notNil", []), "_and_", [function () {return smalltalk.send(self['@items'], "_notEmpty", []);}]);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_hideLoader",
smalltalk.method({
selector: "hideLoader",
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(smalltalk.send(smalltalk.send(self, "_loader", []), "_asJQuery", []), "_hide", []), "_fadeOut_", [0.5]);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_initializeItemsGetter",
smalltalk.method({
selector: "initializeItemsGetter",
fn: function () {
    var self = this;
    return self['@itemsGetter'] = smalltalk.send(self, "_makeItemsGetter", []);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_initializeLoader",
smalltalk.method({
selector: "initializeLoader",
fn: function () {
    var self = this;
    return self['@loader'] = smalltalk.send(self, "_makeLoader", []);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_isLoaded",
smalltalk.method({
selector: "isLoaded",
fn: function () {
    var self = this;
    return smalltalk.send(self['@items'], "_notNil", []);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_itemPresenters",
smalltalk.method({
selector: "itemPresenters",
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_items", []), "_collect_", [function (item) {return smalltalk.send(self, "_presenterFor_", [item]);}]);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_items",
smalltalk.method({
selector: "items",
fn: function () {
    var self = this;
    return self['@items'];
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_items_",
smalltalk.method({
selector: "items:",
fn: function (someModels) {
    var self = this;
    self['@items'] = someModels;
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_itemsDo_",
smalltalk.method({
selector: "itemsDo:",
fn: function (aBlock) {
    var self = this;
    smalltalk.send(self['@items'], "_ifNil_ifNotNil_", [function () {return smalltalk.send(self, "_loadItemsDo_", [aBlock]);}, aBlock]);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_itemsGetter",
smalltalk.method({
selector: "itemsGetter",
fn: function () {
    var self = this;
    return ($receiver = self['@itemsGetter']) == nil || $receiver == undefined ? function () {return smalltalk.send(self, "_initializeItemsGetter", []);}() : $receiver;
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_itemsGetter_",
smalltalk.method({
selector: "itemsGetter:",
fn: function (aBlock) {
    var self = this;
    self['@itemsGetter'] = aBlock;
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_loadAndPaintOn_",
smalltalk.method({
selector: "loadAndPaintOn:",
fn: function (html) {
    var self = this;
    smalltalk.send(self, "_loadAndPaintOn_done_", [html, function () {return nil;}]);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_loadAndPaintOn_done_",
smalltalk.method({
selector: "loadAndPaintOn:done:",
fn: function (html, aBlock) {
    var self = this;
    smalltalk.send(self, "_onAboutToLoad", []);
    smalltalk.send(self, "_itemsDo_", [function () {smalltalk.send(self, "_paintItemsOn_", [html]);smalltalk.send(self, "_onAfterLoaded", []);return smalltalk.send(aBlock, "_value", []);}]);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_loadItemsDo_",
smalltalk.method({
selector: "loadItemsDo:",
fn: function (aBlock) {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_itemsGetter", []), "_value_", [aBlock]);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_loader",
smalltalk.method({
selector: "loader",
fn: function () {
    var self = this;
    return ($receiver = self['@loader']) == nil || $receiver == undefined ? function () {return smalltalk.send(self, "_initializeLoader", []);}() : $receiver;
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_makeItemsFromJson_",
smalltalk.method({
selector: "makeItemsFromJson:",
fn: function (someJson) {
    var self = this;
    smalltalk.send(self, "_subclassResponsibility", []);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_makeItemsGetter",
smalltalk.method({
selector: "makeItemsGetter",
fn: function () {
    var self = this;
    return function (onDone) {return smalltalk.send(self, "_getItemsDo_", [onDone]);};
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_makeLoader",
smalltalk.method({
selector: "makeLoader",
fn: function () {
    var self = this;
    smalltalk.send(self, "_subclassResponsibility", []);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_makePresenterFor_",
smalltalk.method({
selector: "makePresenterFor:",
fn: function (anItem) {
    var self = this;
    var itemPresenter = nil;
    itemPresenter = smalltalk.send(smalltalk.send(self, "_presenterClassFor_", [anItem]), "_new", []);
    smalltalk.send(self, "_onModel_for_", [anItem, itemPresenter]);
    return itemPresenter;
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_observeItemPresenter_",
smalltalk.method({
selector: "observeItemPresenter:",
fn: function (anItemPresenter) {
    var self = this;
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_onAboutToLoad",
smalltalk.method({
selector: "onAboutToLoad",
fn: function () {
    var self = this;
    smalltalk.send(self, "_showLoader", []);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_onAfterLoad_do_",
smalltalk.method({
selector: "onAfterLoad:do:",
fn: function (someJson, onDone) {
    var self = this;
    self['@items'] = smalltalk.send(self, "_makeItemsFromJson_", [someJson]);
    smalltalk.send(onDone, "_value", []);
    smalltalk.send(self, "_announce_", [smalltalk.send(smalltalk.ItemsLoaded || ItemsLoaded, "_new", [])]);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_onAfterLoaded",
smalltalk.method({
selector: "onAfterLoaded",
fn: function () {
    var self = this;
    smalltalk.send(self, "_hideLoader", []);
    smalltalk.send(self, "_announce_", [smalltalk.send(smalltalk.ItemsLoaded || ItemsLoaded, "_new", [])]);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_onModel_for_",
smalltalk.method({
selector: "onModel:for:",
fn: function (anItem, anItemPresenter) {
    var self = this;
    smalltalk.send(anItemPresenter, "_model_", [anItem]);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_paintItemsOn_",
smalltalk.method({
selector: "paintItemsOn:",
fn: function (html) {
    var self = this;
    smalltalk.send(self['@items'], "_do_", [function (item) {return smalltalk.send(self, "_paint_", [smalltalk.send(self, "_presenterFor_", [item])]);}]);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_paintOn_",
smalltalk.method({
selector: "paintOn:",
fn: function (html) {
    var self = this;
    smalltalk.send(self, "_loadAndPaintOn_", [html]);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_presenterClassFor_",
smalltalk.method({
selector: "presenterClassFor:",
fn: function (anItem) {
    var self = this;
    smalltalk.send(self, "_subclassResponsibility", []);
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_presenterFor_",
smalltalk.method({
selector: "presenterFor:",
fn: function (anItem) {
    var self = this;
    var itemPresenter = nil;
    itemPresenter = smalltalk.send(self, "_ifAbsentAt_put_andDo_", [anItem, function () {return smalltalk.send(self, "_makePresenterFor_", [anItem]);}, function () {return smalltalk.send(self, "_observeItemPresenter_", [smalltalk.send(self, "_at_", [anItem])]);}]);
    return itemPresenter;
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_refresh",
smalltalk.method({
selector: "refresh",
fn: function () {
    var self = this;
    var $early = {};
    try {
        ($receiver = smalltalk.send(self, "_isLoaded", [])).klass === smalltalk.Boolean ? !$receiver ? function () {return function () {throw $early = [self];}();}() : nil : smalltalk.send($receiver, "_ifFalse_", [function () {return function () {throw $early = [self];}();}]);
        smalltalk.send(self, "_reset", []);
        smalltalk.send(self, "_loadAndPaintOn_", [smalltalk.send(self, "_newCanvas", [])]);
        return self;
    } catch (e) {
        if (e === $early) {
            return e[0];
        }
        throw e;
    }
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_refreshDo_",
smalltalk.method({
selector: "refreshDo:",
fn: function (aBlock) {
    var self = this;
    var $early = {};
    try {
        ($receiver = smalltalk.send(self, "_isLoaded", [])).klass === smalltalk.Boolean ? !$receiver ? function () {return function () {throw $early = [self];}();}() : nil : smalltalk.send($receiver, "_ifFalse_", [function () {return function () {throw $early = [self];}();}]);
        smalltalk.send(self, "_reset", []);
        smalltalk.send(self, "_loadAndPaintOn_done_", [smalltalk.send(self, "_newCanvas", []), aBlock]);
        return self;
    } catch (e) {
        if (e === $early) {
            return e[0];
        }
        throw e;
    }
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_reset",
smalltalk.method({
selector: "reset",
fn: function () {
    var self = this;
    smalltalk.send(self, "_removeAll", []);
    self['@items'] = nil;
    return self;
}
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_showLoader",
smalltalk.method({
selector: "showLoader",
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(smalltalk.send(smalltalk.send(self, "_loader", []), "_asJQuery", []), "_hide", []), "_fadeIn", []);
    return self;
}
}),
smalltalk.ItemsPresenter);



smalltalk.addClass('BunchPresenter', smalltalk.ItemsPresenter, ['bunchSize', 'index', 'atEnd', 'lastItems', 'more'], 'Flow-Presenters');
smalltalk.addMethod(
"_addItems_",
smalltalk.method({
selector: "addItems:",
fn: function (someItems) {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_items", []), "_addAll_", [someItems]);
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_atEnd",
smalltalk.method({
selector: "atEnd",
fn: function () {
    var self = this;
    return self['@atEnd'];
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_bunchSize",
smalltalk.method({
selector: "bunchSize",
fn: function () {
    var self = this;
    return ($receiver = self['@bunchSize']) == nil || $receiver == undefined ? function () {return smalltalk.send(self, "_initializeBunchSize", []);}() : $receiver;
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_bunchSize_",
smalltalk.method({
selector: "bunchSize:",
fn: function (anInteger) {
    var self = this;
    self['@bunchSize'] = anInteger;
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_end",
smalltalk.method({
selector: "end",
fn: function () {
    var self = this;
    return ($receiver = smalltalk.send(self, "_index", [])).klass === smalltalk.Number ? $receiver * smalltalk.send(self, "_bunchSize", []) : smalltalk.send($receiver, "__star", [smalltalk.send(self, "_bunchSize", [])]);
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_getItemsDo_",
smalltalk.method({
selector: "getItemsDo:",
fn: function (aBlock) {
    var self = this;
    return smalltalk.send(self, "_subclassResponsibility", []);
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_index",
smalltalk.method({
selector: "index",
fn: function () {
    var self = this;
    return ($receiver = self['@index']) == nil || $receiver == undefined ? function () {return smalltalk.send(self, "_initializeIndex", []);}() : $receiver;
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_index_",
smalltalk.method({
selector: "index:",
fn: function (anInteger) {
    var self = this;
    self['@index'] = anInteger;
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_initialize",
smalltalk.method({
selector: "initialize",
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.BunchPresenter.superclass || nil);
    self['@atEnd'] = false;
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_initializeBunchSize",
smalltalk.method({
selector: "initializeBunchSize",
fn: function () {
    var self = this;
    return self['@bunchSize'] = smalltalk.send(smalltalk.send(self, "_class", []), "_defaultBunchSize", []);
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_initializeIndex",
smalltalk.method({
selector: "initializeIndex",
fn: function () {
    var self = this;
    return self['@index'] = 1;
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_initializeItems",
smalltalk.method({
selector: "initializeItems",
fn: function () {
    var self = this;
    return self['@items'] = smalltalk.send(smalltalk.Array || Array, "_new", []);
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_initializeMore",
smalltalk.method({
selector: "initializeMore",
fn: function () {
    var self = this;
    return self['@more'] = smalltalk.send(self, "_makeMore", []);
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_items",
smalltalk.method({
selector: "items",
fn: function () {
    var self = this;
    return ($receiver = self['@items']) == nil || $receiver == undefined ? function () {return smalltalk.send(self, "_initializeItems", []);}() : $receiver;
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_itemsDo_",
smalltalk.method({
selector: "itemsDo:",
fn: function (aBlock) {
    var self = this;
    smalltalk.send(self, "_loadItemsDo_", [aBlock]);
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_makeMore",
smalltalk.method({
selector: "makeMore",
fn: function () {
    var self = this;
    smalltalk.send(self, "_subclassResponsibility", []);
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_more",
smalltalk.method({
selector: "more",
fn: function () {
    var self = this;
    return ($receiver = self['@more']) == nil || $receiver == undefined ? function () {return smalltalk.send(self, "_initializeMore", []);}() : $receiver;
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_nextBunch",
smalltalk.method({
selector: "nextBunch",
fn: function () {
    var self = this;
    return smalltalk.send(self, "_index_", [($receiver = smalltalk.send(self, "_index", [])).klass === smalltalk.Number ? $receiver + 1 : smalltalk.send($receiver, "__plus", [1])]);
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_onAfterLoad_do_",
smalltalk.method({
selector: "onAfterLoad:do:",
fn: function (someJson, onDone) {
    var self = this;
    self['@lastItems'] = smalltalk.send(self, "_makeItemsFromJson_", [someJson]);
    smalltalk.send(self, "_addItems_", [self['@lastItems']]);
    ($receiver = ($receiver = smalltalk.send(self, "_bunchSize", [])).klass === smalltalk.Number ? $receiver > smalltalk.send(self['@lastItems'], "_size", []) : smalltalk.send($receiver, "__gt", [smalltalk.send(self['@lastItems'], "_size", [])])).klass === smalltalk.Boolean ? $receiver ? function () {return self['@atEnd'] = true;}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return self['@atEnd'] = true;}]);
    smalltalk.send(onDone, "_value", []);
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_onAfterLoaded",
smalltalk.method({
selector: "onAfterLoaded",
fn: function () {
    var self = this;
    smalltalk.send(self, "_onAfterLoaded", [], smalltalk.BunchPresenter.superclass || nil);
    smalltalk.send(smalltalk.send(smalltalk.send(self, "_wrapper", []), "_asJQuery", []), "_append_", [smalltalk.send(smalltalk.send(self, "_loader", []), "_asJQuery", [])]);
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_paintItemsOn_",
smalltalk.method({
selector: "paintItemsOn:",
fn: function (html) {
    var self = this;
    smalltalk.send(self['@lastItems'], "_do_", [function (item) {return smalltalk.send(self, "_paint_", [smalltalk.send(self, "_presenterFor_", [item])]);}]);
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_removeItems_",
smalltalk.method({
selector: "removeItems:",
fn: function (someItems) {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_items", []), "_removeAll_", [someItems]);
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_reset",
smalltalk.method({
selector: "reset",
fn: function () {
    var self = this;
    smalltalk.send(self, "_reset", [], smalltalk.BunchPresenter.superclass || nil);
    self['@index'] = nil;
    self['@atEnd'] = false;
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_showLoader",
smalltalk.method({
selector: "showLoader",
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(smalltalk.send(smalltalk.send(self, "_loader", []), "_asJQuery", []), "_hide", []), "_fadeIn", []);
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_showMore",
smalltalk.method({
selector: "showMore",
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(smalltalk.send(smalltalk.send(self, "_more", []), "_asJQuery", []), "_hide", []), "_fadeIn", []);
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_showNext",
smalltalk.method({
selector: "showNext",
fn: function () {
    var self = this;
    var html = nil;
    html = smalltalk.send(self, "_newCanvas", []);
    smalltalk.send(self, "_nextBunch", []);
    smalltalk.send(self, "_loadAndPaintOn_", [html]);
    return self;
}
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_start",
smalltalk.method({
selector: "start",
fn: function () {
    var self = this;
    return ($receiver = ($receiver = ($receiver = smalltalk.send(self, "_index", [])).klass === smalltalk.Number ? $receiver - 1 : smalltalk.send($receiver, "__minus", [1])).klass === smalltalk.Number ? $receiver * smalltalk.send(self, "_bunchSize", []) : smalltalk.send($receiver, "__star", [smalltalk.send(self, "_bunchSize", [])])).klass === smalltalk.Number ? $receiver + 1 : smalltalk.send($receiver, "__plus", [1]);
    return self;
}
}),
smalltalk.BunchPresenter);


smalltalk.addMethod(
"_defaultBunchSize",
smalltalk.method({
selector: "defaultBunchSize",
fn: function () {
    var self = this;
    return 7;
    return self;
}
}),
smalltalk.BunchPresenter.klass);


smalltalk.addClass('TagBrushPresenter', smalltalk.Presenter, [], 'Flow-Presenters');

smalltalk.addMethod(
"_for_",
smalltalk.method({
selector: "for:",
fn: function (aTagBrush) {
    var self = this;
    return function ($rec) {smalltalk.send($rec, "_wrapper_", [aTagBrush]);return smalltalk.send($rec, "_yourself", []);}(smalltalk.send(self, "_new", []));
    return self;
}
}),
smalltalk.TagBrushPresenter.klass);


