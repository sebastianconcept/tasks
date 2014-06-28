smalltalk.addPackage('Flow-Presenters', {});
smalltalk.addClass('Presenter', smalltalk.Widget, ['children', 'model', 'announcer', 'parent', 'wrapper'], 'Flow-Presenters');
smalltalk.addMethod(
"_addAllSubPresentersTo_",
smalltalk.method({
selector: "addAllSubPresentersTo:",
category: 'accessing',
fn: function (aSet) {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_subPpresenters", []), "_do_", [function (aPresenter) {return ($receiver = aPresenter) != nil && $receiver != undefined ? function () {smalltalk.send(aSet, "_add_", [aPresenter]);return smalltalk.send(aPresenter, "_addAllSubPresentersTo_", [aSet]);}() : nil;}]);
    return aSet;
    return self;
},
args: ["aSet"],
source: "addAllSubPresentersTo: aSet\x0a\x09\x22Adds all the sub presenters of the receiver to aSet.\x22\x0a\x09\x0a\x09self subPpresenters do:[:aPresenter|\x0a\x09\x09aPresenter ifNotNil:[\x0a\x09\x09\x09aSet add: aPresenter.\x0a\x09\x09\x09aPresenter addAllSubPresentersTo: aSet]].\x0a\x09\x0a\x09^ aSet\x0a",
messageSends: ["do:", "subPpresenters", "ifNotNil:", "add:", "addAllSubPresentersTo:"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_allSubPresenters",
smalltalk.method({
selector: "allSubPresenters",
category: 'accessing',
fn: function () {
    var self = this;
    return smalltalk.send(self, "_addAllSubPresentersTo_", [smalltalk.send(smalltalk.Set || Set, "_new", [])]);
    return self;
},
args: [],
source: "allSubPresenters\x0a\x0a\x09^ self addAllSubPresentersTo: Set new",
messageSends: ["addAllSubPresentersTo:", "new"],
referencedClasses: ["Set"]
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_announce_",
smalltalk.method({
selector: "announce:",
category: 'accessing',
fn: function (anAnnouncement) {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_announcer", []), "_announce_", [anAnnouncement]);
    ($receiver = smalltalk.send(anAnnouncement, "_isBubbling", [])).klass === smalltalk.Boolean ? $receiver ? function () {return ($receiver = smalltalk.send(self, "_parent", [])) != nil && $receiver != undefined ? function () {return smalltalk.send(smalltalk.send(self, "_parent", []), "_announce_", [anAnnouncement]);}() : nil;}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return ($receiver = smalltalk.send(self, "_parent", [])) != nil && $receiver != undefined ? function () {return smalltalk.send(smalltalk.send(self, "_parent", []), "_announce_", [anAnnouncement]);}() : nil;}]);
    return self;
},
args: ["anAnnouncement"],
source: "announce: anAnnouncement\x0a\x09\x22Announce (bubbling through parents)\x22\x0a\x09\x0a\x09self announcer announce: anAnnouncement.\x0a\x0a\x09anAnnouncement isBubbling ifTrue:[\x0a\x09\x09self parent ifNotNil:[\x0a\x09\x09\x09self parent announce: anAnnouncement]]\x0a",
messageSends: ["announce:", "announcer", "ifTrue:", "isBubbling", "ifNotNil:", "parent"],
referencedClasses: []
}),
smalltalk.Presenter);

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
smalltalk.Presenter);

smalltalk.addMethod(
"_append_",
smalltalk.method({
selector: "append:",
category: 'actions',
fn: function (aPresenter) {
    var self = this;
    smalltalk.send(self['@wrapper'], "_append_", [smalltalk.send(aPresenter, "_asJQuery", [])]);
    return self;
},
args: ["aPresenter"],
source: "append: aPresenter\x0a\x09\x22Appends aPresenter to this presenter.\x0a\x09Note: it's actually appended to the wrapper of this guy.\x22\x0a\x0a\x09wrapper append: aPresenter asJQuery\x0a",
messageSends: ["append:", "asJQuery"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_asJQuery",
smalltalk.method({
selector: "asJQuery",
category: 'accessing',
fn: function () {
    var self = this;
    return smalltalk.send(self['@wrapper'], "_asJQuery", []);
    return self;
},
args: [],
source: "asJQuery\x0a\x09\x0a\x09^ wrapper asJQuery\x0a",
messageSends: ["asJQuery"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_at_",
smalltalk.method({
selector: "at:",
category: 'accessing',
fn: function (aKey) {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_children", []), "_at_", [aKey]);
    return self;
},
args: ["aKey"],
source: "at: aKey\x0a\x09\x22Answers the (sub)widget stored in this receiver at aKey\x22\x0a\x09^ self children at: aKey",
messageSends: ["at:", "children"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_at_ifAbsent_",
smalltalk.method({
selector: "at:ifAbsent:",
category: 'accessing',
fn: function (aKey, aBlock) {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_children", []), "_at_ifAbsent_", [aKey, aBlock]);
    return self;
},
args: ["aKey", "aBlock"],
source: "at: aKey ifAbsent: aBlock\x0a\x0a\x09^ self children at: aKey ifAbsent: aBlock",
messageSends: ["at:ifAbsent:", "children"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_at_put_",
smalltalk.method({
selector: "at:put:",
category: 'accessing',
fn: function (aKey, aPresenter) {
    var self = this;
    smalltalk.send(self, "_childrenAt_put_", [aKey, aPresenter]);
    return aPresenter;
    return self;
},
args: ["aKey", "aPresenter"],
source: "at: aKey put: aPresenter \x0a\x0a\x09self childrenAt: aKey put: aPresenter.\x0a\x0a\x09^ aPresenter ",
messageSends: ["childrenAt:put:"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_children",
smalltalk.method({
selector: "children",
category: 'accessing',
fn: function () {
    var self = this;
    return ($receiver = self['@children']) == nil || $receiver == undefined ? function () {return smalltalk.send(self, "_initializeChildren", []);}() : $receiver;
    return self;
},
args: [],
source: "children\x0a\x0a\x09^ children ifNil:[self initializeChildren]",
messageSends: ["ifNil:", "initializeChildren"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_childrenAt_",
smalltalk.method({
selector: "childrenAt:",
category: 'accessing',
fn: function (aKey) {
    var self = this;
    return smalltalk.send(self, "_at_", [aKey]);
    return self;
},
args: ["aKey"],
source: "childrenAt: aKey\x0a\x09\x22Answers the (sub)widget that the receiver has stored at aKey\x22\x0a\x09^ self at: aKey",
messageSends: ["at:"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_childrenAt_put_",
smalltalk.method({
selector: "childrenAt:put:",
category: 'accessing',
fn: function (aKey, aPresenter) {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_children", []), "_at_put_", [aKey, aPresenter]);
    ($receiver = smalltalk.send(aPresenter, "_isPresenter", [])).klass === smalltalk.Boolean ? $receiver ? function () {return smalltalk.send(aPresenter, "_parent_", [self]);}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return smalltalk.send(aPresenter, "_parent_", [self]);}]);
    return aPresenter;
    return self;
},
args: ["aKey", "aPresenter"],
source: "childrenAt: aKey put: aPresenter \x0a\x09\x22Answers the presenter recently added to the receiver\x0a\x09(with this receiver as its parent).\x22\x0a\x09\x0a\x09self children at: aKey put: aPresenter.\x0a\x09aPresenter isPresenter ifTrue: [aPresenter parent: self].\x0a\x09^ aPresenter ",
messageSends: ["at:put:", "children", "ifTrue:", "isPresenter", "parent:"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_componentsAt_put_",
smalltalk.method({
selector: "componentsAt:put:",
category: 'accessing',
fn: function (aKey, aPresenter) {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_children", []), "_at_put_", [aKey, aPresenter]);
    ($receiver = smalltalk.send(aPresenter, "_isPresenter", [])).klass === smalltalk.Boolean ? $receiver ? function () {return smalltalk.send(aPresenter, "_parent_", [self]);}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return smalltalk.send(aPresenter, "_parent_", [self]);}]);
    return aPresenter;
    return self;
},
args: ["aKey", "aPresenter"],
source: "componentsAt: aKey put: aPresenter \x0a\x09\x22Answers the presenter recently added to the receiver\x0a\x09and with this receiver as its parent.\x22\x0a\x09\x0a\x09self children at: aKey put: aPresenter.\x0a\x09aPresenter isPresenter ifTrue: [aPresenter parent: self].\x0a\x09^ aPresenter ",
messageSends: ["at:put:", "children", "ifTrue:", "isPresenter", "parent:"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_doesNotUnderstand_",
smalltalk.method({
selector: "doesNotUnderstand:",
category: 'actions',
fn: function (aMessage) {
    var self = this;
    return ($receiver = smalltalk.send(self, "_hasPresenterAt_", [smalltalk.send(smalltalk.send(aMessage, "_selector", []), "_asSymbol", [])])).klass === smalltalk.Boolean ? $receiver ? function () {return smalltalk.send(self, "_at_", [smalltalk.send(smalltalk.send(aMessage, "_selector", []), "_asSymbol", [])]);}() : function () {return smalltalk.send(self, "_doesNotUnderstand_", [aMessage], smalltalk.Presenter.superclass || nil);}() : smalltalk.send($receiver, "_ifTrue_ifFalse_", [function () {return smalltalk.send(self, "_at_", [smalltalk.send(smalltalk.send(aMessage, "_selector", []), "_asSymbol", [])]);}, function () {return smalltalk.send(self, "_doesNotUnderstand_", [aMessage], smalltalk.Presenter.superclass || nil);}]);
    return self;
},
args: ["aMessage"],
source: "doesNotUnderstand: aMessage\x0a\x0a\x09^ (self hasPresenterAt: aMessage selector asSymbol)\x0a\x09\x09ifTrue:[self at: aMessage selector asSymbol]\x0a\x09\x09ifFalse:[super doesNotUnderstand: aMessage]\x0a",
messageSends: ["ifTrue:ifFalse:", "hasPresenterAt:", "asSymbol", "selector", "at:", "doesNotUnderstand:"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_feedbackText_",
smalltalk.method({
selector: "feedbackText:",
category: 'actions',
fn: function (aString) {
    var self = this;
    smalltalk.send(self, "_neutralFeedbackText_", [aString]);
    return self;
},
args: ["aString"],
source: "feedbackText: aString\x0a\x0a\x09self neutralFeedbackText: aString",
messageSends: ["neutralFeedbackText:"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_hasParent",
smalltalk.method({
selector: "hasParent",
category: 'testing',
fn: function () {
    var self = this;
    return smalltalk.send(self['@parent'], "_notNil", []);
    return self;
},
args: [],
source: "hasParent\x0a\x0a\x09^ parent notNil",
messageSends: ["notNil"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_hasPresenterAt_",
smalltalk.method({
selector: "hasPresenterAt:",
category: 'testing',
fn: function (aKey) {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_children", []), "_includesKey_", [aKey]);
    return self;
},
args: ["aKey"],
source: "hasPresenterAt: aKey\x0a\x0a\x09^ self children includesKey: aKey ",
messageSends: ["includesKey:", "children"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_hide",
smalltalk.method({
selector: "hide",
category: 'actions',
fn: function () {
    var self = this;
    ($receiver = smalltalk.send(self, "_isPainted", [])).klass === smalltalk.Boolean ? $receiver ? function () {return smalltalk.send(smalltalk.send(self['@wrapper'], "_asJQuery", []), "_hide", []);}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return smalltalk.send(smalltalk.send(self['@wrapper'], "_asJQuery", []), "_hide", []);}]);
    smalltalk.send(self, "_onHide", []);
    return self;
},
args: [],
source: "hide\x0a\x0a\x09self isPainted ifTrue:[\x0a\x09\x09wrapper asJQuery hide].\x0a\x09\x0a\x09self onHide.",
messageSends: ["ifTrue:", "isPainted", "hide", "asJQuery", "onHide"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_hideAll",
smalltalk.method({
selector: "hideAll",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(smalltalk.send(self, "_children", []), "_keys", []), "_do_", [function (each) {var child = nil;child = smalltalk.send(self, "_at_ifAbsent_", [each, function () {return nil;}]);return ($receiver = child) != nil && $receiver != undefined ? function () {return smalltalk.send(child, "_hide", []);}() : nil;}]);
    return self;
},
args: [],
source: "hideAll\x0a\x09\x22Hide all the children.\x0a\x09Note: it's done in such a way that if they don't exist they won't evoked.\x22\x0a\x0a\x09self children keys do:[:each| |child|\x0a\x09\x09child := self at: each ifAbsent:[nil].\x0a\x09\x09child ifNotNil:[\x0a\x09\x09\x09child hide]].\x0a",
messageSends: ["do:", "keys", "children", "at:ifAbsent:", "ifNotNil:", "hide"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_ifAbsentAt_put_",
smalltalk.method({
selector: "ifAbsentAt:put:",
category: 'accessing',
fn: function (aKey, aBlock) {
    var self = this;
    ($receiver = smalltalk.send(self, "_hasPresenterAt_", [aKey])).klass === smalltalk.Boolean ? !$receiver ? function () {return smalltalk.send(self, "_at_put_", [aKey, smalltalk.send(aBlock, "_value", [])]);}() : nil : smalltalk.send($receiver, "_ifFalse_", [function () {return smalltalk.send(self, "_at_put_", [aKey, smalltalk.send(aBlock, "_value", [])]);}]);
    return smalltalk.send(self, "_at_", [aKey]);
    return self;
},
args: ["aKey", "aBlock"],
source: "ifAbsentAt: aKey put: aBlock\x0a\x09\x22Conditionally (to its abscence) adds the presenter returned by aBlock\x0a\x09as (sub)widget of this widget.\x0a\x09Conveniently, answers that very presenter\x22\x0a\x09\x0a\x09(self hasPresenterAt: aKey) ifFalse:[\x0a\x09\x09self at: aKey put: aBlock value].\x0a\x0a\x09^ self at: aKey",
messageSends: ["ifFalse:", "hasPresenterAt:", "at:put:", "value", "at:"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_ifAbsentAt_put_andDo_",
smalltalk.method({
selector: "ifAbsentAt:put:andDo:",
category: 'accessing',
fn: function (aKey, aBlock, anotherBlock) {
    var self = this;
    ($receiver = smalltalk.send(self, "_hasPresenterAt_", [aKey])).klass === smalltalk.Boolean ? !$receiver ? function () {smalltalk.send(self, "_at_put_", [aKey, smalltalk.send(aBlock, "_value", [])]);return smalltalk.send(anotherBlock, "_value", []);}() : nil : smalltalk.send($receiver, "_ifFalse_", [function () {smalltalk.send(self, "_at_put_", [aKey, smalltalk.send(aBlock, "_value", [])]);return smalltalk.send(anotherBlock, "_value", []);}]);
    return smalltalk.send(self, "_at_", [aKey]);
    return self;
},
args: ["aKey", "aBlock", "anotherBlock"],
source: "ifAbsentAt: aKey put: aBlock andDo: anotherBlock\x0a\x09\x22Conditionally (to its abscence) adds the presenter answered by aBlock\x0a\x09as (sub)widget of this widget and then evaluates anotherBlock.\x0a\x09Note: the typical applicability of anotherBlock is the configuration \x0a\x09of the observation of sub widgets' announcements\x22\x0a\x09\x0a\x09(self hasPresenterAt: aKey) ifFalse:[\x0a\x09\x09self at: aKey put: aBlock value.\x0a\x09\x09anotherBlock value] .\x0a\x0a\x09^ self at: aKey",
messageSends: ["ifFalse:", "hasPresenterAt:", "at:put:", "value", "at:"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_initialize",
smalltalk.method({
selector: "initialize",
category: 'initialization',
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.Presenter.superclass || nil);
    painted = false;
    return self;
},
args: [],
source: "initialize\x0a\x0a\x09super initialize.\x0a\x0a\x09painted := false",
messageSends: ["initialize"],
referencedClasses: []
}),
smalltalk.Presenter);

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
smalltalk.Presenter);

smalltalk.addMethod(
"_initializeChildren",
smalltalk.method({
selector: "initializeChildren",
category: 'initialization',
fn: function () {
    var self = this;
    return self['@children'] = smalltalk.send(smalltalk.Dictionary || Dictionary, "_new", []);
    return self;
},
args: [],
source: "initializeChildren\x0a\x0a\x09^ children := Dictionary new",
messageSends: ["new"],
referencedClasses: ["Dictionary"]
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_initializeVisible",
smalltalk.method({
selector: "initializeVisible",
category: 'initialization',
fn: function () {
    var self = this;
    return visible = false;
    return self;
},
args: [],
source: "initializeVisible\x0a\x09\x22We set the visible default in false so the application's\x0a\x09behavior can decide elsewere how (and if) this presenter should appear.\x22\x0a\x0a\x09^ visible := false",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_isPainted",
smalltalk.method({
selector: "isPainted",
category: 'testing',
fn: function () {
    var self = this;
    return typeof painted == "undefined" ? nil : painted;
    return self;
},
args: [],
source: "isPainted\x0a\x0a\x09^ painted",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_isPresenter",
smalltalk.method({
selector: "isPresenter",
category: 'testing',
fn: function () {
    var self = this;
    return true;
    return self;
},
args: [],
source: "isPresenter\x0a\x0a\x09^ true",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_model",
smalltalk.method({
selector: "model",
category: 'accessing',
fn: function () {
    var self = this;
    return self['@model'];
    return self;
},
args: [],
source: "model\x0a\x09\x0a\x09^ model",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_model_",
smalltalk.method({
selector: "model:",
category: 'initialization',
fn: function (aModel) {
    var self = this;
    smalltalk.send(self, "_setModel_", [aModel]);
    smalltalk.send(self, "_onModelUpdated", []);
    return self;
},
args: ["aModel"],
source: "model: aModel\x0a\x09\x22Set the model of this widget\x22\x0a\x0a\x09self setModel: aModel.\x0a\x09\x0a\x09self onModelUpdated",
messageSends: ["setModel:", "onModelUpdated"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_negativeFeedbackText_",
smalltalk.method({
selector: "negativeFeedbackText:",
category: 'actions',
fn: function (aString) {
    var self = this;
    smalltalk.send(self, "_announce_", [smalltalk.send(smalltalk.send(smalltalk.FeedbackRequest || FeedbackRequest, "_for_", [aString]), "_beNegative", [])]);
    return self;
},
args: ["aString"],
source: "negativeFeedbackText: aString\x0a\x0a\x09self announce: (FeedbackRequest for: aString) beNegative",
messageSends: ["announce:", "beNegative", "for:"],
referencedClasses: ["FeedbackRequest"]
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_neutralFeedbackText_",
smalltalk.method({
selector: "neutralFeedbackText:",
category: 'actions',
fn: function (aString) {
    var self = this;
    smalltalk.send(self, "_announce_", [smalltalk.send(smalltalk.send(smalltalk.FeedbackRequest || FeedbackRequest, "_for_", [aString]), "_beNeutral", [])]);
    return self;
},
args: ["aString"],
source: "neutralFeedbackText: aString\x0a\x0a\x09self announce: (FeedbackRequest for: aString) beNeutral",
messageSends: ["announce:", "beNeutral", "for:"],
referencedClasses: ["FeedbackRequest"]
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_newCanvas",
smalltalk.method({
selector: "newCanvas",
category: 'actions',
fn: function () {
    var self = this;
    return smalltalk.send(self, "_newCanvasOn_", [smalltalk.send(self, "_asJQuery", [])]);
    return self;
},
args: [],
source: "newCanvas\x0a\x09\x22Answers a new canvas based on the JQuery element wrapping this presenter.\x22\x0a\x0a\x09^ self newCanvasOn: self asJQuery",
messageSends: ["newCanvasOn:", "asJQuery"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_newCanvasOn_",
smalltalk.method({
selector: "newCanvasOn:",
category: 'actions',
fn: function (aJQuery) {
    var self = this;
    return smalltalk.send(smalltalk.HTMLCanvas || HTMLCanvas, "_onJQuery_", [aJQuery]);
    return self;
},
args: ["aJQuery"],
source: "newCanvasOn: aJQuery\x0a\x09\x22Answers a new canvas based on the given aJQuery element.\x22\x0a\x0a\x09^ HTMLCanvas onJQuery: aJQuery",
messageSends: ["onJQuery:"],
referencedClasses: ["HTMLCanvas"]
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_on_do_",
smalltalk.method({
selector: "on:do:",
category: 'accessing',
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
smalltalk.Presenter);

smalltalk.addMethod(
"_onAboutToOpen",
smalltalk.method({
selector: "onAboutToOpen",
category: 'reactions',
fn: function () {
    var self = this;
    return self;
},
args: [],
source: "onAboutToOpen\x0a\x0a\x09\x22no-op\x22",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_onAboutToUpdateModel_",
smalltalk.method({
selector: "onAboutToUpdateModel:",
category: 'reactions',
fn: function (aModel) {
    var self = this;
    return self;
},
args: ["aModel"],
source: "onAboutToUpdateModel: aModel\x0a\x09\x22The model is about to be updated.\x0a\x09This is your chance to react about it.\x22\x0a\x09\x0a\x09\x22no-op\x22",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_onAfterOpen",
smalltalk.method({
selector: "onAfterOpen",
category: 'reactions',
fn: function () {
    var self = this;
    return self;
},
args: [],
source: "onAfterOpen\x0a\x0a\x09\x22no-op\x22",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_onAfterPainted",
smalltalk.method({
selector: "onAfterPainted",
category: 'reactions',
fn: function () {
    var self = this;
    return self;
},
args: [],
source: "onAfterPainted\x0a\x0a\x09\x22no-op\x22",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_onHide",
smalltalk.method({
selector: "onHide",
category: 'reactions',
fn: function () {
    var self = this;
    return self;
},
args: [],
source: "onHide\x0a\x0a\x09\x22no-op\x22",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_onModelUpdated",
smalltalk.method({
selector: "onModelUpdated",
category: 'reactions',
fn: function () {
    var self = this;
    return self;
},
args: [],
source: "onModelUpdated\x0a\x09\x22The model has been updated.\x0a\x09This is your chance to react about it.\x22\x0a\x09\x0a\x09\x22no-op\x22",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_onShow",
smalltalk.method({
selector: "onShow",
category: 'reactions',
fn: function () {
    var self = this;
    return self;
},
args: [],
source: "onShow\x0a\x0a\x09\x22no-op\x22",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_open",
smalltalk.method({
selector: "open",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_onAboutToOpen", []);
    smalltalk.send(self, "_paintOnJQuery_", [smalltalk.send("body", "_asJQuery", [])]);
    smalltalk.send(self, "_onAfterOpen", []);
    return self;
},
args: [],
source: "open\x0a\x09\x0a\x09self onAboutToOpen.\x0a\x0a\x09self paintOnJQuery: 'body' asJQuery.\x0a\x0a\x09self onAfterOpen",
messageSends: ["onAboutToOpen", "paintOnJQuery:", "asJQuery", "onAfterOpen"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_paint_",
smalltalk.method({
selector: "paint:",
category: 'painting',
fn: function (aPresenter) {
    var self = this;
    smalltalk.send(function () {return smalltalk.send(aPresenter, "_paintOnJQuery_", [smalltalk.send(self, "_asJQuery", [])]);}, "_on_do_", [smalltalk.Error || Error, function (x) {return smalltalk.send(self, "_halt", []);}]);
    return self;
},
args: ["aPresenter"],
source: "paint: aPresenter\x0a\x09\x22Paints the (sub)preenter aPresenter in this (parent)presenter.\x22\x0a\x0a\x09[aPresenter paintOnJQuery: self asJQuery] on: Error do:[:x| self halt]",
messageSends: ["on:do:", "paintOnJQuery:", "asJQuery", "halt"],
referencedClasses: ["Error"]
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_paint_on_",
smalltalk.method({
selector: "paint:on:",
category: 'painting',
fn: function (aPresenter, html) {
    var self = this;
    smalltalk.send(aPresenter, "_renderOn_", [html]);
    return self;
},
args: ["aPresenter", "html"],
source: "paint: aPresenter on: html\x0a\x09\x22Paints the (sub)preenter aPresenter in this (parent)presenter using\x0a\x09the given html as canvas.\x22\x0a\x0a\x09aPresenter renderOn: html",
messageSends: ["renderOn:"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_paintOn_",
smalltalk.method({
selector: "paintOn:",
category: 'painting',
fn: function (html) {
    var self = this;
    return self;
    return self;
},
args: ["html"],
source: "paintOn: html\x0a\x09\x22Paints the content (the wrapped content, like wrapper NOT included)\x0a\x09of this presenter. Sub classes should override as appropiate.\x22\x0a\x09\x0a\x09\x22default is paint nothing\x22\x0a\x09^ self",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_paintOnJQuery_",
smalltalk.method({
selector: "paintOnJQuery:",
category: 'painting',
fn: function (aJQuery) {
    var self = this;
    smalltalk.send(self, "_appendToJQuery_", [aJQuery]);
    return self;
},
args: ["aJQuery"],
source: "paintOnJQuery: aJQuery\x0a\x09\x22Appends what's being paint by this presenter to aJQuery.\x22\x0a\x0a\x09self appendToJQuery: aJQuery",
messageSends: ["appendToJQuery:"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_parent",
smalltalk.method({
selector: "parent",
category: 'accessing',
fn: function () {
    var self = this;
    return self['@parent'];
    return self;
},
args: [],
source: "parent\x0a\x0a\x09^ parent",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_parent_",
smalltalk.method({
selector: "parent:",
category: 'accessing',
fn: function (aPresenter) {
    var self = this;
    self['@parent'] = aPresenter;
    return self;
},
args: ["aPresenter"],
source: "parent: aPresenter\x0a\x09parent := aPresenter",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_positiveFeedbackText_",
smalltalk.method({
selector: "positiveFeedbackText:",
category: 'actions',
fn: function (aString) {
    var self = this;
    smalltalk.send(self, "_announce_", [smalltalk.send(smalltalk.send(smalltalk.FeedbackRequest || FeedbackRequest, "_for_", [aString]), "_bePositive", [])]);
    return self;
},
args: ["aString"],
source: "positiveFeedbackText: aString\x0a\x0a\x09self announce: (FeedbackRequest for: aString) bePositive",
messageSends: ["announce:", "bePositive", "for:"],
referencedClasses: ["FeedbackRequest"]
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_remove",
smalltalk.method({
selector: "remove",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_removeAll", []);
    ($receiver = self['@parent']) == nil || $receiver == undefined ? function () {return smalltalk.send(smalltalk.send(self, "_asJQuery", []), "_remove", []);}() : function () {return smalltalk.send(self['@parent'], "_remove_", [self]);}();
    return self;
},
args: [],
source: "remove\x0a\x0a\x09self removeAll.\x0a\x0a\x09parent \x0a\x09\x09ifNil:[self asJQuery remove.]\x0a\x09\x09ifNotNil:[parent remove: self].\x0a\x0a",
messageSends: ["removeAll", "ifNil:ifNotNil:", "remove", "asJQuery", "remove:"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_remove_",
smalltalk.method({
selector: "remove:",
category: 'actions',
fn: function (aPresenter) {
    var self = this;
    var target = nil;
    smalltalk.send(smalltalk.send(self, "_children", []), "_keysAndValuesDo_", [function (k, v) {return ($receiver = smalltalk.send(v, "__eq_eq", [aPresenter])).klass === smalltalk.Boolean ? $receiver ? function () {return target = k;}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return target = k;}]);}]);
    smalltalk.send(smalltalk.send(aPresenter, "_asJQuery", []), "_remove", []);
    smalltalk.send(smalltalk.send(self, "_children", []), "_removeKey_ifAbsent_", [target, function () {return nil;}]);
    return self;
},
args: ["aPresenter"],
source: "remove: aPresenter\x0a\x09\x22Removes aPresenter from this presenter and from the DOM.\x0a\x09Note: JQuery takes care of terminating event observations.\x22\x0a\x09| target |\x0a\x0a\x09self children keysAndValuesDo:[:k :v|\x09\x0a\x09\x09(v == aPresenter) ifTrue:[\x0a\x09\x09\x09target := k]].\x0a\x0a\x09aPresenter asJQuery remove.\x0a\x09self children removeKey: target ifAbsent:[nil].\x0a",
messageSends: ["keysAndValuesDo:", "children", "ifTrue:", "==", "remove", "asJQuery", "removeKey:ifAbsent:"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_removeAll",
smalltalk.method({
selector: "removeAll",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_subPresenters", []), "_do_", [function (e) {return smalltalk.send(e, "_remove", []);}]);
    return self;
},
args: [],
source: "removeAll\x0a\x0a\x09self subPresenters do:[:e| \x0a\x09\x09e remove]",
messageSends: ["do:", "subPresenters", "remove"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_removeAt_",
smalltalk.method({
selector: "removeAt:",
category: 'actions',
fn: function (aKey) {
    var self = this;
    var target = nil;
    smalltalk.send(smalltalk.send(self, "_children", []), "_keysAndValuesDo_", [function (k, v) {return ($receiver = smalltalk.send(k, "__eq_eq", [aKey])).klass === smalltalk.Boolean ? $receiver ? function () {return target = v;}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return target = v;}]);}]);
    ($receiver = target) != nil && $receiver != undefined ? function () {return smalltalk.send(target, "_remove", []);}() : nil;
    return self;
},
args: ["aKey"],
source: "removeAt: aKey\x0a\x09\x22Removes the (sub)presenter found at aKey\x0a\x09does nothing otherwise.\x22\x0a\x09| target |\x0a\x0a\x09self children keysAndValuesDo:[:k :v|\x09\x0a\x09\x09(k == aKey) ifTrue:[\x0a\x09\x09\x09target := v]].\x0a\x0a\x09target ifNotNil:[target remove]\x0a",
messageSends: ["keysAndValuesDo:", "children", "ifTrue:", "==", "ifNotNil:", "remove"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_renderOn_",
smalltalk.method({
selector: "renderOn:",
category: 'rendering',
fn: function (html) {
    var self = this;
    self['@wrapper'] = smalltalk.send(html, "_div", []);
    (function ($rec) {smalltalk.send($rec, "_class_", [smalltalk.send(smalltalk.send(self, "_class", []), "_name", [])]);return smalltalk.send($rec, "_with_", [function () {return smalltalk.send(self, "_paintOn_", [html]);}]);}(self['@wrapper']));
    smalltalk.send(self, "_onAfterPainted", []);
    painted = true;
    return self;
},
args: ["html"],
source: "renderOn: html\x0a\x09\x22Start to paint this presenter.\x0a\x09Note: presenters are always wrapped in a div using as CSS class\x0a\x09it's class name.\x22\x0a\x0a\x09wrapper := html div.\x0a\x09\x0a\x09wrapper\x0a\x09\x09class: self class name;\x0a\x09\x09with:[self paintOn: html].\x0a\x09self onAfterPainted.\x0a\x09painted := true.",
messageSends: ["div", "class:", "name", "class", "with:", "paintOn:", "onAfterPainted"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_setId_",
smalltalk.method({
selector: "setId:",
category: 'actions',
fn: function (aString) {
    var self = this;
    smalltalk.send(self['@wrapper'], "_id_", [aString]);
    return self;
},
args: ["aString"],
source: "setId: aString\x0a\x0a\x09wrapper id: aString",
messageSends: ["id:"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_setModel_",
smalltalk.method({
selector: "setModel:",
category: 'actions',
fn: function (aModel) {
    var self = this;
    smalltalk.send(self, "_onAboutToUpdateModel_", [aModel]);
    self['@model'] = aModel;
    return self;
},
args: ["aModel"],
source: "setModel: aModel\x0a\x09\x22This widget is going updating its model to aModel\x22\x0a\x0a\x09self onAboutToUpdateModel: aModel.\x0a\x0a\x09model := aModel \x0a",
messageSends: ["onAboutToUpdateModel:"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_show",
smalltalk.method({
selector: "show",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(self['@wrapper'], "_asJQuery", []), "_show", []);
    smalltalk.send(self, "_onShow", []);
    return self;
},
args: [],
source: "show\x0a\x0a\x09wrapper asJQuery show.\x0a\x09self onShow",
messageSends: ["show", "asJQuery", "onShow"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_subPresenters",
smalltalk.method({
selector: "subPresenters",
category: 'accessing',
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_children", []), "_values", []);
    return self;
},
args: [],
source: "subPresenters\x0a\x0a\x09^ self children values",
messageSends: ["values", "children"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_tr",
smalltalk.method({
selector: "tr",
category: 'accessing',
fn: function () {
    var self = this;
    return typeof tr == "undefined" ? nil : tr;
    return self;
},
args: [],
source: "tr\x0a\x0a\x09^ tr",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_visible",
smalltalk.method({
selector: "visible",
category: 'accessing',
fn: function () {
    var self = this;
    return ($receiver = typeof visible == "undefined" ? nil : visible) == nil ||
        $receiver == undefined ? function () {return smalltalk.send(self, "_initializeVisible", []);}() : $receiver;
    return self;
},
args: [],
source: "visible\x0a\x09\x22Answers true if this presenter should be visible \x0a\x09as soon as it get's rendered (or not, in which case,\x0a\x09 it will be rendered while staying hidden).\x22\x0a\x09^ visible ifNil:[self initializeVisible]",
messageSends: ["ifNil:", "initializeVisible"],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_visible_",
smalltalk.method({
selector: "visible:",
category: 'accessing',
fn: function (aBoolean) {
    var self = this;
    visible = aBoolean;
    return self;
},
args: ["aBoolean"],
source: "visible: aBoolean\x0a\x09\x22Sets if this presenter should be visible \x0a\x09as soon as it get's rendered (or not, in which case,\x0a\x09 it will be rendered while staying hidden).\x22\x0a\x09visible := aBoolean\x0a",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_wrapper",
smalltalk.method({
selector: "wrapper",
category: 'accessing',
fn: function () {
    var self = this;
    return self['@wrapper'];
    return self;
},
args: [],
source: "wrapper\x0a\x0a\x09^ wrapper",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter);

smalltalk.addMethod(
"_wrapper_",
smalltalk.method({
selector: "wrapper:",
category: 'accessing',
fn: function (aTagBrush) {
    var self = this;
    self['@wrapper'] = aTagBrush;
    return self;
},
args: ["aTagBrush"],
source: "wrapper: aTagBrush\x0a\x0a\x09wrapper := aTagBrush",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter);


smalltalk.addMethod(
"_defaultModel",
smalltalk.method({
selector: "defaultModel",
category: 'accessing',
fn: function () {
    var self = this;
    return nil;
    return self;
},
args: [],
source: "defaultModel\x0a\x0a\x09^ nil",
messageSends: [],
referencedClasses: []
}),
smalltalk.Presenter.klass);

smalltalk.addMethod(
"_for_",
smalltalk.method({
selector: "for:",
category: 'actions',
fn: function (aModel) {
    var self = this;
    return function ($rec) {smalltalk.send($rec, "_model_", [aModel]);return smalltalk.send($rec, "_yourself", []);}(smalltalk.send(self, "_new", []));
    return self;
},
args: ["aModel"],
source: "for: aModel\x0a\x0a\x09^ self new\x0a\x09\x09model: aModel;\x0a\x09\x09yourself",
messageSends: ["model:", "yourself", "new"],
referencedClasses: []
}),
smalltalk.Presenter.klass);

smalltalk.addMethod(
"_open",
smalltalk.method({
selector: "open",
category: 'actions',
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_for_", [smalltalk.send(self, "_defaultModel", [])]), "_open", []);
    return self;
},
args: [],
source: "open\x0a\x0a\x09^ (self for: self defaultModel) open",
messageSends: ["open", "for:", "defaultModel"],
referencedClasses: []
}),
smalltalk.Presenter.klass);


smalltalk.addClass('Application', smalltalk.Presenter, ['feedback'], 'Flow-Presenters');
smalltalk.addMethod(
"_feedback",
smalltalk.method({
selector: "feedback",
category: 'accessing',
fn: function () {
    var self = this;
    return ($receiver = self['@feedback']) == nil || $receiver == undefined ? function () {return smalltalk.send(self, "_initializeFeedback", []);}() : $receiver;
    return self;
},
args: [],
source: "feedback\x0a\x09^ feedback ifNil:[self initializeFeedback]",
messageSends: ["ifNil:", "initializeFeedback"],
referencedClasses: []
}),
smalltalk.Application);

smalltalk.addMethod(
"_feedbackText_",
smalltalk.method({
selector: "feedbackText:",
category: 'accessing',
fn: function (aString) {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_feedback", []), "_text_", [aString]);
    return self;
},
args: ["aString"],
source: "feedbackText: aString\x0a\x0a\x09self feedback text: aString",
messageSends: ["text:", "feedback"],
referencedClasses: []
}),
smalltalk.Application);

smalltalk.addMethod(
"_initialize",
smalltalk.method({
selector: "initialize",
category: 'initialization',
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.Application.superclass || nil);
    smalltalk.send(smalltalk.send(typeof window == "undefined" ? nil : window, "_jQuery_", [typeof window == "undefined" ? nil : window]), "_bind_do_", ["hashchange", function () {return smalltalk.send(self, "_onHash", []);}]);
    return self;
},
args: [],
source: "initialize\x0a\x0a\x09super initialize.\x0a\x0a\x09(window jQuery: window) bind: 'hashchange' do: [self onHash].",
messageSends: ["initialize", "bind:do:", "jQuery:", "onHash"],
referencedClasses: []
}),
smalltalk.Application);

smalltalk.addMethod(
"_initializeFeedback",
smalltalk.method({
selector: "initializeFeedback",
category: 'initialization',
fn: function () {
    var self = this;
    return self['@feedback'] = smalltalk.send(self, "_makeFeedback", []);
    return self;
},
args: [],
source: "initializeFeedback\x0a\x09^ feedback := self makeFeedback",
messageSends: ["makeFeedback"],
referencedClasses: []
}),
smalltalk.Application);

smalltalk.addMethod(
"_makeFeedback",
smalltalk.method({
selector: "makeFeedback",
category: 'actions',
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.Feedback || Feedback, "_new", []);
    return self;
},
args: [],
source: "makeFeedback\x0a\x0a\x09^ Feedback new",
messageSends: ["new"],
referencedClasses: ["Feedback"]
}),
smalltalk.Application);

smalltalk.addMethod(
"_negativeFeedbackText_",
smalltalk.method({
selector: "negativeFeedbackText:",
category: 'accessing',
fn: function (aString) {
    var self = this;
    (function ($rec) {smalltalk.send($rec, "_beNegative", []);return smalltalk.send($rec, "_text_", [aString]);}(smalltalk.send(self, "_feedback", [])));
    return self;
},
args: ["aString"],
source: "negativeFeedbackText: aString\x0a\x0a\x09self feedback beNegative; text: aString",
messageSends: ["beNegative", "text:", "feedback"],
referencedClasses: []
}),
smalltalk.Application);

smalltalk.addMethod(
"_neutralFeedbackText_",
smalltalk.method({
selector: "neutralFeedbackText:",
category: 'accessing',
fn: function (aString) {
    var self = this;
    (function ($rec) {smalltalk.send($rec, "_beNeutral", []);return smalltalk.send($rec, "_text_", [aString]);}(smalltalk.send(self, "_feedback", [])));
    return self;
},
args: ["aString"],
source: "neutralFeedbackText: aString\x0a\x0a\x09self feedback beNeutral; text: aString",
messageSends: ["beNeutral", "text:", "feedback"],
referencedClasses: []
}),
smalltalk.Application);

smalltalk.addMethod(
"_onAfterOpen",
smalltalk.method({
selector: "onAfterOpen",
category: 'reactions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_onAfterOpen", [], smalltalk.Application.superclass || nil);
    smalltalk.send(typeof window == "undefined" ? nil : window, "_at_put_", ["app", self]);
    return self;
},
args: [],
source: "onAfterOpen\x0a\x0a\x09super onAfterOpen.\x0a\x0a\x09window at: 'app' put: self\x0a",
messageSends: ["onAfterOpen", "at:put:"],
referencedClasses: []
}),
smalltalk.Application);

smalltalk.addMethod(
"_onFeedback_",
smalltalk.method({
selector: "onFeedback:",
category: 'reactions',
fn: function (anAnnouncement) {
    var self = this;
    var polarity = nil;
    polarity = smalltalk.send(anAnnouncement, "_polarity", []);
    smalltalk.send(self, "_perform_withArguments_", [smalltalk.send(polarity, "__comma", ["FeedbackText:"]), smalltalk.send(smalltalk.Array || Array, "_with_", [smalltalk.send(anAnnouncement, "_subject", [])])]);
    return self;
},
args: ["anAnnouncement"],
source: "onFeedback: anAnnouncement\x0a\x0a\x09| polarity |\x0a\x0a\x09polarity := anAnnouncement polarity.\x0a\x09self perform: (polarity, 'FeedbackText:') withArguments: (Array with: anAnnouncement subject)\x0a\x0a\x09",
messageSends: ["polarity", "perform:withArguments:", ",", "with:", "subject"],
referencedClasses: ["Array"]
}),
smalltalk.Application);

smalltalk.addMethod(
"_onHash",
smalltalk.method({
selector: "onHash",
category: 'reactions',
fn: function () {
    var self = this;
    return self;
},
args: [],
source: "onHash\x0a\x09\x22The hash in the application URL has changed.\x22\x0a\x0a\x09\x22no-op\x22",
messageSends: [],
referencedClasses: []
}),
smalltalk.Application);

smalltalk.addMethod(
"_positiveFeedbackText_",
smalltalk.method({
selector: "positiveFeedbackText:",
category: 'accessing',
fn: function (aString) {
    var self = this;
    (function ($rec) {smalltalk.send($rec, "_bePositive", []);return smalltalk.send($rec, "_text_", [aString]);}(smalltalk.send(self, "_feedback", [])));
    return self;
},
args: ["aString"],
source: "positiveFeedbackText: aString\x0a\x0a\x09self feedback bePositive; text: aString",
messageSends: ["bePositive", "text:", "feedback"],
referencedClasses: []
}),
smalltalk.Application);


smalltalk.Application.klass.iVarNames = ['login'];
smalltalk.addMethod(
"_login",
smalltalk.method({
selector: "login",
category: 'accessing',
fn: function () {
    var self = this;
    return self['@login'];
    return self;
},
args: [],
source: "login\x0a\x0a\x09^ login",
messageSends: [],
referencedClasses: []
}),
smalltalk.Application.klass);

smalltalk.addMethod(
"_login_",
smalltalk.method({
selector: "login:",
category: 'accessing',
fn: function (aLogin) {
    var self = this;
    self['@login'] = aLogin;
    return self;
},
args: ["aLogin"],
source: "login: aLogin\x0a\x0a\x09login := aLogin",
messageSends: [],
referencedClasses: []
}),
smalltalk.Application.klass);


smalltalk.addClass('Feedback', smalltalk.Presenter, ['content', 'text'], 'Flow-Presenters');
smalltalk.addMethod(
"_beNegative",
smalltalk.method({
selector: "beNegative",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_removePolarity", []);
    smalltalk.send(smalltalk.send("#feedbackWrapper", "_asJQuery", []), "_addClass_", ["negativeFeedback"]);
    return self;
},
args: [],
source: "beNegative\x0a\x0a\x09self removePolarity.\x0a\x09'#feedbackWrapper' asJQuery addClass: 'negativeFeedback'",
messageSends: ["removePolarity", "addClass:", "asJQuery"],
referencedClasses: []
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_beNeutral",
smalltalk.method({
selector: "beNeutral",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_removePolarity", []);
    smalltalk.send(smalltalk.send("#feedbackWrapper", "_asJQuery", []), "_addClass_", ["neutralFeedback"]);
    return self;
},
args: [],
source: "beNeutral\x0a\x0a\x09self removePolarity.\x0a\x09'#feedbackWrapper' asJQuery addClass: 'neutralFeedback'",
messageSends: ["removePolarity", "addClass:", "asJQuery"],
referencedClasses: []
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_bePositive",
smalltalk.method({
selector: "bePositive",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_removePolarity", []);
    smalltalk.send(smalltalk.send("#feedbackWrapper", "_asJQuery", []), "_addClass_", ["positiveFeedback"]);
    return self;
},
args: [],
source: "bePositive\x0a\x0a\x09self removePolarity.\x0a\x09'#feedbackWrapper' asJQuery addClass: 'positiveFeedback'",
messageSends: ["removePolarity", "addClass:", "asJQuery"],
referencedClasses: []
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_initialize",
smalltalk.method({
selector: "initialize",
category: 'initialization',
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.Feedback.superclass || nil);
    self['@text'] = smalltalk.send(smalltalk.String || String, "_new", []);
    return self;
},
args: [],
source: "initialize\x0a\x0a\x09super initialize.\x0a\x0a\x09text := String new.\x0a",
messageSends: ["initialize", "new"],
referencedClasses: ["String"]
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_onClose",
smalltalk.method({
selector: "onClose",
category: 'reactions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_hide", []);
    return self;
},
args: [],
source: "onClose\x0a\x0a\x09self hide",
messageSends: ["hide"],
referencedClasses: []
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_paintButtonOn_",
smalltalk.method({
selector: "paintButtonOn:",
category: 'paint',
fn: function (html) {
    var self = this;
    (function ($rec) {smalltalk.send($rec, "_id_", ["feedbackClose"]);smalltalk.send($rec, "_with_", ["Close"]);return smalltalk.send($rec, "_onClick_", [function () {return smalltalk.send(self, "_onClose", []);}]);}(smalltalk.send(html, "_button", [])));
    return self;
},
args: ["html"],
source: "paintButtonOn: html\x0a\x0a\x09html button\x0a\x09\x09id: 'feedbackClose';\x0a\x09\x09with: 'Close';\x0a\x09\x09onClick:[self onClose]",
messageSends: ["id:", "with:", "onClick:", "onClose", "button"],
referencedClasses: []
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_paintContentOn_",
smalltalk.method({
selector: "paintContentOn:",
category: 'paint',
fn: function (html) {
    var self = this;
    self['@content'] = function ($rec) {smalltalk.send($rec, "_id_", ["feedbackContentWrapper"]);return smalltalk.send($rec, "_yourself", []);}(smalltalk.send(html, "_div", []));
    return self;
},
args: ["html"],
source: "paintContentOn: html\x0a\x0a\x09content := html div\x0a\x09\x09\x09\x09id: 'feedbackContentWrapper';\x0a\x09\x09\x09\x09yourself",
messageSends: ["id:", "yourself", "div"],
referencedClasses: []
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_paintOn_",
smalltalk.method({
selector: "paintOn:",
category: 'paint',
fn: function (html) {
    var self = this;
    smalltalk.send(self['@wrapper'], "_id_", ["feedback"]);
    smalltalk.send(smalltalk.send(self['@wrapper'], "_asJQuery", []), "_hide", []);
    (function ($rec) {smalltalk.send($rec, "_id_", ["feedbackWrapper"]);return smalltalk.send($rec, "_with_", [function () {return function ($rec) {smalltalk.send($rec, "_id_", ["feedbackContent"]);return smalltalk.send($rec, "_with_", [function () {smalltalk.send(self, "_paintContentOn_", [html]);return smalltalk.send(self, "_paintButtonOn_", [html]);}]);}(smalltalk.send(html, "_div", []));}]);}(smalltalk.send(html, "_div", [])));
    return self;
},
args: ["html"],
source: "paintOn: html\x0a\x09\x0a\x09wrapper id: 'feedback'.\x0a\x09wrapper asJQuery hide.\x0a\x0a\x09html div\x0a\x09\x09id: 'feedbackWrapper';\x0a\x09\x09with:[\x0a\x09\x09\x09html div\x0a\x09\x09\x09\x09id:'feedbackContent';\x0a\x09\x09\x09\x09with:[\x0a\x09\x09\x09\x09\x09self paintContentOn: html.\x0a\x09\x09\x09\x09\x09self paintButtonOn: html]]\x0a",
messageSends: ["id:", "hide", "asJQuery", "with:", "paintContentOn:", "paintButtonOn:", "div"],
referencedClasses: []
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_refresh",
smalltalk.method({
selector: "refresh",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(self['@content'], "_asJQuery", []), "_html_", [self['@text']]);
    return self;
},
args: [],
source: "refresh\x0a\x0a\x09content asJQuery html: text\x0a",
messageSends: ["html:", "asJQuery"],
referencedClasses: []
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_removePolarity",
smalltalk.method({
selector: "removePolarity",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(["neutralFeedback", "neutralFeedback", "neutralFeedback"], "_do_", [function (cssClass) {return smalltalk.send(smalltalk.send(self['@wrapper'], "_asJQuery", []), "_removeClass_", [cssClass]);}]);
    return self;
},
args: [],
source: "removePolarity\x0a\x0a\x09#('neutralFeedback' 'neutralFeedback' 'neutralFeedback') do:[:cssClass|\x0a\x09\x09wrapper asJQuery removeClass: cssClass]",
messageSends: ["do:", "removeClass:", "asJQuery"],
referencedClasses: []
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_show",
smalltalk.method({
selector: "show",
category: 'actions',
fn: function () {
    var self = this;
    (function ($rec) {smalltalk.send($rec, "_slideDown_", [500]);return smalltalk.send($rec, "_fadeIn_do_", [500, function () {return smalltalk.send(typeof window == "undefined" ? nil : window, "_setTimeout_delay_", [function () {return function ($rec) {smalltalk.send($rec, "_slideUp_", [500]);return smalltalk.send($rec, "_fadeOut_", [500]);}(smalltalk.send(self['@wrapper'], "_asJQuery", []));}, 3000]);}]);}(smalltalk.send(self['@wrapper'], "_asJQuery", [])));
    return self;
},
args: [],
source: "show\x0a\x0a\x09wrapper asJQuery slideDown: 500; fadeIn: 500 do:[\x0a\x09\x09window setTimeout: [wrapper asJQuery slideUp: 500; fadeOut: 500] delay: 3000]\x09",
messageSends: ["slideDown:", "fadeIn:do:", "setTimeout:delay:", "slideUp:", "fadeOut:", "asJQuery"],
referencedClasses: []
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_text",
smalltalk.method({
selector: "text",
category: 'accessing',
fn: function () {
    var self = this;
    return self['@text'];
    return self;
},
args: [],
source: "text\x0a\x0a\x09^ text",
messageSends: [],
referencedClasses: []
}),
smalltalk.Feedback);

smalltalk.addMethod(
"_text_",
smalltalk.method({
selector: "text:",
category: 'actions',
fn: function (aString) {
    var self = this;
    self['@text'] = aString;
    smalltalk.send(self, "_refresh", []);
    smalltalk.send(self, "_show", []);
    return self;
},
args: ["aString"],
source: "text: aString\x0a\x0a\x09text := aString.\x0a\x09self refresh.\x0a\x09self show",
messageSends: ["refresh", "show"],
referencedClasses: []
}),
smalltalk.Feedback);



smalltalk.addClass('ItemsPresenter', smalltalk.Presenter, ['items', 'itemsGetter', 'loader'], 'Flow-Presenters');
smalltalk.ItemsPresenter.comment="This kind of presenter is prepared to deal with a bunch of similar subpresenters.\x0a\x0aYou typically use one of these for presenting lists of things (you should subclass it and specialize it).\x0a\x0aWhat you list in this presenters are the models you set in the #items instVar. Note that #items gets lazily initialized and the list of models is queried when the #itemsGetter block is evaluated.\x0a\x0a"
smalltalk.addMethod(
"_atItemId_",
smalltalk.method({
selector: "atItemId:",
category: 'accessing',
fn: function (anId) {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_items", []), "_detect_", [function (e) {return smalltalk.send(smalltalk.send(e, "_id", []), "__eq", [anId]);}]);
    return self;
},
args: ["anId"],
source: "atItemId: anId\x0a\x0a\x09^ self items detect: [:e| e id = anId]",
messageSends: ["detect:", "items", "=", "id"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_getItemsDo_",
smalltalk.method({
selector: "getItemsDo:",
category: 'actions',
fn: function (aBlock) {
    var self = this;
    return smalltalk.send(self, "_subclassResponsibility", []);
    return self;
},
args: ["aBlock"],
source: "getItemsDo: aBlock\x0a\x09\x22Returns the collection of items that should be presented by this presenter.\x0a\x09Evaluates the onDone reaction when the answer arrives.\x22\x0a\x0a\x09\x22You can use this idea in your concrete ItemsPresenter subclass:\x0a\x0a\x09jQuery ajax: #{ \x0a\x09\x09'url' -> ('api/Blah/find?whatever=thing&you=needToDo').\x0a\x09\x09'type' -> 'GET'.\x0a\x09\x09'success' -> [:x| self onAfterLoad: x do: onDone].\x0a\x09\x09'fail' -> [:x| GettingItemsError signal: 'Cannot get the Blah kind of items: ', x responseText].\x0a\x09\x09'error' -> [:x| GettingItemsError signal: 'Cannot get the Blah kind of items: ', x responseText]}\x22  \x0a\x0a\x09^ self subclassResponsibility",
messageSends: ["subclassResponsibility"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_hasItems",
smalltalk.method({
selector: "hasItems",
category: 'testing',
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.send(self['@items'], "_notNil", []), "_and_", [function () {return smalltalk.send(self['@items'], "_notEmpty", []);}]);
    return self;
},
args: [],
source: "hasItems\x0a\x0a\x09^ items notNil and:[\x0a\x09items notEmpty]",
messageSends: ["and:", "notNil", "notEmpty"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_hideLoader",
smalltalk.method({
selector: "hideLoader",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(smalltalk.send(smalltalk.send(self, "_loader", []), "_asJQuery", []), "_hide", []), "_fadeOut_", [0.5]);
    return self;
},
args: [],
source: "hideLoader\x0a\x0a\x09self loader asJQuery hide fadeOut: 0.5",
messageSends: ["fadeOut:", "hide", "asJQuery", "loader"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_initializeItemsGetter",
smalltalk.method({
selector: "initializeItemsGetter",
category: 'initializing',
fn: function () {
    var self = this;
    return self['@itemsGetter'] = smalltalk.send(self, "_makeItemsGetter", []);
    return self;
},
args: [],
source: "initializeItemsGetter\x0a\x09^ itemsGetter := self makeItemsGetter",
messageSends: ["makeItemsGetter"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_initializeLoader",
smalltalk.method({
selector: "initializeLoader",
category: 'initializing',
fn: function () {
    var self = this;
    return self['@loader'] = smalltalk.send(self, "_makeLoader", []);
    return self;
},
args: [],
source: "initializeLoader\x0a\x0a\x09^ loader := self makeLoader",
messageSends: ["makeLoader"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_isLoaded",
smalltalk.method({
selector: "isLoaded",
category: 'testing',
fn: function () {
    var self = this;
    return smalltalk.send(self['@items'], "_notNil", []);
    return self;
},
args: [],
source: "isLoaded\x0a\x0a\x09^ items notNil",
messageSends: ["notNil"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_itemPresenters",
smalltalk.method({
selector: "itemPresenters",
category: 'accessing',
fn: function () {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_items", []), "_collect_", [function (item) {return smalltalk.send(self, "_presenterFor_", [item]);}]);
    return self;
},
args: [],
source: "itemPresenters\x0a\x0a\x09^ self items collect:[:item|\x0a\x09\x09self presenterFor: item]",
messageSends: ["collect:", "items", "presenterFor:"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_items",
smalltalk.method({
selector: "items",
category: 'accessing',
fn: function () {
    var self = this;
    return self['@items'];
    return self;
},
args: [],
source: "items\x0a\x0a\x09^ items ",
messageSends: [],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_items_",
smalltalk.method({
selector: "items:",
category: 'accessing',
fn: function (someModels) {
    var self = this;
    self['@items'] = someModels;
    return self;
},
args: ["someModels"],
source: "items: someModels\x0a\x0a\x09items := someModels\x0a",
messageSends: [],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_itemsDo_",
smalltalk.method({
selector: "itemsDo:",
category: 'actions',
fn: function (aBlock) {
    var self = this;
    smalltalk.send(self['@items'], "_ifNil_ifNotNil_", [function () {return smalltalk.send(self, "_loadItemsDo_", [aBlock]);}, aBlock]);
    return self;
},
args: ["aBlock"],
source: "itemsDo: aBlock\x0a\x09\x22Performs aBlock either after loading the items or when it already have them.\x22\x0a\x0a\x09items \x0a\x09\x09ifNil: [self loadItemsDo: aBlock]\x0a\x09\x09ifNotNil: aBlock",
messageSends: ["ifNil:ifNotNil:", "loadItemsDo:"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_itemsGetter",
smalltalk.method({
selector: "itemsGetter",
category: 'accessing',
fn: function () {
    var self = this;
    return ($receiver = self['@itemsGetter']) == nil || $receiver == undefined ? function () {return smalltalk.send(self, "_initializeItemsGetter", []);}() : $receiver;
    return self;
},
args: [],
source: "itemsGetter\x0a\x0a\x09^ itemsGetter ifNil:[self initializeItemsGetter]",
messageSends: ["ifNil:", "initializeItemsGetter"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_itemsGetter_",
smalltalk.method({
selector: "itemsGetter:",
category: 'accessing',
fn: function (aBlock) {
    var self = this;
    self['@itemsGetter'] = aBlock;
    return self;
},
args: ["aBlock"],
source: "itemsGetter: aBlock\x0a\x0a\x09itemsGetter := aBlock",
messageSends: [],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_loadAndPaintOn_",
smalltalk.method({
selector: "loadAndPaintOn:",
category: 'painting',
fn: function (html) {
    var self = this;
    smalltalk.send(self, "_loadAndPaintOn_done_", [html, function () {return nil;}]);
    return self;
},
args: ["html"],
source: "loadAndPaintOn: html\x0a\x0a\x09self loadAndPaintOn: html done: [nil]",
messageSends: ["loadAndPaintOn:done:"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_loadAndPaintOn_done_",
smalltalk.method({
selector: "loadAndPaintOn:done:",
category: 'painting',
fn: function (html, aBlock) {
    var self = this;
    smalltalk.send(self, "_onAboutToLoad", []);
    smalltalk.send(self, "_itemsDo_", [function () {smalltalk.send(self, "_paintItemsOn_", [html]);smalltalk.send(self, "_onAfterLoaded", []);return smalltalk.send(aBlock, "_value", []);}]);
    return self;
},
args: ["html", "aBlock"],
source: "loadAndPaintOn: html done: aBlock\x0a\x0a\x09self onAboutToLoad.\x0a\x0a\x09self itemsDo:[\x0a\x09\x09self paintItemsOn: html.\x0a\x09\x09self onAfterLoaded.\x0a\x09\x09aBlock value]",
messageSends: ["onAboutToLoad", "itemsDo:", "paintItemsOn:", "onAfterLoaded", "value"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_loadItemsDo_",
smalltalk.method({
selector: "loadItemsDo:",
category: 'actions',
fn: function (aBlock) {
    var self = this;
    return smalltalk.send(smalltalk.send(self, "_itemsGetter", []), "_value_", [aBlock]);
    return self;
},
args: ["aBlock"],
source: "loadItemsDo: aBlock\x0a\x09\x22Loads the items form the getter block and evaluates passing the reaction aBlock to it.\x22\x0a\x0a\x09^ self itemsGetter value: aBlock\x09",
messageSends: ["value:", "itemsGetter"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_loader",
smalltalk.method({
selector: "loader",
category: 'accessing',
fn: function () {
    var self = this;
    return ($receiver = self['@loader']) == nil || $receiver == undefined ? function () {return smalltalk.send(self, "_initializeLoader", []);}() : $receiver;
    return self;
},
args: [],
source: "loader\x0a\x0a\x09^ loader ifNil:[self initializeLoader]",
messageSends: ["ifNil:", "initializeLoader"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_makeItemsFromJson_",
smalltalk.method({
selector: "makeItemsFromJson:",
category: 'actions',
fn: function (someJson) {
    var self = this;
    smalltalk.send(self, "_subclassResponsibility", []);
    return self;
},
args: ["someJson"],
source: "makeItemsFromJson: someJson\x0a\x09\x22Answers the collection of items reified from someJson.\x22\x0a\x0a\x09self subclassResponsibility\x09",
messageSends: ["subclassResponsibility"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_makeItemsGetter",
smalltalk.method({
selector: "makeItemsGetter",
category: 'actions',
fn: function () {
    var self = this;
    return function (onDone) {return smalltalk.send(self, "_getItemsDo_", [onDone]);};
    return self;
},
args: [],
source: "makeItemsGetter\x0a\x0a\x09^ [:onDone| self getItemsDo: onDone]",
messageSends: ["getItemsDo:"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_makeLoader",
smalltalk.method({
selector: "makeLoader",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_subclassResponsibility", []);
    return self;
},
args: [],
source: "makeLoader\x0a\x09\x22Makes the loader that's shown while waiting to get the results when querying for the items.\x22\x0a\x0a\x09self subclassResponsibility\x0a\x0a\x09\x22 use this as an idea\x0a\x0a\x09^(self newCanvasOn: '#ThisPresenter' asJQuery) img\x0a\x09\x09id: 'ThisPresenterLoader';\x0a\x09\x09src: '../files/YourLibrary/img/loaderBar.gif';\x0a\x09\x09yourself\x22",
messageSends: ["subclassResponsibility"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_makePresenterFor_",
smalltalk.method({
selector: "makePresenterFor:",
category: 'actions',
fn: function (anItem) {
    var self = this;
    var itemPresenter = nil;
    itemPresenter = smalltalk.send(smalltalk.send(self, "_presenterClassFor_", [anItem]), "_new", []);
    smalltalk.send(self, "_onModel_for_", [anItem, itemPresenter]);
    return itemPresenter;
    return self;
},
args: ["anItem"],
source: "makePresenterFor: anItem\x0a\x09\x22Answers a new presenter corresponding to anItem.\x22\x0a\x0a\x09| itemPresenter |\x0a\x0a\x09itemPresenter := (self presenterClassFor: anItem) new.\x0a\x0a\x09self onModel: anItem for: itemPresenter.\x0a\x0a\x09^ itemPresenter",
messageSends: ["new", "presenterClassFor:", "onModel:for:"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_observeItemPresenter_",
smalltalk.method({
selector: "observeItemPresenter:",
category: 'actions',
fn: function (anItemPresenter) {
    var self = this;
    return self;
},
args: ["anItemPresenter"],
source: "observeItemPresenter: anItemPresenter\x0a\x09\x22Sets the observations for anItemPresenter.\x22\x0a\x0a\x09\x22no-op\x22",
messageSends: [],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_onAboutToLoad",
smalltalk.method({
selector: "onAboutToLoad",
category: 'reactions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_showLoader", []);
    return self;
},
args: [],
source: "onAboutToLoad\x0a\x0a\x09self showLoader",
messageSends: ["showLoader"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_onAfterLoad_do_",
smalltalk.method({
selector: "onAfterLoad:do:",
category: 'reactions',
fn: function (someJson, onDone) {
    var self = this;
    self['@items'] = smalltalk.send(self, "_makeItemsFromJson_", [someJson]);
    smalltalk.send(onDone, "_value", []);
    smalltalk.send(self, "_announce_", [smalltalk.send(smalltalk.ItemsLoaded || ItemsLoaded, "_new", [])]);
    return self;
},
args: ["someJson", "onDone"],
source: "onAfterLoad: someJson do: onDone\x0a\x0a\x09items := self makeItemsFromJson: someJson.\x0a\x0a\x09onDone value.\x0a\x0a\x09self announce: ItemsLoaded new.\x0a",
messageSends: ["makeItemsFromJson:", "value", "announce:", "new"],
referencedClasses: ["ItemsLoaded"]
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_onAfterLoaded",
smalltalk.method({
selector: "onAfterLoaded",
category: 'reactions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_hideLoader", []);
    smalltalk.send(self, "_announce_", [smalltalk.send(smalltalk.ItemsLoaded || ItemsLoaded, "_new", [])]);
    return self;
},
args: [],
source: "onAfterLoaded\x0a\x0a\x09self hideLoader.\x0a\x09\x0a\x09self announce: ItemsLoaded new.\x0a",
messageSends: ["hideLoader", "announce:", "new"],
referencedClasses: ["ItemsLoaded"]
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_onModel_for_",
smalltalk.method({
selector: "onModel:for:",
category: 'reactions',
fn: function (anItem, anItemPresenter) {
    var self = this;
    smalltalk.send(anItemPresenter, "_model_", [anItem]);
    return self;
},
args: ["anItem", "anItemPresenter"],
source: "onModel: anItem for: anItemPresenter\x0a\x09\x22The model for anItemPresenter is about to be set.\x22\x0a\x0a\x09anItemPresenter model: anItem\x0a",
messageSends: ["model:"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_paintItemsOn_",
smalltalk.method({
selector: "paintItemsOn:",
category: 'painting',
fn: function (html) {
    var self = this;
    smalltalk.send(self['@items'], "_do_", [function (item) {return smalltalk.send(self, "_paint_", [smalltalk.send(self, "_presenterFor_", [item])]);}]);
    return self;
},
args: ["html"],
source: "paintItemsOn: html\x0a\x0a\x09items do:[:item|\x0a\x09\x09self paint: (self presenterFor: item)].\x0a",
messageSends: ["do:", "paint:", "presenterFor:"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_paintOn_",
smalltalk.method({
selector: "paintOn:",
category: 'painting',
fn: function (html) {
    var self = this;
    smalltalk.send(self, "_loadAndPaintOn_", [html]);
    return self;
},
args: ["html"],
source: "paintOn: html\x0a\x09\x22Paints the presenters for the items.\x0a\x09Note: this will paint the loader bar until it gets the results about querying the items.\x0a\x09Once they arrive, they get created lazily and the loader will be hidden and the items' presenters will be pained\x22\x0a\x09\x0a\x09self loadAndPaintOn: html",
messageSends: ["loadAndPaintOn:"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_presenterClassFor_",
smalltalk.method({
selector: "presenterClassFor:",
category: 'accessing',
fn: function (anItem) {
    var self = this;
    smalltalk.send(self, "_subclassResponsibility", []);
    return self;
},
args: ["anItem"],
source: "presenterClassFor: anItem\x0a\x09\x22Returns the class of the presenter specialized in presenting anItem.\x22\x0a\x0a\x09self subclassResponsibility",
messageSends: ["subclassResponsibility"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_presenterFor_",
smalltalk.method({
selector: "presenterFor:",
category: 'accessing',
fn: function (anItem) {
    var self = this;
    var itemPresenter = nil;
    itemPresenter = smalltalk.send(self, "_ifAbsentAt_put_andDo_", [anItem, function () {return smalltalk.send(self, "_makePresenterFor_", [anItem]);}, function () {return smalltalk.send(self, "_observeItemPresenter_", [smalltalk.send(self, "_at_", [anItem])]);}]);
    return itemPresenter;
    return self;
},
args: ["anItem"],
source: "presenterFor: anItem\x0a\x09\x22Answers the presenter that corresponds to anItem.\x0a\x09If this presenter doesn't have a subpresenter for anItem,\x0a\x09it'll be lazily created.\x22\x0a\x09| itemPresenter |\x0a\x0a\x09itemPresenter := self \x0a\x09\x09ifAbsentAt: anItem \x0a\x09\x09put:[self makePresenterFor: anItem]\x0a\x09\x09andDo:[self observeItemPresenter: (self at: anItem) ].\x0a\x0a\x09^ itemPresenter\x0a\x09",
messageSends: ["ifAbsentAt:put:andDo:", "makePresenterFor:", "observeItemPresenter:", "at:"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_refresh",
smalltalk.method({
selector: "refresh",
category: 'actions',
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
},
args: [],
source: "refresh\x0a\x09\x0a\x09self isLoaded ifFalse:[^self].\x0a\x0a\x09self reset.\x0a\x09self loadAndPaintOn: self newCanvas",
messageSends: ["ifFalse:", "isLoaded", "reset", "loadAndPaintOn:", "newCanvas"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_refreshDo_",
smalltalk.method({
selector: "refreshDo:",
category: 'actions',
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
},
args: ["aBlock"],
source: "refreshDo: aBlock\x0a\x0a\x09self isLoaded ifFalse:[^self].\x0a\x0a\x09self reset.\x0a\x09self loadAndPaintOn: self newCanvas done: aBlock",
messageSends: ["ifFalse:", "isLoaded", "reset", "loadAndPaintOn:done:", "newCanvas"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_reset",
smalltalk.method({
selector: "reset",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_removeAll", []);
    self['@items'] = nil;
    return self;
},
args: [],
source: "reset\x0a\x0a\x09self removeAll.\x0a\x09items := nil.\x0a\x09",
messageSends: ["removeAll"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);

smalltalk.addMethod(
"_showLoader",
smalltalk.method({
selector: "showLoader",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(smalltalk.send(smalltalk.send(self, "_loader", []), "_asJQuery", []), "_hide", []), "_fadeIn", []);
    return self;
},
args: [],
source: "showLoader\x0a\x0a\x09self loader asJQuery hide fadeIn\x0a",
messageSends: ["fadeIn", "hide", "asJQuery", "loader"],
referencedClasses: []
}),
smalltalk.ItemsPresenter);



smalltalk.addClass('BunchPresenter', smalltalk.ItemsPresenter, ['bunchSize', 'index', 'atEnd', 'lastItems', 'more'], 'Flow-Presenters');
smalltalk.BunchPresenter.comment="This preseter is specialized in presenting items that are loaded in bunches.\x0aThe intention is to fragment the quantity of items to present at one time so the experience gets improved.\x0a\x0aSubpresenters of BunchPresenter can do things like painting 20 tweets at the time, query for another 20 tweets, so on and so forth"
smalltalk.addMethod(
"_addItems_",
smalltalk.method({
selector: "addItems:",
category: 'actions',
fn: function (someItems) {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_items", []), "_addAll_", [someItems]);
    return self;
},
args: ["someItems"],
source: "addItems: someItems\x0a\x0a\x09self items addAll: someItems",
messageSends: ["addAll:", "items"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_atEnd",
smalltalk.method({
selector: "atEnd",
category: 'testing',
fn: function () {
    var self = this;
    return self['@atEnd'];
    return self;
},
args: [],
source: "atEnd\x0a\x09\x22Answers true if there isn't any other items to load.\x22\x0a\x09^ atEnd",
messageSends: [],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_bunchSize",
smalltalk.method({
selector: "bunchSize",
category: 'accessing',
fn: function () {
    var self = this;
    return ($receiver = self['@bunchSize']) == nil || $receiver == undefined ? function () {return smalltalk.send(self, "_initializeBunchSize", []);}() : $receiver;
    return self;
},
args: [],
source: "bunchSize\x0a\x0a\x09^ bunchSize ifNil:[self initializeBunchSize]",
messageSends: ["ifNil:", "initializeBunchSize"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_bunchSize_",
smalltalk.method({
selector: "bunchSize:",
category: 'accessing',
fn: function (anInteger) {
    var self = this;
    self['@bunchSize'] = anInteger;
    return self;
},
args: ["anInteger"],
source: "bunchSize: anInteger\x0a\x0a\x09bunchSize := anInteger",
messageSends: [],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_end",
smalltalk.method({
selector: "end",
category: 'accessing',
fn: function () {
    var self = this;
    return ($receiver = smalltalk.send(self, "_index", [])).klass === smalltalk.Number ? $receiver * smalltalk.send(self, "_bunchSize", []) : smalltalk.send($receiver, "__star", [smalltalk.send(self, "_bunchSize", [])]);
    return self;
},
args: [],
source: "end\x0a\x0a\x09^ self index * self bunchSize",
messageSends: ["*", "index", "bunchSize"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_getItemsDo_",
smalltalk.method({
selector: "getItemsDo:",
category: 'actions',
fn: function (aBlock) {
    var self = this;
    return smalltalk.send(self, "_subclassResponsibility", []);
    return self;
},
args: ["aBlock"],
source: "getItemsDo: aBlock\x0a\x09\x22Returns the collection of items that should be presented by this presenter.\x0a\x09Evaluates the onDone reaction when the answer arrives.\x22\x0a\x0a\x09\x22You can use this idea in your concrete ItemsPresenter subclass:\x0a\x0a\x09jQuery ajax: #{ \x0a\x09\x09'url' -> ('api/Blah/find?whatever=thing&from=',self start asString,'&to=',self end asString).\x0a\x09\x09'type' -> 'GET'.\x0a\x09\x09'success' -> [:x| self onAfterLoad: x do: onDone].\x0a\x09\x09'fail' -> [:x| GettingItemsError signal: 'Cannot get the Blah kind of items: ', x responseText].\x0a\x09\x09'error' -> [:x| GettingItemsError signal: 'Cannot get the Blah kind of items: ', x responseText]}\x22  \x0a\x0a\x09^ self subclassResponsibility",
messageSends: ["subclassResponsibility"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_index",
smalltalk.method({
selector: "index",
category: 'accessing',
fn: function () {
    var self = this;
    return ($receiver = self['@index']) == nil || $receiver == undefined ? function () {return smalltalk.send(self, "_initializeIndex", []);}() : $receiver;
    return self;
},
args: [],
source: "index\x0a\x0a\x09^ index ifNil:[self initializeIndex]",
messageSends: ["ifNil:", "initializeIndex"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_index_",
smalltalk.method({
selector: "index:",
category: 'accessing',
fn: function (anInteger) {
    var self = this;
    self['@index'] = anInteger;
    return self;
},
args: ["anInteger"],
source: "index: anInteger\x0a\x0a\x09index := anInteger",
messageSends: [],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_initialize",
smalltalk.method({
selector: "initialize",
category: 'initialization',
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.BunchPresenter.superclass || nil);
    self['@atEnd'] = false;
    return self;
},
args: [],
source: "initialize\x0a\x0a\x09super initialize.\x0a\x0a\x09atEnd := false",
messageSends: ["initialize"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_initializeBunchSize",
smalltalk.method({
selector: "initializeBunchSize",
category: 'initialization',
fn: function () {
    var self = this;
    return self['@bunchSize'] = smalltalk.send(smalltalk.send(self, "_class", []), "_defaultBunchSize", []);
    return self;
},
args: [],
source: "initializeBunchSize\x0a\x0a\x09^ bunchSize := self class defaultBunchSize",
messageSends: ["defaultBunchSize", "class"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_initializeIndex",
smalltalk.method({
selector: "initializeIndex",
category: 'initialization',
fn: function () {
    var self = this;
    return self['@index'] = 1;
    return self;
},
args: [],
source: "initializeIndex\x0a\x0a\x09^ index := 1",
messageSends: [],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_initializeItems",
smalltalk.method({
selector: "initializeItems",
category: 'initialization',
fn: function () {
    var self = this;
    return self['@items'] = smalltalk.send(smalltalk.Array || Array, "_new", []);
    return self;
},
args: [],
source: "initializeItems\x0a\x0a\x09^ items := Array new",
messageSends: ["new"],
referencedClasses: ["Array"]
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_initializeMore",
smalltalk.method({
selector: "initializeMore",
category: 'initialization',
fn: function () {
    var self = this;
    return self['@more'] = smalltalk.send(self, "_makeMore", []);
    return self;
},
args: [],
source: "initializeMore\x0a\x0a\x09^ more := self makeMore",
messageSends: ["makeMore"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_items",
smalltalk.method({
selector: "items",
category: 'accessing',
fn: function () {
    var self = this;
    return ($receiver = self['@items']) == nil || $receiver == undefined ? function () {return smalltalk.send(self, "_initializeItems", []);}() : $receiver;
    return self;
},
args: [],
source: "items\x0a\x0a\x09^ items ifNil:[self initializeItems]",
messageSends: ["ifNil:", "initializeItems"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_itemsDo_",
smalltalk.method({
selector: "itemsDo:",
category: 'actions',
fn: function (aBlock) {
    var self = this;
    smalltalk.send(self, "_loadItemsDo_", [aBlock]);
    return self;
},
args: ["aBlock"],
source: "itemsDo: aBlock\x0a\x09\x22Performs aBlock either after loading the items or when it already have them.\x22\x0a\x0a\x09self loadItemsDo: aBlock",
messageSends: ["loadItemsDo:"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_makeMore",
smalltalk.method({
selector: "makeMore",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_subclassResponsibility", []);
    return self;
},
args: [],
source: "makeMore\x0a\x09\x22Makes the button that should load more items.\x22\x0a\x0a\x09self subclassResponsibility\x0a\x0a\x09\x22 use this as an idea\x0a\x0a\x09^(self newCanvasOn: '#ThisPresenter' asJQuery) a\x0a\x09\x09id: 'SeeMoreItems';\x0a\x09\x09onClick:[self onMore];\x0a\x09\x09with: 'More items...';\x09\x0a\x09\x09yourself\x22",
messageSends: ["subclassResponsibility"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_more",
smalltalk.method({
selector: "more",
category: 'accessing',
fn: function () {
    var self = this;
    return ($receiver = self['@more']) == nil || $receiver == undefined ? function () {return smalltalk.send(self, "_initializeMore", []);}() : $receiver;
    return self;
},
args: [],
source: "more\x0a\x0a\x09^ more ifNil:[self initializeMore]\x0a",
messageSends: ["ifNil:", "initializeMore"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_nextBunch",
smalltalk.method({
selector: "nextBunch",
category: 'accessing',
fn: function () {
    var self = this;
    return smalltalk.send(self, "_index_", [($receiver = smalltalk.send(self, "_index", [])).klass === smalltalk.Number ? $receiver + 1 : smalltalk.send($receiver, "__plus", [1])]);
    return self;
},
args: [],
source: "nextBunch\x0a\x0a\x09^ self index: self index + 1",
messageSends: ["index:", "+", "index"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_onAfterLoad_do_",
smalltalk.method({
selector: "onAfterLoad:do:",
category: 'reactions',
fn: function (someJson, onDone) {
    var self = this;
    self['@lastItems'] = smalltalk.send(self, "_makeItemsFromJson_", [someJson]);
    smalltalk.send(self, "_addItems_", [self['@lastItems']]);
    ($receiver = ($receiver = smalltalk.send(self, "_bunchSize", [])).klass === smalltalk.Number ? $receiver > smalltalk.send(self['@lastItems'], "_size", []) : smalltalk.send($receiver, "__gt", [smalltalk.send(self['@lastItems'], "_size", [])])).klass === smalltalk.Boolean ? $receiver ? function () {return self['@atEnd'] = true;}() : nil : smalltalk.send($receiver, "_ifTrue_", [function () {return self['@atEnd'] = true;}]);
    smalltalk.send(onDone, "_value", []);
    return self;
},
args: ["someJson", "onDone"],
source: "onAfterLoad: someJson do: onDone\x0a\x0a\x09lastItems := self makeItemsFromJson: someJson.\x0a\x09self addItems: lastItems.\x0a\x09self bunchSize > lastItems size ifTrue:[\x0a\x09\x09atEnd := true].\x0a\x0a\x09onDone value\x0a\x0a",
messageSends: ["makeItemsFromJson:", "addItems:", "ifTrue:", ">", "bunchSize", "size", "value"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_onAfterLoaded",
smalltalk.method({
selector: "onAfterLoaded",
category: 'reactions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_onAfterLoaded", [], smalltalk.BunchPresenter.superclass || nil);
    smalltalk.send(smalltalk.send(smalltalk.send(self, "_wrapper", []), "_asJQuery", []), "_append_", [smalltalk.send(smalltalk.send(self, "_loader", []), "_asJQuery", [])]);
    return self;
},
args: [],
source: "onAfterLoaded\x0a\x0a\x09super onAfterLoaded.\x0a\x0a\x09\x22Once the items are loaded, move the loader to the bottom.\x22\x0a\x09self wrapper asJQuery append: self loader asJQuery.",
messageSends: ["onAfterLoaded", "append:", "asJQuery", "wrapper", "loader"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_paintItemsOn_",
smalltalk.method({
selector: "paintItemsOn:",
category: 'painting',
fn: function (html) {
    var self = this;
    smalltalk.send(self['@lastItems'], "_do_", [function (item) {return smalltalk.send(self, "_paint_", [smalltalk.send(self, "_presenterFor_", [item])]);}]);
    return self;
},
args: ["html"],
source: "paintItemsOn: html\x0a\x0a\x09lastItems do:[:item|\x0a\x09\x09self paint: (self presenterFor: item)].\x0a",
messageSends: ["do:", "paint:", "presenterFor:"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_removeItems_",
smalltalk.method({
selector: "removeItems:",
category: 'actions',
fn: function (someItems) {
    var self = this;
    smalltalk.send(smalltalk.send(self, "_items", []), "_removeAll_", [someItems]);
    return self;
},
args: ["someItems"],
source: "removeItems: someItems\x0a\x0a\x09self items removeAll: someItems",
messageSends: ["removeAll:", "items"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_reset",
smalltalk.method({
selector: "reset",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(self, "_reset", [], smalltalk.BunchPresenter.superclass || nil);
    self['@index'] = nil;
    self['@atEnd'] = false;
    return self;
},
args: [],
source: "reset\x0a\x0a\x09super reset.\x0a\x0a\x09index := nil.\x0a\x09atEnd := false",
messageSends: ["reset"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_showLoader",
smalltalk.method({
selector: "showLoader",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(smalltalk.send(smalltalk.send(self, "_loader", []), "_asJQuery", []), "_hide", []), "_fadeIn", []);
    return self;
},
args: [],
source: "showLoader\x0a\x0a\x09self loader asJQuery hide fadeIn",
messageSends: ["fadeIn", "hide", "asJQuery", "loader"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_showMore",
smalltalk.method({
selector: "showMore",
category: 'actions',
fn: function () {
    var self = this;
    smalltalk.send(smalltalk.send(smalltalk.send(smalltalk.send(self, "_more", []), "_asJQuery", []), "_hide", []), "_fadeIn", []);
    return self;
},
args: [],
source: "showMore\x0a\x0a\x09self more asJQuery hide fadeIn",
messageSends: ["fadeIn", "hide", "asJQuery", "more"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_showNext",
smalltalk.method({
selector: "showNext",
category: 'actions',
fn: function () {
    var self = this;
    var html = nil;
    html = smalltalk.send(self, "_newCanvas", []);
    smalltalk.send(self, "_nextBunch", []);
    smalltalk.send(self, "_loadAndPaintOn_", [html]);
    return self;
},
args: [],
source: "showNext\x0a\x0a\x09| html|\x0a\x09html := self newCanvas.\x0a\x09self nextBunch.\x0a\x09self loadAndPaintOn: html\x0a",
messageSends: ["newCanvas", "nextBunch", "loadAndPaintOn:"],
referencedClasses: []
}),
smalltalk.BunchPresenter);

smalltalk.addMethod(
"_start",
smalltalk.method({
selector: "start",
category: 'accessing',
fn: function () {
    var self = this;
    return ($receiver = ($receiver = ($receiver = smalltalk.send(self, "_index", [])).klass === smalltalk.Number ? $receiver - 1 : smalltalk.send($receiver, "__minus", [1])).klass === smalltalk.Number ? $receiver * smalltalk.send(self, "_bunchSize", []) : smalltalk.send($receiver, "__star", [smalltalk.send(self, "_bunchSize", [])])).klass === smalltalk.Number ? $receiver + 1 : smalltalk.send($receiver, "__plus", [1]);
    return self;
},
args: [],
source: "start\x0a\x0a\x09^ ((self index -1) * self bunchSize) +1",
messageSends: ["+", "*", "-", "index", "bunchSize"],
referencedClasses: []
}),
smalltalk.BunchPresenter);


smalltalk.addMethod(
"_defaultBunchSize",
smalltalk.method({
selector: "defaultBunchSize",
category: 'not yet classified',
fn: function () {
    var self = this;
    return 7;
    return self;
},
args: [],
source: "defaultBunchSize\x0a\x09\x22Not even geniuses can pay attention to about \x0a\x093 or 4 things at the same time, so 7 sounds like more than enough.\x0a\x09Of course you can override. Every application should do what it has to do \x0a\x09in order to provide the best user experience.\x22\x0a\x0a\x09^ 7",
messageSends: [],
referencedClasses: []
}),
smalltalk.BunchPresenter.klass);


smalltalk.addClass('TagBrushPresenter', smalltalk.Presenter, [], 'Flow-Presenters');
smalltalk.TagBrushPresenter.comment="This presenter is a wrapping facility for aTagBrush.\x0a\x0aIts value is in wrapping aTagBrush as if it were a flow presenter.\x0a\x0aSo you can make something like\x0a\x0aanAlbum>>makeAddTrack\x0a\x0a\x09aTagBrush := html div id: 'quickDiv'; class: 'aham'; with:[html a with: 'add new track'].\x0a\x0a\x09^ TagBrushPresenter for: aTagBrush\x0a\x0a\x0aIn that way, your Album presenter can treat that div as if it were a subpresenter\x0a\x09\x09\x0a"

smalltalk.addMethod(
"_for_",
smalltalk.method({
selector: "for:",
category: 'actions',
fn: function (aTagBrush) {
    var self = this;
    return function ($rec) {smalltalk.send($rec, "_wrapper_", [aTagBrush]);return smalltalk.send($rec, "_yourself", []);}(smalltalk.send(self, "_new", []));
    return self;
},
args: ["aTagBrush"],
source: "for: aTagBrush\x0a\x0a\x09^ self new\x0a\x09\x09wrapper: aTagBrush;\x0a\x09\x09yourself",
messageSends: ["wrapper:", "yourself", "new"],
referencedClasses: []
}),
smalltalk.TagBrushPresenter.klass);


