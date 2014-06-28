smalltalk.addPackage('Flow-Announcements', {});
smalltalk.addClass('Announcement', smalltalk.Object, ['subject', 'isBubbling'], 'Flow-Announcements');
smalltalk.addMethod(
"_initialize",
smalltalk.method({
selector: "initialize",
category: 'initialization',
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.Announcement.superclass || nil);
    self['@isBubbling'] = true;
    return self;
},
args: [],
source: "initialize\x0a\x0a\x09super initialize.\x0a\x09\x0a\x09isBubbling := true",
messageSends: ["initialize"],
referencedClasses: []
}),
smalltalk.Announcement);

smalltalk.addMethod(
"_isBubbling",
smalltalk.method({
selector: "isBubbling",
category: 'testing',
fn: function () {
    var self = this;
    return self['@isBubbling'];
    return self;
},
args: [],
source: "isBubbling\x0a\x09\x0a\x09^ isBubbling ",
messageSends: [],
referencedClasses: []
}),
smalltalk.Announcement);

smalltalk.addMethod(
"_stop",
smalltalk.method({
selector: "stop",
category: 'actions',
fn: function () {
    var self = this;
    self['@isBubbling'] = false;
    return self;
},
args: [],
source: "stop\x0a\x09\x0a\x09isBubbling := false",
messageSends: [],
referencedClasses: []
}),
smalltalk.Announcement);

smalltalk.addMethod(
"_subject",
smalltalk.method({
selector: "subject",
category: 'accessing',
fn: function () {
    var self = this;
    return self['@subject'];
    return self;
},
args: [],
source: "subject\x0a\x0a\x09^ subject",
messageSends: [],
referencedClasses: []
}),
smalltalk.Announcement);

smalltalk.addMethod(
"_subject_",
smalltalk.method({
selector: "subject:",
category: 'accessing',
fn: function (anObject) {
    var self = this;
    self['@subject'] = anObject;
    return self;
},
args: ["anObject"],
source: "subject: anObject\x0a\x0a\x09subject := anObject",
messageSends: [],
referencedClasses: []
}),
smalltalk.Announcement);


smalltalk.addMethod(
"_for_",
smalltalk.method({
selector: "for:",
category: 'not yet classified',
fn: function (anObject) {
    var self = this;
    return function ($rec) {smalltalk.send($rec, "_subject_", [anObject]);return smalltalk.send($rec, "_yourself", []);}(smalltalk.send(self, "_new", []));
    return self;
},
args: ["anObject"],
source: "for: anObject\x0a\x0a\x09^ self new\x0a\x09\x09subject: anObject;\x0a\x09\x09yourself",
messageSends: ["subject:", "yourself", "new"],
referencedClasses: []
}),
smalltalk.Announcement.klass);


smalltalk.addClass('AjaxSuccess', smalltalk.Announcement, [], 'Flow-Announcements');


smalltalk.addClass('DiscardChanges', smalltalk.Announcement, [], 'Flow-Announcements');


smalltalk.addClass('FeedbackRequest', smalltalk.Announcement, ['polarity'], 'Flow-Announcements');
smalltalk.addMethod(
"_beNegative",
smalltalk.method({
selector: "beNegative",
category: 'actions',
fn: function () {
    var self = this;
    self['@polarity'] = "negative";
    return self;
},
args: [],
source: "beNegative\x0a\x0a\x09polarity := 'negative'",
messageSends: [],
referencedClasses: []
}),
smalltalk.FeedbackRequest);

smalltalk.addMethod(
"_beNeutral",
smalltalk.method({
selector: "beNeutral",
category: 'actions',
fn: function () {
    var self = this;
    self['@polarity'] = "neutral";
    return self;
},
args: [],
source: "beNeutral\x0a\x0a\x09polarity := 'neutral'",
messageSends: [],
referencedClasses: []
}),
smalltalk.FeedbackRequest);

smalltalk.addMethod(
"_bePositive",
smalltalk.method({
selector: "bePositive",
category: 'actions',
fn: function () {
    var self = this;
    self['@polarity'] = "positive";
    return self;
},
args: [],
source: "bePositive\x0a\x0a\x09polarity := 'positive'",
messageSends: [],
referencedClasses: []
}),
smalltalk.FeedbackRequest);

smalltalk.addMethod(
"_polarity",
smalltalk.method({
selector: "polarity",
category: 'accessing',
fn: function () {
    var self = this;
    return self['@polarity'];
    return self;
},
args: [],
source: "polarity\x0a\x0a\x09^ polarity",
messageSends: [],
referencedClasses: []
}),
smalltalk.FeedbackRequest);

smalltalk.addMethod(
"_polarity_",
smalltalk.method({
selector: "polarity:",
category: 'accessing',
fn: function (aString) {
    var self = this;
    self['@polarity'] = aString;
    return self;
},
args: ["aString"],
source: "polarity: aString\x0a\x0a\x09polarity := aString",
messageSends: [],
referencedClasses: []
}),
smalltalk.FeedbackRequest);



smalltalk.addClass('GoBackClicked', smalltalk.Announcement, [], 'Flow-Announcements');


smalltalk.addClass('ItemsLoaded', smalltalk.Announcement, [], 'Flow-Announcements');


smalltalk.addClass('ModelCreated', smalltalk.Announcement, [], 'Flow-Announcements');


smalltalk.addClass('ModelDeleted', smalltalk.Announcement, [], 'Flow-Announcements');


smalltalk.addClass('ModelRefreshed', smalltalk.Announcement, [], 'Flow-Announcements');


smalltalk.addClass('ModelSaved', smalltalk.Announcement, [], 'Flow-Announcements');


smalltalk.addClass('SaveChanges', smalltalk.Announcement, [], 'Flow-Announcements');


