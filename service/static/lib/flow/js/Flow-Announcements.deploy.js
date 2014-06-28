smalltalk.addPackage('Flow-Announcements', {});
smalltalk.addClass('Announcement', smalltalk.Object, ['subject', 'isBubbling'], 'Flow-Announcements');
smalltalk.addMethod(
"_initialize",
smalltalk.method({
selector: "initialize",
fn: function () {
    var self = this;
    smalltalk.send(self, "_initialize", [], smalltalk.Announcement.superclass || nil);
    self['@isBubbling'] = true;
    return self;
}
}),
smalltalk.Announcement);

smalltalk.addMethod(
"_isBubbling",
smalltalk.method({
selector: "isBubbling",
fn: function () {
    var self = this;
    return self['@isBubbling'];
    return self;
}
}),
smalltalk.Announcement);

smalltalk.addMethod(
"_stop",
smalltalk.method({
selector: "stop",
fn: function () {
    var self = this;
    self['@isBubbling'] = false;
    return self;
}
}),
smalltalk.Announcement);

smalltalk.addMethod(
"_subject",
smalltalk.method({
selector: "subject",
fn: function () {
    var self = this;
    return self['@subject'];
    return self;
}
}),
smalltalk.Announcement);

smalltalk.addMethod(
"_subject_",
smalltalk.method({
selector: "subject:",
fn: function (anObject) {
    var self = this;
    self['@subject'] = anObject;
    return self;
}
}),
smalltalk.Announcement);


smalltalk.addMethod(
"_for_",
smalltalk.method({
selector: "for:",
fn: function (anObject) {
    var self = this;
    return function ($rec) {smalltalk.send($rec, "_subject_", [anObject]);return smalltalk.send($rec, "_yourself", []);}(smalltalk.send(self, "_new", []));
    return self;
}
}),
smalltalk.Announcement.klass);


smalltalk.addClass('AjaxSuccess', smalltalk.Announcement, [], 'Flow-Announcements');


smalltalk.addClass('DiscardChanges', smalltalk.Announcement, [], 'Flow-Announcements');


smalltalk.addClass('FeedbackRequest', smalltalk.Announcement, ['polarity'], 'Flow-Announcements');
smalltalk.addMethod(
"_beNegative",
smalltalk.method({
selector: "beNegative",
fn: function () {
    var self = this;
    self['@polarity'] = "negative";
    return self;
}
}),
smalltalk.FeedbackRequest);

smalltalk.addMethod(
"_beNeutral",
smalltalk.method({
selector: "beNeutral",
fn: function () {
    var self = this;
    self['@polarity'] = "neutral";
    return self;
}
}),
smalltalk.FeedbackRequest);

smalltalk.addMethod(
"_bePositive",
smalltalk.method({
selector: "bePositive",
fn: function () {
    var self = this;
    self['@polarity'] = "positive";
    return self;
}
}),
smalltalk.FeedbackRequest);

smalltalk.addMethod(
"_polarity",
smalltalk.method({
selector: "polarity",
fn: function () {
    var self = this;
    return self['@polarity'];
    return self;
}
}),
smalltalk.FeedbackRequest);

smalltalk.addMethod(
"_polarity_",
smalltalk.method({
selector: "polarity:",
fn: function (aString) {
    var self = this;
    self['@polarity'] = aString;
    return self;
}
}),
smalltalk.FeedbackRequest);



smalltalk.addClass('GoBackClicked', smalltalk.Announcement, [], 'Flow-Announcements');


smalltalk.addClass('ItemsLoaded', smalltalk.Announcement, [], 'Flow-Announcements');


smalltalk.addClass('ModelCreated', smalltalk.Announcement, [], 'Flow-Announcements');


smalltalk.addClass('ModelDeleted', smalltalk.Announcement, [], 'Flow-Announcements');


smalltalk.addClass('ModelRefreshed', smalltalk.Announcement, [], 'Flow-Announcements');


smalltalk.addClass('ModelSaved', smalltalk.Announcement, [], 'Flow-Announcements');


smalltalk.addClass('SaveChanges', smalltalk.Announcement, [], 'Flow-Announcements');


