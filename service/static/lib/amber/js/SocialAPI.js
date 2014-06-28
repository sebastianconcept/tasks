smalltalk.addPackage('SocialAPI');
smalltalk.addClass('AbstractSocialAPI', smalltalk.Object, ['appId'], 'SocialAPI');
smalltalk.addMethod(
smalltalk.method({
selector: "appId",
category: 'accessing',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@appId"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"appId",{},smalltalk.AbstractSocialAPI)});},
args: [],
source: "appId\x0a\x0a\x09^ appId",
messageSends: [],
referencedClasses: []
}),
smalltalk.AbstractSocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "appId:",
category: 'accessing',
fn: function (aString) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@appId"]=aString;
return self}, function($ctx1) {$ctx1.fill(self,"appId:",{aString:aString},smalltalk.AbstractSocialAPI)});},
args: ["aString"],
source: "appId: aString\x0a\x0a\x09appId := aString",
messageSends: [],
referencedClasses: []
}),
smalltalk.AbstractSocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "hasLoginDo:",
category: 'testing',
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclassResponsibility();
return $1;
}, function($ctx1) {$ctx1.fill(self,"hasLoginDo:",{aBlock:aBlock},smalltalk.AbstractSocialAPI)});},
args: ["aBlock"],
source: "hasLoginDo: aBlock\x0a\x09\x22Request state and value aBlock with true if there is a user login in this plattform.\x22\x0a\x09\x0a\x09^ self subclassResponsibility",
messageSends: ["subclassResponsibility"],
referencedClasses: []
}),
smalltalk.AbstractSocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "load",
category: 'actions',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclassResponsibility();
return $1;
}, function($ctx1) {$ctx1.fill(self,"load",{},smalltalk.AbstractSocialAPI)});},
args: [],
source: "load\x0a\x0a\x09^ self subclassResponsibility",
messageSends: ["subclassResponsibility"],
referencedClasses: []
}),
smalltalk.AbstractSocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "login",
category: 'actions',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclassResponsibility();
return $1;
}, function($ctx1) {$ctx1.fill(self,"login",{},smalltalk.AbstractSocialAPI)});},
args: [],
source: "login\x0a\x0a\x09^ self subclassResponsibility",
messageSends: ["subclassResponsibility"],
referencedClasses: []
}),
smalltalk.AbstractSocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "logout",
category: 'actions',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclassResponsibility();
return $1;
}, function($ctx1) {$ctx1.fill(self,"logout",{},smalltalk.AbstractSocialAPI)});},
args: [],
source: "logout\x0a\x0a\x09^ self subclassResponsibility",
messageSends: ["subclassResponsibility"],
referencedClasses: []
}),
smalltalk.AbstractSocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "profileDo:",
category: 'actions',
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclassResponsibility();
return $1;
}, function($ctx1) {$ctx1.fill(self,"profileDo:",{aBlock:aBlock},smalltalk.AbstractSocialAPI)});},
args: ["aBlock"],
source: "profileDo: aBlock\x0a\x09\x22Evaluate aBlock with the profile.\x0a\x09Get it lazily. Use the cached one if already requested.\x22\x0a\x09^ self subclassResponsibility",
messageSends: ["subclassResponsibility"],
referencedClasses: []
}),
smalltalk.AbstractSocialAPI);



smalltalk.addClass('Facebook', smalltalk.AbstractSocialAPI, ['channelUrl'], 'SocialAPI');
smalltalk.addMethod(
smalltalk.method({
selector: "basicLoad",
category: 'actions',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
// Load the SDK asynchronously
  (function(d, s, id){
     var js, fjs = d.getElementsByTagName(s)[0];
     if (d.getElementById(id)) {return;}
     js = d.createElement(s); js.id = id;
     js.src = "//connect.facebook.net/en_US/all.js";
     fjs.parentNode.insertBefore(js, fjs);
   }(document, 'script', 'facebook-jssdk'));;
return self}, function($ctx1) {$ctx1.fill(self,"basicLoad",{},smalltalk.Facebook)});},
args: [],
source: "basicLoad\x0a<// Load the SDK asynchronously\x0a  (function(d, s, id){\x0a     var js, fjs = d.getElementsByTagName(s)[0];\x0a     if (d.getElementById(id)) {return;}\x0a     js = d.createElement(s); js.id = id;\x0a     js.src = \x22//connect.facebook.net/en_US/all.js\x22;\x0a     fjs.parentNode.insertBefore(js, fjs);\x0a   }(document, 'script', 'facebook-jssdk'));>",
messageSends: [],
referencedClasses: []
}),
smalltalk.Facebook);

smalltalk.addMethod(
smalltalk.method({
selector: "channelUrl",
category: 'actions',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@channelUrl"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"channelUrl",{},smalltalk.Facebook)});},
args: [],
source: "channelUrl\x0a\x0a\x09^ channelUrl",
messageSends: [],
referencedClasses: []
}),
smalltalk.Facebook);

smalltalk.addMethod(
smalltalk.method({
selector: "channelUrl:",
category: 'actions',
fn: function (aString) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@channelUrl"]=aString;
return self}, function($ctx1) {$ctx1.fill(self,"channelUrl:",{aString:aString},smalltalk.Facebook)});},
args: ["aString"],
source: "channelUrl: aString\x0a\x0a\x09channelUrl := aString",
messageSends: [],
referencedClasses: []
}),
smalltalk.Facebook);

smalltalk.addMethod(
smalltalk.method({
selector: "hasLoginDo:",
category: 'testing',
fn: function (aBlock) {
var self=this;
function $FB(){return smalltalk.FB||(typeof FB=="undefined"?nil:FB)}
return smalltalk.withContext(function($ctx1) { 
_st($FB())._getLoginStatus_((function(answer){
return smalltalk.withContext(function($ctx2) {
return _st(aBlock)._value_(_st(_st(answer)._status()).__eq("connected"));
}, function($ctx2) {$ctx2.fillBlock({answer:answer},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"hasLoginDo:",{aBlock:aBlock},smalltalk.Facebook)});},
args: ["aBlock"],
source: "hasLoginDo: aBlock\x0a\x09\x22Request state and value aBlock with true if there is a user login in this plattform.\x22\x0a\x09\x0a\x09FB getLoginStatus:[:answer| \x0a\x09\x09aBlock value: answer status = 'connected' ]",
messageSends: ["getLoginStatus:", "value:", "=", "status"],
referencedClasses: ["FB"]
}),
smalltalk.Facebook);

smalltalk.addMethod(
smalltalk.method({
selector: "load",
category: 'actions',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=self;
_st($1)._setup();
$2=_st($1)._basicLoad();
return self}, function($ctx1) {$ctx1.fill(self,"load",{},smalltalk.Facebook)});},
args: [],
source: "load\x0a\x0a\x09self setup; basicLoad\x0a\x09",
messageSends: ["setup", "basicLoad"],
referencedClasses: []
}),
smalltalk.Facebook);

smalltalk.addMethod(
smalltalk.method({
selector: "setup",
category: 'actions',
fn: function () {
var self=this;
function $FB(){return smalltalk.FB||(typeof FB=="undefined"?nil:FB)}
return smalltalk.withContext(function($ctx1) { 
_st(window)._at_put_("fbAsyncInit",(function(){
return smalltalk.withContext(function($ctx2) {
return _st($FB())._init_(smalltalk.HashedCollection._fromPairs_([_st("appId").__minus_gt(_st(self)._appId()),_st("channelUrl").__minus_gt(_st(self)._channelUrl()),_st("status").__minus_gt(true),_st("xfbml").__minus_gt(true)]));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"setup",{},smalltalk.Facebook)});},
args: [],
source: "setup\x0a\x0a\x09window at: 'fbAsyncInit' put:[\x0a\x09\x09FB init: #{\x0a\x09\x09\x09'appId' -> self appId.\x0a\x09\x09\x09'channelUrl'-> self channelUrl.\x0a\x09\x09\x09'status' -> true.\x0a\x09\x09\x09'xfbml' -> true\x0a\x09\x09\x09}].",
messageSends: ["at:put:", "init:", "->", "appId", "channelUrl"],
referencedClasses: ["FB"]
}),
smalltalk.Facebook);



smalltalk.addClass('GooglePlus', smalltalk.AbstractSocialAPI, [], 'SocialAPI');


smalltalk.addClass('LinkedIn', smalltalk.AbstractSocialAPI, [], 'SocialAPI');
smalltalk.addMethod(
smalltalk.method({
selector: "renderLoadOn:",
category: 'rendering',
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(html)._script();
_st($1)._at_put_("type","text/javascript");
$2=_st($1)._src_("http://platform.linkedin.com/in.js");
_st(_st($2)._asJQuery())._html_(_st(_st("api_key: ").__comma(_st(self)._appId())).__comma("\x0aauthorize: true"));
return self}, function($ctx1) {$ctx1.fill(self,"renderLoadOn:",{html:html},smalltalk.LinkedIn)});},
args: ["html"],
source: "renderLoadOn: html\x0a\x0a\x09(html script\x0a\x09\x09at: 'type' put: 'text/javascript';\x0a\x09\x09src: 'http://platform.linkedin.com/in.js')\x0a\x09\x09\x09asJQuery \x0a\x09\x09\x09\x09html: 'api_key: ',self appId, '\x0aauthorize: true'.",
messageSends: ["html:", ",", "appId", "asJQuery", "at:put:", "script", "src:"],
referencedClasses: []
}),
smalltalk.LinkedIn);



smalltalk.addClass('SocialAPI', smalltalk.Object, ['facebook', 'linkedIn', 'googlePlus'], 'SocialAPI');
smalltalk.addMethod(
smalltalk.method({
selector: "facebook",
category: 'accessing',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@facebook"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeFacebook();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"facebook",{},smalltalk.SocialAPI)});},
args: [],
source: "facebook\x0a\x0a\x09^ facebook ifNil:[self initializeFacebook]",
messageSends: ["ifNil:", "initializeFacebook"],
referencedClasses: []
}),
smalltalk.SocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "googlePlus",
category: 'accessing',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@googlePlus"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeGooglePlus();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"googlePlus",{},smalltalk.SocialAPI)});},
args: [],
source: "googlePlus\x0a\x0a\x09^ googlePlus ifNil:[self initializeGooglePlus]",
messageSends: ["ifNil:", "initializeGooglePlus"],
referencedClasses: []
}),
smalltalk.SocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeFacebook",
category: 'initialization',
fn: function () {
var self=this;
function $Facebook(){return smalltalk.Facebook||(typeof Facebook=="undefined"?nil:Facebook)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@facebook"]=_st($Facebook())._new();
$1=self["@facebook"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeFacebook",{},smalltalk.SocialAPI)});},
args: [],
source: "initializeFacebook\x0a\x0a\x09^ facebook := Facebook new",
messageSends: ["new"],
referencedClasses: ["Facebook"]
}),
smalltalk.SocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeGooglePlus",
category: 'initialization',
fn: function () {
var self=this;
function $GooglePlus(){return smalltalk.GooglePlus||(typeof GooglePlus=="undefined"?nil:GooglePlus)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@googlePlus"]=_st($GooglePlus())._new();
$1=self["@googlePlus"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeGooglePlus",{},smalltalk.SocialAPI)});},
args: [],
source: "initializeGooglePlus\x0a\x0a\x09^ googlePlus := GooglePlus new",
messageSends: ["new"],
referencedClasses: ["GooglePlus"]
}),
smalltalk.SocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeLinkedIn",
category: 'initialization',
fn: function () {
var self=this;
function $LinkedIn(){return smalltalk.LinkedIn||(typeof LinkedIn=="undefined"?nil:LinkedIn)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@linkedIn"]=_st($LinkedIn())._new();
$1=self["@linkedIn"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeLinkedIn",{},smalltalk.SocialAPI)});},
args: [],
source: "initializeLinkedIn\x0a\x0a\x09^ linkedIn := LinkedIn new",
messageSends: ["new"],
referencedClasses: ["LinkedIn"]
}),
smalltalk.SocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "linkedIn",
category: 'accessing',
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $2,$1;
$2=self["@linkedIn"];
if(($receiver = $2) == nil || $receiver == undefined){
$1=_st(self)._initializeLinkedIn();
} else {
$1=$2;
};
return $1;
}, function($ctx1) {$ctx1.fill(self,"linkedIn",{},smalltalk.SocialAPI)});},
args: [],
source: "linkedIn\x0a\x0a\x09^ linkedIn ifNil:[self initializeLinkedIn]",
messageSends: ["ifNil:", "initializeLinkedIn"],
referencedClasses: []
}),
smalltalk.SocialAPI);



