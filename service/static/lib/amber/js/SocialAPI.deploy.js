smalltalk.addPackage('SocialAPI');
smalltalk.addClass('AbstractSocialAPI', smalltalk.Object, ['appId'], 'SocialAPI');
smalltalk.addMethod(
smalltalk.method({
selector: "appId",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@appId"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"appId",{},smalltalk.AbstractSocialAPI)});},
messageSends: []}),
smalltalk.AbstractSocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "appId:",
fn: function (aString) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@appId"]=aString;
return self}, function($ctx1) {$ctx1.fill(self,"appId:",{aString:aString},smalltalk.AbstractSocialAPI)});},
messageSends: []}),
smalltalk.AbstractSocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "hasLoginDo:",
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclassResponsibility();
return $1;
}, function($ctx1) {$ctx1.fill(self,"hasLoginDo:",{aBlock:aBlock},smalltalk.AbstractSocialAPI)});},
messageSends: ["subclassResponsibility"]}),
smalltalk.AbstractSocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "load",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclassResponsibility();
return $1;
}, function($ctx1) {$ctx1.fill(self,"load",{},smalltalk.AbstractSocialAPI)});},
messageSends: ["subclassResponsibility"]}),
smalltalk.AbstractSocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "login",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclassResponsibility();
return $1;
}, function($ctx1) {$ctx1.fill(self,"login",{},smalltalk.AbstractSocialAPI)});},
messageSends: ["subclassResponsibility"]}),
smalltalk.AbstractSocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "logout",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclassResponsibility();
return $1;
}, function($ctx1) {$ctx1.fill(self,"logout",{},smalltalk.AbstractSocialAPI)});},
messageSends: ["subclassResponsibility"]}),
smalltalk.AbstractSocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "profileDo:",
fn: function (aBlock) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=_st(self)._subclassResponsibility();
return $1;
}, function($ctx1) {$ctx1.fill(self,"profileDo:",{aBlock:aBlock},smalltalk.AbstractSocialAPI)});},
messageSends: ["subclassResponsibility"]}),
smalltalk.AbstractSocialAPI);



smalltalk.addClass('Facebook', smalltalk.AbstractSocialAPI, ['channelUrl'], 'SocialAPI');
smalltalk.addMethod(
smalltalk.method({
selector: "basicLoad",
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
messageSends: []}),
smalltalk.Facebook);

smalltalk.addMethod(
smalltalk.method({
selector: "channelUrl",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1;
$1=self["@channelUrl"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"channelUrl",{},smalltalk.Facebook)});},
messageSends: []}),
smalltalk.Facebook);

smalltalk.addMethod(
smalltalk.method({
selector: "channelUrl:",
fn: function (aString) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
self["@channelUrl"]=aString;
return self}, function($ctx1) {$ctx1.fill(self,"channelUrl:",{aString:aString},smalltalk.Facebook)});},
messageSends: []}),
smalltalk.Facebook);

smalltalk.addMethod(
smalltalk.method({
selector: "hasLoginDo:",
fn: function (aBlock) {
var self=this;
function $FB(){return smalltalk.FB||(typeof FB=="undefined"?nil:FB)}
return smalltalk.withContext(function($ctx1) { 
_st($FB())._getLoginStatus_((function(answer){
return smalltalk.withContext(function($ctx2) {
return _st(aBlock)._value_(_st(_st(answer)._status()).__eq("connected"));
}, function($ctx2) {$ctx2.fillBlock({answer:answer},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"hasLoginDo:",{aBlock:aBlock},smalltalk.Facebook)});},
messageSends: ["getLoginStatus:", "value:", "=", "status"]}),
smalltalk.Facebook);

smalltalk.addMethod(
smalltalk.method({
selector: "load",
fn: function () {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=self;
_st($1)._setup();
$2=_st($1)._basicLoad();
return self}, function($ctx1) {$ctx1.fill(self,"load",{},smalltalk.Facebook)});},
messageSends: ["setup", "basicLoad"]}),
smalltalk.Facebook);

smalltalk.addMethod(
smalltalk.method({
selector: "setup",
fn: function () {
var self=this;
function $FB(){return smalltalk.FB||(typeof FB=="undefined"?nil:FB)}
return smalltalk.withContext(function($ctx1) { 
_st(window)._at_put_("fbAsyncInit",(function(){
return smalltalk.withContext(function($ctx2) {
return _st($FB())._init_(smalltalk.HashedCollection._fromPairs_([_st("appId").__minus_gt(_st(self)._appId()),_st("channelUrl").__minus_gt(_st(self)._channelUrl()),_st("status").__minus_gt(true),_st("xfbml").__minus_gt(true)]));
}, function($ctx2) {$ctx2.fillBlock({},$ctx1)})}));
return self}, function($ctx1) {$ctx1.fill(self,"setup",{},smalltalk.Facebook)});},
messageSends: ["at:put:", "init:", "->", "appId", "channelUrl"]}),
smalltalk.Facebook);



smalltalk.addClass('GooglePlus', smalltalk.AbstractSocialAPI, [], 'SocialAPI');


smalltalk.addClass('LinkedIn', smalltalk.AbstractSocialAPI, [], 'SocialAPI');
smalltalk.addMethod(
smalltalk.method({
selector: "renderLoadOn:",
fn: function (html) {
var self=this;
return smalltalk.withContext(function($ctx1) { 
var $1,$2;
$1=_st(html)._script();
_st($1)._at_put_("type","text/javascript");
$2=_st($1)._src_("http://platform.linkedin.com/in.js");
_st(_st($2)._asJQuery())._html_(_st(_st("api_key: ").__comma(_st(self)._appId())).__comma("\x0aauthorize: true"));
return self}, function($ctx1) {$ctx1.fill(self,"renderLoadOn:",{html:html},smalltalk.LinkedIn)});},
messageSends: ["html:", ",", "appId", "asJQuery", "at:put:", "script", "src:"]}),
smalltalk.LinkedIn);



smalltalk.addClass('SocialAPI', smalltalk.Object, ['facebook', 'linkedIn', 'googlePlus'], 'SocialAPI');
smalltalk.addMethod(
smalltalk.method({
selector: "facebook",
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
messageSends: ["ifNil:", "initializeFacebook"]}),
smalltalk.SocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "googlePlus",
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
messageSends: ["ifNil:", "initializeGooglePlus"]}),
smalltalk.SocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeFacebook",
fn: function () {
var self=this;
function $Facebook(){return smalltalk.Facebook||(typeof Facebook=="undefined"?nil:Facebook)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@facebook"]=_st($Facebook())._new();
$1=self["@facebook"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeFacebook",{},smalltalk.SocialAPI)});},
messageSends: ["new"]}),
smalltalk.SocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeGooglePlus",
fn: function () {
var self=this;
function $GooglePlus(){return smalltalk.GooglePlus||(typeof GooglePlus=="undefined"?nil:GooglePlus)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@googlePlus"]=_st($GooglePlus())._new();
$1=self["@googlePlus"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeGooglePlus",{},smalltalk.SocialAPI)});},
messageSends: ["new"]}),
smalltalk.SocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "initializeLinkedIn",
fn: function () {
var self=this;
function $LinkedIn(){return smalltalk.LinkedIn||(typeof LinkedIn=="undefined"?nil:LinkedIn)}
return smalltalk.withContext(function($ctx1) { 
var $1;
self["@linkedIn"]=_st($LinkedIn())._new();
$1=self["@linkedIn"];
return $1;
}, function($ctx1) {$ctx1.fill(self,"initializeLinkedIn",{},smalltalk.SocialAPI)});},
messageSends: ["new"]}),
smalltalk.SocialAPI);

smalltalk.addMethod(
smalltalk.method({
selector: "linkedIn",
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
messageSends: ["ifNil:", "initializeLinkedIn"]}),
smalltalk.SocialAPI);



