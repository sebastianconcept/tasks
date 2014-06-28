Smalltalk current createPackage: 'SocialAPI'!
Object subclass: #AbstractSocialAPI
	instanceVariableNames: 'appId'
	package: 'SocialAPI'!

!AbstractSocialAPI methodsFor: 'accessing'!

appId

	^ appId
!

appId: aString

	appId := aString
! !

!AbstractSocialAPI methodsFor: 'actions'!

load

	^ self subclassResponsibility
!

login

	^ self subclassResponsibility
!

logout

	^ self subclassResponsibility
!

profileDo: aBlock
	"Evaluate aBlock with the profile.
	Get it lazily. Use the cached one if already requested."
	^ self subclassResponsibility
! !

!AbstractSocialAPI methodsFor: 'testing'!

hasLoginDo: aBlock
	"Request state and value aBlock with true if there is a user login in this plattform."
	
	^ self subclassResponsibility
! !

AbstractSocialAPI subclass: #Facebook
	instanceVariableNames: 'channelUrl'
	package: 'SocialAPI'!

!Facebook methodsFor: 'actions'!

basicLoad
<// Load the SDK asynchronously
  (function(d, s, id){
     var js, fjs = d.getElementsByTagName(s)[0];
     if (d.getElementById(id)) {return;}
     js = d.createElement(s); js.id = id;
     js.src = "//connect.facebook.net/en_US/all.js";
     fjs.parentNode.insertBefore(js, fjs);
   }(document, 'script', 'facebook-jssdk'));>
!

channelUrl

	^ channelUrl
!

channelUrl: aString

	channelUrl := aString
!

load

	self setup; basicLoad
!

setup

	window at: 'fbAsyncInit' put:[
		FB init: #{
			'appId' -> self appId.
			'channelUrl'-> self channelUrl.
			'status' -> true.
			'xfbml' -> true
			}].
! !

!Facebook methodsFor: 'testing'!

hasLoginDo: aBlock
	"Request state and value aBlock with true if there is a user login in this plattform."
	
	FB getLoginStatus:[:answer| 
		aBlock value: answer status = 'connected' ]
! !

AbstractSocialAPI subclass: #GooglePlus
	instanceVariableNames: ''
	package: 'SocialAPI'!

AbstractSocialAPI subclass: #LinkedIn
	instanceVariableNames: ''
	package: 'SocialAPI'!

!LinkedIn methodsFor: 'rendering'!

renderLoadOn: html

	(html script
		at: 'type' put: 'text/javascript';
		src: 'http://platform.linkedin.com/in.js')
			asJQuery 
				html: 'api_key: ',self appId, '
authorize: true'.
! !

Object subclass: #SocialAPI
	instanceVariableNames: 'facebook linkedIn googlePlus'
	package: 'SocialAPI'!

!SocialAPI methodsFor: 'accessing'!

facebook

	^ facebook ifNil:[self initializeFacebook]
!

googlePlus

	^ googlePlus ifNil:[self initializeGooglePlus]
!

linkedIn

	^ linkedIn ifNil:[self initializeLinkedIn]
! !

!SocialAPI methodsFor: 'initialization'!

initializeFacebook

	^ facebook := Facebook new
!

initializeGooglePlus

	^ googlePlus := GooglePlus new
!

initializeLinkedIn

	^ linkedIn := LinkedIn new
! !

