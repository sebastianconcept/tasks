Smalltalk current createPackage: 'Flow-Announcements' properties: #{}!
Object subclass: #Announcement
	instanceVariableNames: 'subject isBubbling'
	package: 'Flow-Announcements'!

!Announcement methodsFor: 'accessing'!

subject

	^ subject
!

subject: anObject

	subject := anObject
! !

!Announcement methodsFor: 'actions'!

stop
	
	isBubbling := false
! !

!Announcement methodsFor: 'initialization'!

initialize

	super initialize.
	
	isBubbling := true
! !

!Announcement methodsFor: 'testing'!

isBubbling
	
	^ isBubbling
! !

!Announcement class methodsFor: 'not yet classified'!

for: anObject

	^ self new
		subject: anObject;
		yourself
! !

Announcement subclass: #AjaxSuccess
	instanceVariableNames: ''
	package: 'Flow-Announcements'!

Announcement subclass: #DiscardChanges
	instanceVariableNames: ''
	package: 'Flow-Announcements'!

Announcement subclass: #FeedbackRequest
	instanceVariableNames: 'polarity'
	package: 'Flow-Announcements'!

!FeedbackRequest methodsFor: 'accessing'!

polarity

	^ polarity
!

polarity: aString

	polarity := aString
! !

!FeedbackRequest methodsFor: 'actions'!

beNegative

	polarity := 'negative'
!

beNeutral

	polarity := 'neutral'
!

bePositive

	polarity := 'positive'
! !

Announcement subclass: #GoBackClicked
	instanceVariableNames: ''
	package: 'Flow-Announcements'!

Announcement subclass: #ItemsLoaded
	instanceVariableNames: ''
	package: 'Flow-Announcements'!

Announcement subclass: #ModelCreated
	instanceVariableNames: ''
	package: 'Flow-Announcements'!

Announcement subclass: #ModelDeleted
	instanceVariableNames: ''
	package: 'Flow-Announcements'!

Announcement subclass: #ModelRefreshed
	instanceVariableNames: ''
	package: 'Flow-Announcements'!

Announcement subclass: #ModelSaved
	instanceVariableNames: ''
	package: 'Flow-Announcements'!

Announcement subclass: #SaveChanges
	instanceVariableNames: ''
	package: 'Flow-Announcements'!

