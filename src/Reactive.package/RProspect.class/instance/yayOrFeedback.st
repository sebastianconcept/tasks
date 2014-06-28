actions
yayOrFeedback

	^ self feedback isNil 
		ifTrue:  [ 'like' ]
		ifFalse: [ 'feedback' ]