Instance Creation
on: anObject

	^ (anObject respondsTo: #'__wrappedRubyObject')
		ifTrue: ["Don't wrap more than once" anObject]
		ifFalse: [self basicNew __wrappedRubyObject: anObject].

