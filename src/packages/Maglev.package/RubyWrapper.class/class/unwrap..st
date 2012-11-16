Instance Conversion
unwrap: anObject
	"Unwraps the object, if it is currently wrapped with RubyWrapper"
	^ (anObject respondsTo: #'__wrappedRubyObject')
		ifTrue: [anObject __wrappedRubyObject]
		ifFalse: [anObject]

