as yet unclassified
testNonWrappedObjectsShouldUnwrap
	| myObj wrappedObj |
	myObj := ''.
	self assert: (RubyWrapper unwrap: myObj) = myObj.

