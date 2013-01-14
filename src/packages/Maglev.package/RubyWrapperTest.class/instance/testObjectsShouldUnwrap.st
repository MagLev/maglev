as yet unclassified
testObjectsShouldUnwrap
	| myObj wrappedObj |
	myObj := ''.
	wrappedObj := RubyWrapper on: myObj.
	self assert: (RubyWrapper unwrap: wrappedObj) = wrappedObj __wrappedRubyObject.

