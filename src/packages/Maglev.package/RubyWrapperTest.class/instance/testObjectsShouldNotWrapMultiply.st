as yet unclassified
testObjectsShouldNotWrapMultiply
	| myObj |
	myObj := ''.
	self assert: (RubyWrapper on: (RubyWrapper on: myObj)) __wrappedRubyObject = myObj.

