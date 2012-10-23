as yet unclassified
testAskingForClassShouldReturnWrappeeClass
	| myObj |
	myObj := ''.
	self assert: (RubyWrapper on: myObj) class = myObj class.

