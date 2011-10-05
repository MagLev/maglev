
doit
TestCase subclass: 'RubyWrapperTest'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Tests'
	options: #()

%

set class RubyWrapperTest
removeallmethods
removeallclassmethods

set class RubyWrapperTest
category: 'as yet unclassified'
method:
testAskingForClassShouldReturnWrappeeClass
	| myObj |
	myObj := ''.
	self assert: (RubyWrapper on: myObj) class = myObj class.

%


set class RubyWrapperTest
category: 'as yet unclassified'
method:
testNonWrappedObjectsShouldUnwrap
	| myObj wrappedObj |
	myObj := ''.
	self assert: (RubyWrapper unwrap: myObj) = myObj.

%


set class RubyWrapperTest
category: 'as yet unclassified'
method:
testObjectsShouldNotWrapMultiply
	| myObj |
	myObj := ''.
	self assert: (RubyWrapper on: (RubyWrapper on: myObj)) __wrappedRubyObject = myObj.

%


set class RubyWrapperTest
category: 'as yet unclassified'
method:
testObjectsShouldUnwrap
	| myObj wrappedObj |
	myObj := ''.
	wrappedObj := RubyWrapper on: myObj.
	self assert: (RubyWrapper unwrap: wrappedObj) = wrappedObj __wrappedRubyObject.

%


set class RubyWrapperTest
category: 'as yet unclassified'
method:
testWrappedObjectsShouldRespondToRubyMethods

	self assert: ((RubyWrapper unwrap: ([(RubyWrapper on: '') to_s] on: Error do: [:e | nil])) = '').

%


set class RubyWrapperTest
category: 'as yet unclassified'
method:
testWrappedObjectsShouldRespondToRubyMethodsBeforeSmalltalkMethods

	self assert: (RubyWrapper unwrap: ((RubyWrapper on: 1) / 2)) ~= (1 / 2).

%


set class RubyWrapperTest
category: 'as yet unclassified'
method:
testWrappedObjectsShouldRespondToSmalltalkMethods

	self assert: ([RubyWrapper unwrap: (RubyWrapper on: '') asString] on: Error do: [nil]) ~= nil

%

