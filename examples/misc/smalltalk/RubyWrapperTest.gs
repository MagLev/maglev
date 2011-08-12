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

! Remove existing behavior from RubyWrapperTest
doit
RubyWrapperTest removeAllMethods.
RubyWrapperTest class removeAllMethods.
%
! ------------------- Class methods for RubyWrapperTest
! ------------------- Instance methods for RubyWrapperTest
category: 'as yet unclassified'
set compile_env: 0
method: RubyWrapperTest
testAskingForClassShouldReturnWrappeeClass
	| myObj |
	myObj := ''.
	self assert: (RubyWrapper on: myObj) class = myObj class.
%
category: 'as yet unclassified'
set compile_env: 0
method: RubyWrapperTest
testNonWrappedObjectsShouldUnwrap
	| myObj wrappedObj |
	myObj := ''.
	self assert: (RubyWrapper unwrap: myObj) = myObj.
%
category: 'as yet unclassified'
set compile_env: 0
method: RubyWrapperTest
testObjectsShouldNotWrapMultiply
	| myObj |
	myObj := ''.
	self assert: (RubyWrapper on: (RubyWrapper on: myObj)) __wrappedRubyObject = myObj.
%
category: 'as yet unclassified'
set compile_env: 0
method: RubyWrapperTest
testObjectsShouldUnwrap
	| myObj wrappedObj |
	myObj := ''.
	wrappedObj := RubyWrapper on: myObj.
	self assert: (RubyWrapper unwrap: wrappedObj) = wrappedObj __wrappedRubyObject.
%
category: 'as yet unclassified'
set compile_env: 0
method: RubyWrapperTest
testWrappedObjectsShouldRespondToRubyMethods

	self assert: ((RubyWrapper unwrap: ([(RubyWrapper on: '') to_s] on: Error do: [:e | nil])) = '').
%
category: 'as yet unclassified'
set compile_env: 0
method: RubyWrapperTest
testWrappedObjectsShouldRespondToRubyMethodsBeforeSmalltalkMethods

	self assert: (RubyWrapper unwrap: ((RubyWrapper on: 1) / 2)) ~= (1 / 2).
%
category: 'as yet unclassified'
set compile_env: 0
method: RubyWrapperTest
testWrappedObjectsShouldRespondToSmalltalkMethods

	self assert: ([RubyWrapper unwrap: (RubyWrapper on: '') asString] on: Error do: [nil]) ~= nil
%
doit
RubyWrapperTest category: 'MagLev-Tests'
%
