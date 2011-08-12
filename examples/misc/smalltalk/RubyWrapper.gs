doit
ProtoObject subclass: 'RubyWrapper'
	instVarNames: #( wrappedObject)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Tools'
	options: #()

%

! Remove existing behavior from RubyWrapper
doit
RubyWrapper removeAllMethods.
RubyWrapper class removeAllMethods.
%
! ------------------- Class methods for RubyWrapper
category: 'Instance Creation'
set compile_env: 0
classmethod: RubyWrapper
on: anObject

	^ (anObject respondsTo: #'__wrappedRubyObject')
		ifTrue: ["Don't wrap more than once" anObject]
		ifFalse: [self basicNew __wrappedRubyObject: anObject].
%
category: 'as yet unclassified'
set compile_env: 0
classmethod: RubyWrapper
unwrap: anObject
	"Unwraps the object, if it is currently wrapped with RubyWrapper"
	^ (anObject respondsTo: #'__wrappedRubyObject')
		ifTrue: [anObject __wrappedRubyObject]
		ifFalse: [anObject]
%
! ------------------- Instance methods for RubyWrapper
category: 'class membership'
set compile_env: 0
method: RubyWrapper
class

	^ wrappedObject class
%
category: 'error handling'
set compile_env: 0
method: RubyWrapper
doesNotUnderstand: aMessage
	| bridgeSelector argSize rubySelector result args |
	argSize := aMessage selector argumentCount.
	args := aMessage arguments collect: [:a | "Unwrap the arguments, or else the call will fail"
												RubyWrapper unwrap: a].
	rubySelector := (aMessage selector asString copyUpTo: $:).
	argSize = 1 ifTrue: [ "Try an accessor first"
		result := [wrappedObject @ruby1:send: (rubySelector, '=') asSymbol __STAR: args]
			on: Error do: [:e | nil]].
	result ifNil: [
		result := [wrappedObject @ruby1:send: rubySelector asSymbol __STAR: args]
			on: Error do: [:e | nil]].
	^ RubyWrapper on: (result ifNil: ["Call to smalltalk"
											wrappedObject
												perform: aMessage selector
												withArguments: aMessage arguments])
%
category: 'testing'
set compile_env: 0
method: RubyWrapper
ifNil: aBlock ifNotNilDo: anotherBlock

	^ wrappedObject ifNil: aBlock ifNotNilDo: anotherBlock
%
category: 'testing'
set compile_env: 0
method: RubyWrapper
ifNotNilDo: aBlock

	^ wrappedObject ifNotNilDo: aBlock
%
category: 'testing'
set compile_env: 0
method: RubyWrapper
ifNotNilDo: aBlock ifNil: anotherBlock

	^ wrappedObject ifNotNilDo: aBlock ifNil: anotherBlock
%
category: 'testing'
set compile_env: 0
method: RubyWrapper
isBehavior

	^ wrappedObject isBehavior
%
category: 'testing'
set compile_env: 0
method: RubyWrapper
isNil

	^ wrappedObject isNil
%
category: 'testing'
set compile_env: 0
method: RubyWrapper
respondsTo: aSelector
	"This is Smalltalk reflective API, this will not be passed to the wrappedObject"
	^ (RubyWrapper persistentMethodDictForEnv: 0) keys includes: aSelector
%
category: 'performing'
set compile_env: 0
method: RubyWrapper
rubyPerform: selector

	^ RubyWrapper on: (wrappedObject @ruby1:send: selector asSymbol)
%
category: 'performing'
set compile_env: 0
method: RubyWrapper
rubyPerform: selector withArguments: args

	^ RubyWrapper on: (wrappedObject @ruby1:send: selector asSymbol __STAR: args)
%
category: 'performing'
set compile_env: 0
method: RubyWrapper
rubyPerform: selector withArguments: args withBlock: block

	^ RubyWrapper on: (wrappedObject
		@ruby1:send: selector asSymbol __STAR: args __BLOCK: (self __rubyPerformWrapperBlockFor: block))
%
category: 'performing'
set compile_env: 0
method: RubyWrapper
__rubyPerformWrapperBlockFor: aBlock
	"Creates a block to pass to the ruby invocation, that wraps its arguments in RubyWrappers and continues"
	aBlock numArgs = 0 ifTrue: [^ aBlock].
	aBlock numArgs = 1 ifTrue: [^ [:t1 | aBlock value: (RubyWrapper on: t1)]].
	aBlock numArgs = 2 ifTrue: [^ [:t1 :t2 | aBlock
											value: (RubyWrapper on: t1)
											value: (RubyWrapper on: t2)]].
	aBlock numArgs = 3 ifTrue: [^ [:t1 :t2 :t3 | aBlock
											value: (RubyWrapper on: t1)
											value: (RubyWrapper on: t2)
											value: (RubyWrapper on: t3)]].
	aBlock numArgs = 4 ifTrue: [^ [:t1 :t2 :t3 :t4 | aBlock
											value: (RubyWrapper on: t1)
											value: (RubyWrapper on: t2)
											value: (RubyWrapper on: t3)
											value: (RubyWrapper on: t4)]].
	aBlock numArgs = 5 ifTrue: [^ [:t1 :t2 :t3 :t4 :t5 | aBlock
											value: (RubyWrapper on: t1)
											value: (RubyWrapper on: t2)
											value: (RubyWrapper on: t3)
											value: (RubyWrapper on: t4)
											value: (RubyWrapper on: t5)]].
	^ self error: 'Only blocks with up to 5 arguments are supported by the RubyService'

%
category: 'accessing'
set compile_env: 0
method: RubyWrapper
__wrappedRubyObject

	^ wrappedObject
%
category: 'accessing'
set compile_env: 0
method: RubyWrapper
__wrappedRubyObject: anObject

	wrappedObject := anObject.
%
doit
RubyWrapper category: 'MagLev-Tools'
%
