
set class RubyWrapper class
category: 'Instance Creation'
method:
on: anObject

	^ (anObject respondsTo: #'__wrappedRubyObject')
		ifTrue: ["Don't wrap more than once" anObject]
		ifFalse: [self basicNew __wrappedRubyObject: anObject].

%


set class RubyWrapper class
category: 'Instance Conversion'
method:
unwrap: anObject
	"Unwraps the object, if it is currently wrapped with RubyWrapper"
	^ (anObject respondsTo: #'__wrappedRubyObject')
		ifTrue: [anObject __wrappedRubyObject]
		ifFalse: [anObject]

%


set class RubyWrapper
category: 'class membership'
method:
class

	^ wrappedObject class

%


set class RubyWrapper
category: 'error handling'
method:
doesNotUnderstand: aMessage
	| bridgeSelector argSize rubySelector result args |
	argSize := aMessage selector argumentCount.
	args := aMessage arguments collect: [:a | "Unwrap the arguments, or else the call will fail"
												RubyWrapper unwrap: a].
	rubySelector := (aMessage selector asString copyUpTo: $:).
	(argSize > 1 and: [(aMessage selector asString _rubyAt1: -3 length: 3) = 'do:']) ifTrue: ["It's a message with a block"
		result := [wrappedObject @ruby1:send: rubySelector asSymbol
									__STAR: args allButLast
									__BLOCK: (self __rubyPerformWrapperBlockFor: args last)] on: Error do: [:e | nil]].
	(result isNil and: [argSize = 1] and: [args last isKindOf: ExecBlock]) ifTrue: ["Try passing an implicit block"
		result := [wrappedObject @ruby1:send: rubySelector asSymbol
									__BLOCK: (self __rubyPerformWrapperBlockFor: args last)] on: Error do: [:e | nil]].
	(result isNil and: [argSize = 1]) ifTrue: ["Try an accessor"
		result := [wrappedObject @ruby1:send: (rubySelector, '=') asSymbol __STAR: args] on: Error do: [:e | nil]].
	result ifNil: [
		result := [wrappedObject @ruby1:send: rubySelector asSymbol __STAR: args] on: Error do: [:e | nil]].
	^ RubyWrapper on: (result ifNil: ["Call to smalltalk"
											wrappedObject
												perform: aMessage selector
												withArguments: aMessage arguments])

%


set class RubyWrapper
category: 'testing'
method:
ifNil: aBlock ifNotNilDo: anotherBlock

	^ wrappedObject ifNil: aBlock ifNotNilDo: anotherBlock

%


set class RubyWrapper
category: 'testing'
method:
ifNotNilDo: aBlock

	^ wrappedObject ifNotNilDo: aBlock

%


set class RubyWrapper
category: 'testing'
method:
ifNotNilDo: aBlock ifNil: anotherBlock

	^ wrappedObject ifNotNilDo: aBlock ifNil: anotherBlock

%


set class RubyWrapper
category: 'testing'
method:
isBehavior

	^ wrappedObject isBehavior

%


set class RubyWrapper
category: 'testing'
method:
isNil

	^ wrappedObject isNil

%


set class RubyWrapper
category: 'testing'
method:
respondsTo: aSelector
	"This is Smalltalk reflective API, this will not be passed to the wrappedObject"
	^ (RubyWrapper persistentMethodDictForEnv: 0) keys includes: aSelector

%


set class RubyWrapper
category: 'performing'
method:
rubyPerform: selector

	^ RubyWrapper on: (wrappedObject @ruby1:send: selector asSymbol)

%


set class RubyWrapper
category: 'performing'
method:
rubyPerform: selector withArguments: args

	^ RubyWrapper on: (wrappedObject @ruby1:send: selector asSymbol __STAR: args)

%


set class RubyWrapper
category: 'performing'
method:
rubyPerform: selector withArguments: args withBlock: block

	^ RubyWrapper on: (wrappedObject
		@ruby1:send: selector asSymbol __STAR: args __BLOCK: (self __rubyPerformWrapperBlockFor: block))

%


set class RubyWrapper
category: 'performing'
method:
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


set class RubyWrapper
category: 'accessing'
method:
__wrappedRubyObject

	^ wrappedObject

%


set class RubyWrapper
category: 'accessing'
method:
__wrappedRubyObject: anObject

	wrappedObject := anObject.

%

