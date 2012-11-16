error handling
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

