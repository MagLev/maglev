performing
rubyPerform: selector withArguments: args withBlock: block

	^ RubyWrapper on: (wrappedObject
		@ruby1:send: selector asSymbol __STAR: args __BLOCK: (self __rubyPerformWrapperBlockFor: block))

