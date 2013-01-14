performing
rubyPerform: selector withArguments: args

	^ RubyWrapper on: (wrappedObject @ruby1:send: selector asSymbol __STAR: args)

