performing
rubyPerform: selector

	^ RubyWrapper on: (wrappedObject @ruby1:send: selector asSymbol)

