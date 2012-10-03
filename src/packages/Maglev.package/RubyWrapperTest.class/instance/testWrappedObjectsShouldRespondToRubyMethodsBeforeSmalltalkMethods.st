as yet unclassified
testWrappedObjectsShouldRespondToRubyMethodsBeforeSmalltalkMethods

	self assert: (RubyWrapper unwrap: ((RubyWrapper on: 1) / 2)) ~= (1 / 2).

