as yet unclassified
testWrappedObjectsShouldRespondToRubyMethods

	self assert: ((RubyWrapper unwrap: ([(RubyWrapper on: '') to_s] on: Error do: [:e | nil])) = '').

