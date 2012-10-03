as yet unclassified
testWrappedObjectsShouldRespondToSmalltalkMethods

	self assert: ([RubyWrapper unwrap: (RubyWrapper on: '') asString] on: Error do: [nil]) ~= nil

