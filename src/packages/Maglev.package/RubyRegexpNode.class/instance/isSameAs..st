as yet unclassified
isSameAs: other
	^ (super isSameAs: other) and: [self value = other value and: [self options = other options]]

