parsetree-test
isSameAs: other
	^ self position isNil or: [other position isNil] or: [self position startLine = other position startLine]

