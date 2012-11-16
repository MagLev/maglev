parsetree-test
isSameAs: other
	^ self newName = other newName
		and: [self oldName = other oldName]

