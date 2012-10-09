parsetree
isSameAs: other
	^ self location = other location 
		and: [self name = other name]

