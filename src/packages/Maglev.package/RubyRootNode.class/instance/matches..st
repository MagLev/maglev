parsetree-test
matches: other
	^ self species = other species and: [bodyNode matches: other bodyNode]

