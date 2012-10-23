as yet unclassified
newFrom: aState withStacksMap: stacks
	
	^ self
		newFrom: aState
		withStacksMap: stacks
		fromStackType: RubyPersistableCompilerStack
		toStackType: RubyCompilerStack
