conversion
newFrom: aState withStacksMap: stacks
	
	^ self
		newFrom: aState
		withStacksMap: stacks
		fromStackType: RubyCompilerStack
		toStackType: RubyPersistableCompilerStack
