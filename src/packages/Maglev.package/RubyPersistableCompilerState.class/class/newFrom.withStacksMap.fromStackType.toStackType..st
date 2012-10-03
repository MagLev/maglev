conversion
newFrom: aState withStacksMap: stacks fromStackType: aStackClass toStackType: anotherStackClass
	| newState |
	newState := self new.
	RubyPersistableCompilerState "here are all the interesting vars" instVarNames do: [:name || var |
		var := aState instVarNamed: name.
		var class == aStackClass
			ifTrue: [newState instVarNamed: name put: (stacks at: var ifAbsentPut: [anotherStackClass newFrom: var])]
			ifFalse: [newState instVarNamed: name put: var]].
	^ newState
