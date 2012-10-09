*maglev-Debugging Support
convertState: stateClass andStack: stackClass to: newStateClass and: newStackClass
	| stackSet |
	self clientData ifNil: [^ self].
	stackSet := IdentityDictionary new.
	
	"migrate any compiler state"
	(self clientData select: [:ea | ea class == stateClass]) asArray do: [:state || newState |
		newState := newStateClass new.
		stateClass instVarNames withIndexDo: [:name :idx || var |
			var := state instVarAt: name.
			var class == newStackClass
				ifTrue: [newState instVarNamed: name put: (stackSet at: var ifAbsentPut: [(newStackClass new: var size)
																							addAll: var;
																							yourself])]
				ifFalse: [newState instVarNamed: name put: var]].
		self clientData at: (self clientData indexOf: state) put: newState].
	
	"migrate any compiler stacks"
	(self clientData select: [:ea | ea class == stackClass]) asArray do: [:stack |
		self clientData
			at: (self clientData indexOf: stack)
			put: (stackSet at: stack ifAbsentPut: [(newStackClass new: stack size)
													addAll: stack;
													yourself])].


