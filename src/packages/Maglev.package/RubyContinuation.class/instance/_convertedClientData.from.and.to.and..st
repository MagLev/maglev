private
_convertedClientData: array from: oldState and: oldStack to: newState and: newStack
	"Returns the stored clientData as a runnable copy"
	| stacks newData |
	newData := array copy.
	stacks := IdentityDictionary new.

	newData withIndexDo: [:each :idx |
		each class == oldState ifTrue: [
			newData at: idx put: (newState newFrom: each withStacksMap: stacks)].
		each class == oldStack ifTrue: [
			newData at: idx put: (stacks at: each ifAbsentPut: (newStack newFrom: each))]].
	^ newData
