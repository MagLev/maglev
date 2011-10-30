
set class RubyContinuation
category: 'as yet unclassified'
method:
ccCall: value

	^ process value: value

%


set class RubyContinuation
category: 'as yet unclassified'
method:
clientData
	"Returns the stored clientData as a runnable copy"
	clientData ifNil: [^ nil].
	^ self
		_convertedClientData: clientData
		from: RubyPersistableCompilerState
		and: RubyPersistableCompilerStack
		to: RubyCompilerState
		and: RubyCompilerStack
%


set class RubyContinuation
category: 'as yet unclassified'
method:
clientData: anArray
	"Stores the passed clientData in a persistable fashion"
	clientData := anArray
		ifNotNil: [self
					_convertedClientData: anArray
					from: RubyCompilerState
					and: RubyCompilerStack
					to: RubyPersistableCompilerState
					and: RubyPersistableCompilerStack]
%


set class RubyContinuation
category: 'as yet unclassified'
method:
continuation: aGsProcess

	process := aGsProcess

%


set class RubyContinuation
category: 'private'
method:
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
%

