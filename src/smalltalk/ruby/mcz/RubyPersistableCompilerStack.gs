
set class RubyPersistableCompilerStack
category: 'as yet unclassified'
classmethod:
newFrom: aStack

	^ (self new: aStack size)
		addAll: aStack;
		yourself
%

