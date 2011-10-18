
set class RubyContinuation class
category: 'as yet unclassified'
method:
with: aGsProcess

	^ self basicNew
		continuation: aGsProcess;
		yourself

%


set class RubyContinuation
category: 'as yet unclassified'
method:
ccCall: value

	^ process value: value

%


set class RubyContinuation
category: 'as yet unclassified'
method:
continuation: aGsProcess

	process := aGsProcess

%

