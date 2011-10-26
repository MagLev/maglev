
set class SequenceableCollection
category: '*maglev-runtime'
method:
< other
	self with: other do:
		[:a :b | 
		a < b ifTrue: [^ true].
		b < a ifTrue: [^ false]].
	^ false

%


set class SequenceableCollection
category: '*maglev-runtime'
method:
<= other
	^ self = other or: [self < other]

%

