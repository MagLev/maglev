*maglev-runtime
< other
	self with: other do:
		[:a :b | 
		a < b ifTrue: [^ true].
		b < a ifTrue: [^ false]].
	^ false

