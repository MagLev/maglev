parsetree-as yet unclassified
detectMismatchWith: other
	self species = other species ifFalse: [^ {self. other}].
	(self isSameAs: other) ifFalse: [^ {self. other}].
	self childrenForMatch 
		with: other childrenForMatch 
		do: [:a :b |
		       a == b ifFalse:
			[(a detectMismatchWith: b) ifNotNilDo: [:c | ^ c]]].
	^ nil

