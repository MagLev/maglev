*maglev-ast
detectMismatchWith: aNode
	^ (aNode isKindOf: RubyNilNode)
		ifFalse: [{self. aNode}]

