parsetree
restArgForTokens: tokens rest: rest opt: opt
	^ rest 
		ifNil: [-1] 
		ifNotNil: [tokens size + (opt ifNil: [0] ifNotNil: [opt list size])]

