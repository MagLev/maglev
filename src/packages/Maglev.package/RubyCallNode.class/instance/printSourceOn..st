printing
printSourceOn: aStream
	aStream
		printNode: receiverNode;
		nextPutAll: (self name asSymbol isInfix ifTrue: [''] ifFalse: ['.']);
		nextPutAll: self name;
		nextPutAll: '('.
		argsNode ifNotNil: [argsNode printArgsOn: aStream].
		aStream nextPutAll: ')';
		printNode: iterNode
	

