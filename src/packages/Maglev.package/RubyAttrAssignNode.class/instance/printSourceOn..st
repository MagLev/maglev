printing
printSourceOn: aStream
	aStream
		printNode: receiverNode;
		nextPutAll: '.', name allButLast, ' = '.
	argsNode printArgsOn: aStream

