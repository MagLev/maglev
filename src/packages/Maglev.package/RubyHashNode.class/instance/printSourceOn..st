printing
printSourceOn: aStream
	aStream
		nextPutAll: '{';
		printNode: listNode;
		nextPutAll: '}'

