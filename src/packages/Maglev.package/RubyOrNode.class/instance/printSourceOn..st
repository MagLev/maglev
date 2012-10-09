printing
printSourceOn: aStream
	aStream
		printNode: firstNode;
		nextPutAll: ' || ';
		printNode: secondNode

