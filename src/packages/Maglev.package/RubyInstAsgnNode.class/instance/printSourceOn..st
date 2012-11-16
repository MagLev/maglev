printing
printSourceOn: aStream
	aStream
		nextPutAll: name;
		nextPutAll: ' = ';
		printNode: valueNode

