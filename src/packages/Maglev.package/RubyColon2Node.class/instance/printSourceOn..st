printing
printSourceOn: aStream
	leftNode ifNotNil:
		[aStream
			printNode: leftNode;
			nextPutAll: '::'].
	aStream nextPutAll: name

