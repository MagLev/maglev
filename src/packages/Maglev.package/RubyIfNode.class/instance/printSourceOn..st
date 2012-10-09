printing
printSourceOn: aStream
	aStream
		nextPutAll: 'if(';
		printNode: condition;
		nextPutAll: ')';
		indent: [aStream cr; printNode: thenBody].
	elseBody ifNotNil:
		[aStream cr; nextPutAll: 'else'; indent: [aStream cr; printNode: elseBody]].
	aStream cr; nextPutAll: 'end'

