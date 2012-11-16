printing
printSourceOn: aStream
	aStream
		nextPutAll: 'def ';
		printNode: nameNode;
		parenthesize: argsNode;
		indentAndEnd: bodyNode

