printing
printSourceOn: aStream
	aStream
		cr;
		nextPutAll: 'class ';
		printNode: cpath.
	superNode ifNotNil:
		[aStream
			nextPutAll: ' < ';
			printNode: superNode].
	aStream indentAndEnd: bodyNode

