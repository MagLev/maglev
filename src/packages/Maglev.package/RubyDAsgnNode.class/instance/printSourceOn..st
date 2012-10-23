printing
printSourceOn: aStream
	aStream nextPutAll: name.
	valueNode ifNotNil:
		[aStream nextPutAll: ' = '; printNode: valueNode]

