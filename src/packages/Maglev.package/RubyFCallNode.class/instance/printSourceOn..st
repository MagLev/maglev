printing
printSourceOn: aStream
	aStream nextPutAll: self name.
	aStream nextPut: $(.
	argsNode ifNotNil: [argsNode printArgsOn: aStream].
	aStream nextPut: $).
	aStream printNode: iterNode

