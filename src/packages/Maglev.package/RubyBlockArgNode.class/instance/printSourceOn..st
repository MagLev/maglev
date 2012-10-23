printing
printSourceOn: aStream
	(((aStream contents last: 2) = ', ') or: [aStream contents last = '(']) ifFalse:
		[aStream nextPutAll: ', '].
	aStream nextPutAll: '&', name asString

