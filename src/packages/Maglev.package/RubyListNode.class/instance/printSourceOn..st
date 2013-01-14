printing
printSourceOn: aStream
	list do: [:ea | aStream printNode: ea] separatedBy: [aStream nextPutAll: ', ']

