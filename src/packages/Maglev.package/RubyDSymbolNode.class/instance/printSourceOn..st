as yet unclassified
printSourceOn: aStream
   aStream nextPutAll: '('. 
   list do: [:ea | aStream printNode: ea] separatedBy: [aStream nextPutAll: ' + '].
   aStream nextPutAll: ')'

