as yet unclassified
indentAndEnd: aNode
	self indent: [self printNode: aNode].
	self cr; nextPutAll: 'end'

