accessing
irArgNodes
	 ^ { self newBlock:
		[:block |
		block appendStatement: secondNode irArgNode.
		block ] }

