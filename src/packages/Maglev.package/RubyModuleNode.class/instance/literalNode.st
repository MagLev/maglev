converting
literalNode
	^ self ir: (GsComLiteralNode new leaf: (self ir: (GsComLitLeaf new methodLiteral: bodyNode )))

