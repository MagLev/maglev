as yet unclassified
irConditionNode
  ^ self newBlock:[:block |
		block appendStatement: conditionNode irEvaluatedRcvrNode.
		block 
		]
	   isInline: blksInline

