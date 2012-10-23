converting
irCaseExpressionWithLeaf: aLeaf
	| list first send nxt  |
	list := expressionNodes list .                          
	first := list at: 1.
	send := first irWhenExpressionWithLeaf: aLeaf .  "builds an #===: send or an  #anySatisfy:" 
	self ir: send .
    nxt :=  self irNextCaseExprWithLeaf: aLeaf rcvr: send list: list idx: 2 .
    ^ nxt ifNotNil:[ nxt ] ifNil:[ send ].

