converting
irNode  
	^ self ir:
		(GsComAssignmentNode _basicNew
			dest: self irLeaf
			source:  valueNode irEvaluatedBlockNode )

