as yet unclassified
irNode  
   isBlockArg ifTrue:[ self error:'irNode illegal for DAsgn as block arg'].
	^ self ir:
		(GsComAssignmentNode _basicNew
			dest: self irLeaf
			source:  valueNode irLocalAsgnValue  )

