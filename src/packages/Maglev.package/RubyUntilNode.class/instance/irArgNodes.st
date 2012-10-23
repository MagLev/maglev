as yet unclassified
irArgNodes
  
	^ {  blksInline ifTrue:[ self irConditionNode ]
		             ifFalse:[ self irBodyNode ] 
		 }

