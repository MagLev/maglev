as yet unclassified
hasInnerParAssign
  | lList |
  1 to: (lList := leftList)  size do:[:n |
	  ( lList at: n) isParAssignNode ifTrue:[  ^ true ]
  ].  
  ^ false

