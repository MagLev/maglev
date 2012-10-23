as yet unclassified
irNode
  | rh  |
  rhsToArraySelector := (rh := rightList at: 1)  parAsgnToarySelector .
  ^ rhsIsToAry ifTrue:[ self irAssignmentNode: rh  irNonSplatNode ]
                ifFalse:[ self _irNode ]

