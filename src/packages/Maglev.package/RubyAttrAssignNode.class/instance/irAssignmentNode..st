as yet unclassified
irAssignmentNode: srcVarNode 
  "used by IR phase of RubyParAsgnNode "			
  srcIrNode ifNotNil:[ self error: 'inconsistent AST tree'].
  srcIrNode := srcVarNode .

  ^ self irNode

