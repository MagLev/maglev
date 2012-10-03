as yet unclassified
irAssignmentNode: srcVarNode 
  isDynamic ifFalse:[ ^ super irAssignmentNode: srcVarNode ].
  ^ constNode irForDynamicCDecl: srcVarNode

