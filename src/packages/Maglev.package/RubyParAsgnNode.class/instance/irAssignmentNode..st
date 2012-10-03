as yet unclassified
irAssignmentNode: srcVarNode 
  "for the send from RubyIterNode buildArgumentsOn:  srcVarNode will be a GsComVarLeaf,
   otherwise it should be  a GsComVariableNode "
 
  ^ GsComStatementsNode new 
       list: (self irAssignmentNonStar: srcVarNode size: leftList size )

