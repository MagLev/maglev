*maglev-runtime
buildBlockArgumentsOn: irBlock
  | argLeaf loc |
  isBlockArg ifFalse:[ self error:'inconsistent isBlockArg in LocalAsgn'].
  (argLeaf := (loc := location) leaf) ifNotNil:[ 
    irBlock appendArg: argLeaf .
    hasAmpersand ifNotNil:[ irBlock setAmpersandArg ].        
  ] ifNil:[  | n asgnNod | "assignment to a for loop variable in an eval"
    n := irBlock numArgs + 1 .
    (argLeaf := GsComVarLeaf new)
       blockArg:  name  argNumber: n forBlock: irBlock.
    irBlock appendArg: argLeaf .
    asgnNod := loc irAssignmentNode: ( GsComVariableNode new leaf: argLeaf ) .
    irBlock appendStatement: asgnNod
  ]

