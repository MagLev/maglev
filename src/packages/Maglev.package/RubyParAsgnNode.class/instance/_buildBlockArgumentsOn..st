*maglev-ast
_buildBlockArgumentsOn: irBlock
  | lList  |
  rightList ifNotNil:[ self error:'invalid RHS on ParAsgn as block args'].
  lList := leftList .
  1 to: lList size do:[:n | (lList at: n) buildBlockArgumentsOn: irBlock ].
  

