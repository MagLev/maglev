*maglev-ast
init_a: rcv b: asgnSelTok c: opSelTok d: val
  |  asgnSel varAsgnStr |
  operatorCallName := opSelTok symval .
  asgnSel := asgnSelTok symval .
  variableCallName := asgnSel .

  (varAsgnStr := String withAll: asgnSel) add: $= .
  variableAsgnCallName := varAsgnStr asSymbol .

  receiverNode := rcv .
  valueNode := val .
  self position: asgnSelTok src_offset  

