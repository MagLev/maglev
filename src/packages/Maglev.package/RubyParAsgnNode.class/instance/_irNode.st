*maglev-runtime
_irNode
  | stms rz lsz lList res rsz tmps  | 
  rsz := rightList size . lList := leftList . lsz := lList size .  tmps := evalTemps . 
  stms := self emitFirstNRhs: rsz leftSize: lsz .
  1 to: lsz do:[:k | | ass val  |
     val :=  k <= rsz ifTrue:[ GsComVariableNode new leaf: (tmps at: k) leaf]
                     ifFalse:[ GsComLiteralNode newNil ].
     ass := ( lList at: k) irAssignmentNode: val .
     stms add: ass .
  ].
  res := GsComArrayBuilderNode new .
  1 to: rsz do:[:m |
     res appendElement: ( GsComVariableNode new leaf: (tmps at: m) leaf ) 
  ].
  stms add: res .
  ^ GsComStatementsNode new list: stms 

