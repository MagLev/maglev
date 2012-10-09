*maglev-runtime
irAssignmentNode: srcVarNode 
    "see documentation about srcVarNode in superclass"
  |  send  ass stms lList lsiz |
  lList := leftList . lsiz := lList size .

  stms := self irAssignmentNonStar: srcVarNode size: lsiz - 1 .
  (send := GsComSendNode new)
      rcvr: (GsComVariableNode new leaf: (evalTemps at:1) leaf );  
      stSelector:  #_parAsgnCopyFrom:  ;
      appendArgument: (GsComLiteralNode newInteger: lsiz) .
  ass := (lList at: lsiz) irAssignmentNode: (self ir: send) .
  stms add: ass .

  ^ GsComStatementsNode new list: stms 

