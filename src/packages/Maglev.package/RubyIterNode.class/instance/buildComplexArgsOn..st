*maglev-runtime
buildComplexArgsOn: irBlock
  | maNod |
  (maNod := multArgsNode )   ifNotNil:[ | irArgs bld send |
         "argNod is a ParAssignSTar  ; maNode has inner ParAsgn "
     (irArgs:= irBlock args) size ~~ 2 ifTrue:[ self error: 'wrong size for iter block args'].
      "RHS of the complex args masgn will be    { blkArg1 } _rubyAddAll: blkArgStar2    "
     (bld := GsComArrayBuilderNode new)
       appendElement: (GsComVariableNode new leaf: (irArgs at: 1)) .
     (send := GsComSendNode new)
        rcvr: bld ;
        stSelector:  #_rubyAddAll:  ;
        appendArgument: (GsComVariableNode new leaf: (irArgs at:2))   .
     irBlock appendStatement:(
          maNod irAssignmentNode: (self ir: send)  ).
  ]

