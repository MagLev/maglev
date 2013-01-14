*maglev-runtime
_irNode
  | rList rsz lList lsz  nonStarSz stms n starBld tmps nsRes lastLeaf |
  rList := rightList . rsz := rList size . lList := leftList . lsz := lList size .
       tmps := evalTemps .   nonStarSz  := rsz - starSize  .
  stms := self emitFirstNRhs: nonStarSz  .
  n := nonStarSz  + 1 .
  starBld := GsComArrayBuilderNode new .  
  true ifTrue:[ | lim |   "Fix Trac629"
     [ n < rsz ] whileTrue:[ starBld appendElement: (rList at: n) irNode . n := n + 1 ].
     (n <= rsz ) ifTrue:[ starBld appendElement: (rList at: n) irNonSplatNode . n := n + 1 ].
  ] ifFalse:[
    [ n <= rsz ] whileTrue:[ starBld appendElement: (rList at: n) irNode . n := n + 1 ].
  ].  
  stms add:( self ir:( GsComAssignmentNode _basicNew dest: (lastLeaf := tmps last leaf) 
						source: starBld)).
  1 to: lsz do:[:k | | val ass  |
     val :=  k <= nonStarSz ifTrue:[ GsComVariableNode new leaf: (tmps at: k) leaf ]
              ifFalse:[ k < lsz ifTrue:[ GsComLiteralNode newNil ]
                              ifFalse:[ GsComVariableNode new leaf:  lastLeaf ]].
     ass := (lList at: k) irAssignmentNode: val .
     stms add: ass .
  ].
  nsRes := GsComArrayBuilderNode new .
  1 to: nonStarSz do:[:m |
     nsRes appendElement: ( GsComVariableNode new leaf: (tmps at: m) leaf ) 
  ]. 
  rsz > nonStarSz ifTrue:[ | send |
     (send := GsComSendNode new)
       rcvr: nsRes ;      
       stSelector:  #_rubyAddAll:  ;
       appendArgument: (  GsComVariableNode new leaf: lastLeaf  ) .
     stms add:( self ir: send) .
  ] ifFalse:[     
      stms add: nsRes 
  ].
  ^ GsComStatementsNode new list: stms 

