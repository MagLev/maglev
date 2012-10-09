*maglev-runtime
irAssignmentNonStar: srcVarNode  size: nonStarCount  
      "ruby_selector_suffix dependent"
  |  send tmpLeaf ass stms lList  |
  srcVarNode class == GsComVarLeaf ifTrue:[  "parallel assign for a complicated ruby block arg"
    tmpLeaf := srcVarNode .  "to_ary coercion not needed"
    stms := { } .  
  ] ifFalse:[  
    tmpLeaf := (evalTemps at: 1) leaf .
    (send := GsComSendNode new)
       rcvr: srcVarNode ;  
       rubySelector: ((rhsToArraySelector ifNil:[ #__par_asgn_to_ary ]) _asSymbolWithRubySuffix: 16r0) . 
    ass := GsComAssignmentNode _basicNew dest: tmpLeaf  source: (self ir: send) .
    stms := { ass } . 
  ].
  lList := leftList .
  1 to: nonStarCount  do:[:n | 
     (send := GsComSendNode new) 
        rcvr: (GsComVariableNode new leaf: tmpLeaf) ;  
        stSelector:  #atOrNil:  ;
        appendArgument: (GsComLiteralNode newInteger: n ).
     ass := ( lList at: n)  irAssignmentNode: ( self ir: send) .
     stms add: ass .
  ].
  ^  stms 

