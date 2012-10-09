*maglev-runtime
irYieldStarNode
      "ruby_selector_suffix dependent"
  | tst nd rcv tmpLeaf toA  |
  (nd := node ) isNilNode ifTrue:[
     ^ nd irYieldStarNode  "we have nil literal from Ruby parser"
  ].
  rcv := GsComAssignmentNode _basicNew dest: (tmpLeaf := evalTmp leaf ) source: nd irNode .
  self ir: rcv .
  (toA := GsComSendNode new) 
     rcvr: (GsComVariableNode new leaf: tmpLeaf ) ;
     rubySelector:  #'to_a#0__'  .
  self ir: toA .
  (tst := GsComSendNode new) 
     rcvr:  rcv  ;
     stSelector:  #ifNil:ifNotNil:  ;
     appendArgument:(     self irInlineBlockIr: (RubyNilNode newForIr irYieldStarNode)) ;
     appendArgument:(  self irInlineBlockIr: toA  ) ;
     optimize .
  ^ self ir: tst 

