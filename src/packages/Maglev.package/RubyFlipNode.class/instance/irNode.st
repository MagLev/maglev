*maglev-runtime
irNode 
      "ruby_selector_suffix dependent"
  | leaf initSnd node |
  leaf := firstTmp leaf  .
  (initSnd := GsComSendNode new)
     rcvr: (GsComLiteralNode newObject: RubyFlipFlop) ;
     stSelector: #init: ;
     appendArgument: ( GsComVariableNode new leaf:  leaf ).
  self ir: initSnd .
  (node := GsComSendNode new)
     rcvr: (GsComAssignmentNode _basicNew dest: leaf  source: initSnd ) ;
     rubySelector: ( isDot3 ifTrue:[ #'__update_from_to3#2__' ] 
                        ifFalse:[ #'__update_from_to#2__']);
     appendArgument: firstNode irNode ;
     appendArgument: secondNode irNode .
  self ir: node .
  ^ node 

