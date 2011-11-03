
set class RubyFlipNode
category: 'as yet unclassified'
method:
first: aNode second: nodeTwo isDot3: aBool
  firstNode := RubyBlockNode s_a: aNode .
  secondNode := RubyBlockNode s_a: nodeTwo .
  isDot3 := aBool

%


set class RubyFlipNode
category: '*maglev-runtime'
method:
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

%


set class RubyFlipNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
  | mth scp |
  mth := RubyCompilerState current topMethodDef .
  scp := mth scope .
  firstTmp := scp newEvaluationTemp .
  firstNode walkWithScope: aScope .
  secondNode walkWithScope: aScope .

%


set class RubyFlipNode
category: '*maglev-runtime'
method:
_inspect
  | dots |
  dots := isDot3 ifTrue:[ '...' ] ifFalse:[ '..' ].
  ^ '[:flip, ', firstNode _inspect, ', ', dots ,  ' , ', secondNode _inspect , $]

%

