
set class RubySplatNode class
category: '*maglev-ast'
method:
s_a: val 
 | res |
  (res := self _basicNew)
     node: val .
  ^ res

%


set class RubySplatNode
category: 'converting'
method:
adjustForLhsSplat
 
   node class == RubyNilNode ifTrue:[ | n |
	  (n := RubyArrayNode _basicNew ) list: { node  } .
	  node := n .
   ].
   ^ self

%


set class RubySplatNode
category: '(as yet unclassified)'
method:
argNodes
  | n |
  walked := true .
  ^ (n := node ) ifNil:[ #() ] ifNotNil:[ { n } ]

%


set class RubySplatNode
category: '*maglev-runtime'
method:
buildBlockArgumentsOn: irNode

  node ifNil:[  "this path might not be used by MRI AST "
    "A ruby block  { |*|  }  ignores all args; generate as  zero arg block"  
  ] ifNotNil:[
    node buildBlockArgumentsOn: irNode .
    irNode setLastArgStar
  ]

%


set class RubySplatNode
category: '*maglev-runtime'
method:
buildIrLeafsInto: anArray
  node ifNil:[
    "A ruby block  { |*|  }  ignores all args; generate as  zero arg block"  
  ] ifNotNil:[
     node buildIrLeafsInto: anArray
  ]

%


set class RubySplatNode
category: 'converting'
method:
fCallArgNodes
   ^ { self }

%


set class RubySplatNode
category: 'as yet unclassified'
method:
getClearIter
  ^ nil

%


set class RubySplatNode
category: 'converting'
method:
hasRestArg
	^ true

%


set class RubySplatNode
category: '*maglev-runtime'
method:
irArgNode
      "ruby_selector_suffix dependent"
 
    ^ GsComSendNode new
        rcvr: node irNode;
        rubySelector: #'__splat_arg_value#0__'

%


set class RubySplatNode
category: 'as yet unclassified'
method:
irAssignmentNode: srcVarNode
  "expected to be used for ParAsgn with AST from RubyParser only"
  ^ node irAssignmentNode: srcVarNode

%


set class RubySplatNode
category: '*maglev-runtime'
method:
irLocalAsgnValue
      "ruby_selector_suffix dependent"
   | lhsp |
   (lhsp := isLhsSplat)  ifNil:[ | res |  
     (res := GsComSendNode new)
        rcvr: node irNode;
        rubySelector: #'__splat_lasgn_value#0__' .
     ^ self  ir: res
   ]. 
   ^ self irNode 

%


set class RubySplatNode
category: '*maglev-runtime'
method:
irNode
      "ruby_selector_suffix dependent"
 
    ^ GsComSendNode new
        rcvr: node irNode;
        rubySelector: #'to_a#0__' 

%


set class RubySplatNode
category: '*maglev-runtime'
method:
irNonSplatNode
 
    ^  node irNode
       

%


set class RubySplatNode
category: '*maglev-runtime'
method:
irReturnNode
      "ruby_selector_suffix dependent"
   
    ^ GsComSendNode new
        rcvr: node irNode;
        rubySelector: #'__splat_return_value#0__' 

%


set class RubySplatNode
category: '*maglev-runtime'
method:
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

%


set class RubySplatNode
category: 'as yet unclassified'
method:
isEmptySplatNode
  ^ node == nil

%


set class RubySplatNode
category: 'as yet unclassified'
method:
isLhsSplat: aBoolean
  isLhsSplat := aBoolean

%


set class RubySplatNode
category: 'as yet unclassified'
method:
isSplatNode
  ^ true

%


set class RubySplatNode
category: 'converting'
method:
list
	^ #()

%


set class RubySplatNode
category: 'accessing'
method:
node

	 ^ node

%


set class RubySplatNode
category: 'accessing'
method:
node: aNode 
   | n | 
   n := aNode .
   isLhsSplat ifNotNil:[ aNode ifNotNil:[  n := aNode adjustForLhsSplat ]].
   node := n

%


set class RubySplatNode
category: 'as yet unclassified'
method:
parAsgnToarySelector
  ^ #__par_asgn_star_to_ary

%


set class RubySplatNode
category: 'printing'
method:
printArgsOn: aStream
	aStream
		nextPutAll: '*';
		printNode: node

%


set class RubySplatNode
category: 'as yet unclassified'
method:
setIsBlockArg
  | n |
  (n := node) ifNotNil:[ n setIsBlockArg ].

%


set class RubySplatNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
  evalTmp := aScope newEvaluationTemp .
  walked ifNil:[ 
    walked := true .
    node walkWithScope: aScope  .
  ].

%


set class RubySplatNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:splat, ', node _inspect , $]

%

