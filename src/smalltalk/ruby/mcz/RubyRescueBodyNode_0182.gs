
set class RubyRescueBodyNode
category: '*maglev-ast'
classmethod:
s_a: ex_list b: body c: next_rescue d: srcOfs 
 | res |
  (res := self _basicNew)
    init_a: ex_list b: body c: next_rescue d: srcOfs .
  ^ res

%


set class RubyRescueBodyNode
category: 'accessing'
method:
bodyNode

	 ^ bodyNode

%


set class RubyRescueBodyNode
category: 'accessing'
method:
bodyNode: aNode
	bodyNode := aNode

%


set class RubyRescueBodyNode
category: 'accessing'
method:
exceptionNodes

	 ^ exceptionNodes

%


set class RubyRescueBodyNode
category: 'accessing'
method:
exceptionNodes: aNode
	exceptionNodes := aNode

%


set class RubyRescueBodyNode
category: '*maglev-ast'
method:
init_a: ex_list b: body c: next_rescue d: srcOfs 
  exceptionNodes := ex_list .
  bodyNode := body .
  nextRescueBody := next_rescue .
  self position: srcOfs

%


set class RubyRescueBodyNode
category: '*maglev-runtime'
method:
irBodyNode
^ self newBlock: [:block| | exLeaf send  exStack bdy |
  [ exStack := RubyCompilerState current lastExceptionStack .
    exLeaf := GsComVarLeaf new blockArg: #__ex argNumber: 1 forBlock: block.
    exStack push: exLeaf . 
    block appendArg: exLeaf .
        "ruby thread data at  OC_RubyGsProcessClientData_lastException no longer used"
    (bdy := bodyNode) ifNil:[
       send := GsComLiteralNode newNil returnNode .
    ] ifNotNil:[
      ( send := GsComSendNode new) 
          rcvr: (bdy  irBlockNode: self)  ;
          stSelector: #value .
    ].
    self ir: send .
    block appendStatement: send .
  ] ensure:[
     exStack pop:  exLeaf 
  ].
  block
]

%


set class RubyRescueBodyNode
category: '*maglev-runtime'
method:
irExceptionNode
  | node exList eNodes | 
  (eNodes := exceptionNodes)  ifNil:[ ^ defaultNode irNode].  
  exList := eNodes list . 
  exList size == 0 ifTrue:[   
     eNodes isSplatNode  ifTrue:[
       node := eNodes irNode   " rescue  *aList  " 
     ] ifFalse:[
       self error:'unrecognized argument to "rescue" ' .
     ].
  ] ifFalse:[
      node := ( exList at: 1 ) irNode  .
      2 to: exList size do:[ :n | | ea | 
        ea := exList at: n .
        node := GsComSendNode new rcvr: node ;
            stSelector: #addException:  ;
            appendArgument: ea irNode  .
      self ir: node .
      ].
  ].
  ^ node

%


set class RubyRescueBodyNode
category: 'as yet unclassified'
method:
isRescueBodyNode
  ^ true

%


set class RubyRescueBodyNode
category: 'as yet unclassified'
method:
nextRescueBody
  ^ nextRescueBody

%


set class RubyRescueBodyNode
category: 'as yet unclassified'
method:
nextRescueBody: aNode
   nextRescueBody := aNode

%


set class RubyRescueBodyNode
category: 'printing'
method:
printSourceOn: aStream
	aStream printNode: bodyNode

%


set class RubyRescueBodyNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
  | exn |
  (exn := exceptionNodes) ifNil:[ 
    (defaultNode := RubyColon2Node newForIr name: #StandardError ) walkWithScope: aScope
  ] ifNotNil:[
    exn walkWithScope: aScope .
  ].
  bodyNode walkWithScope: aScope .
  nextRescueBody walkWithScope: aScope .

%


set class RubyRescueBodyNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:resbody, ', exceptionNodes _inspect , ', ', bodyNode _inspect, 
					', ', nextRescueBody _inspect , $]


%

