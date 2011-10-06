
set class RubyIfNode
category: '*maglev-ast'
classmethod:
s_a: cond b: tBlock c: fBlock
 | res |
 (res := self _basicNew)
   init_a: cond b: tBlock c: fBlock .
 ^ res

%


set class RubyIfNode
category: '(as yet unclassified)'
method:
argNodes
  | t e  |
  t := thenBody .
  e := elseBody .
  ^ t ifNil:[
      e ifNil:[ #() ] ifNotNil:[ { e } ]
    ] ifNotNil:[
      e ifNil:[ { t } ] ifNotNil:[ { t . e } ]
    ]

%


set class RubyIfNode
category: 'accessing'
method:
condition

	 ^ condition

%


set class RubyIfNode
category: 'accessing'
method:
condition: aNode
	condition := aNode

%


set class RubyIfNode
category: 'accessing'
method:
elseBody

	 ^ elseBody

%


set class RubyIfNode
category: 'accessing'
method:
elseBody: aNode
	elseBody := aNode

%


set class RubyIfNode
category: '*maglev-ast'
method:
init_a: cond b: tBlock c: fBlock
  condition := cond .
  thenBody := tBlock .
  elseBody := fBlock .

%


set class RubyIfNode
category: 'converting'
method:
irArgNodes
	^ elseBody 
	     ifNil: [  { thenBody irBlockNodeInline: self  } ] 
	     ifNotNil:[ 
		    thenBody  ifNil: [  { elseBody irBlockNodeInline: self  }  ]
			    ifNotNil: [  { thenBody irBlockNodeInline: self . 
				              elseBody irBlockNodeInline: self  } 
				          ] 
		  ]

%


set class RubyIfNode
category: '*maglev-runtime'
method:
irReceiverNode
  ^ condition irEvaluatedRcvrNode

%


set class RubyIfNode
category: 'converting'
method:
isSmalltalkSend
	^ true

%


set class RubyIfNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		nextPutAll: 'if(';
		printNode: condition;
		nextPutAll: ')';
		indent: [aStream cr; printNode: thenBody].
	elseBody ifNotNil:
		[aStream cr; nextPutAll: 'else'; indent: [aStream cr; printNode: elseBody]].
	aStream cr; nextPutAll: 'end'

%


set class RubyIfNode
category: 'converting'
method:
receiverNode
	^ condition

%


set class RubyIfNode
category: 'converting'
method:
selector
	^ elseBody ifNil: [ #ifTrue: ] 
	        ifNotNil: [  thenBody ifNil: [ #ifFalse: ] ifNotNil: [ #ifTrue:ifFalse: ] ]

%


set class RubyIfNode
category: 'converting'
method:
shouldOptimize
	^ true

%


set class RubyIfNode
category: 'accessing'
method:
thenBody

	 ^ thenBody

%


set class RubyIfNode
category: 'accessing'
method:
thenBody: aNode
	thenBody := aNode

%


set class RubyIfNode
category: '*maglev-runtime'
method:
walkCallArgs: lst withScope: aScope
  | nargs |
  1 to: (nargs := lst size) - 1 do:[:n |
    (lst at: n) walkWithScope: aScope .
  ].
  nargs ~~ 0 ifTrue:[ | lastarg |
    (lastarg := lst at: nargs) walkWithScope: aScope .
    lastarg postWalkForYield  "does not need to_proc conversion"
  ].
  condition postWalkForYield .  "rcvr does not need to_proc"

%


set class RubyIfNode
category: '*maglev-runtime'
method:
_inspect
 ^ '[:if, ', condition _inspect, ', ',  thenBody _inspect, ', ', elseBody _inspect , $]

%

