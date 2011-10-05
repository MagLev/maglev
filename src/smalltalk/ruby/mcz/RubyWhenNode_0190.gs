
set class RubyWhenNode class
category: '*maglev-ast'
method:
s_a: expr b: body c: nxt d: srcOfs
 | res |
  (res := self _basicNew)
     init_a: expr b: body c: nxt d: srcOfs .
  ^ res

%


set class RubyWhenNode
category: 'accessing'
method:
bodyNode

	 ^ bodyNode

%


set class RubyWhenNode
category: 'accessing'
method:
bodyNode: aNode
	bodyNode := aNode

%


set class RubyWhenNode
category: 'accessing'
method:
expressionNodes

	 ^ expressionNodes

%


set class RubyWhenNode
category: 'accessing'
method:
expressionNodes: aNode
	expressionNodes := aNode

%


set class RubyWhenNode
category: '*maglev-ast'
method:
init_a: expr b: body c: nxt d: srcOfs
  expressionNodes := expr .
  bodyNode := body .
  nextCase := nxt .
  self position: srcOfs 

%


set class RubyWhenNode
category: 'converting'
method:
irCaseExpressionWithLeaf: aLeaf
	| list first send nxt  |
	list := expressionNodes list .                          
	first := list at: 1.
	send := first irWhenExpressionWithLeaf: aLeaf .  "builds an #===: send or an  #anySatisfy:" 
	self ir: send .
    nxt :=  self irNextCaseExprWithLeaf: aLeaf rcvr: send list: list idx: 2 .
    ^ nxt ifNotNil:[ nxt ] ifNil:[ send ].

%


set class RubyWhenNode
category: 'converting'
method:
irCaseNodeListWithLeaf: aLeaf
	^ { self irCaseNodeWithLeaf: aLeaf }

%


set class RubyWhenNode
category: '*maglev-runtime'
method:
irCaseNodeListWithLeaf: aLeaf into: blockIr
    blockIr appendStatement: ( self irCaseNodeWithLeaf: aLeaf )

%


set class RubyWhenNode
category: '*maglev-runtime'
method:
irCaseNodeWithLeaf: aLeaf
  | send |
  (send := GsComSendNode new )
    rcvr: (self irCaseExpressionWithLeaf: aLeaf ) ;
    stSelector:  #ifTrue:ifFalse:   ;
    appendArgument: (bodyNode irBlockNodeInline: self) ;
    appendArgument:
      (self newInlineBlock:
	[:block |    "nextCase is the next WhenNode  within the CaseNode "
	  nextCase irCaseNodeListWithLeaf: aLeaf into: block .
          block] ) ;
        optimize .
    ^ send

%


set class RubyWhenNode
category: '*maglev-runtime'
method:
irNextCaseExprWithLeaf: aLeaf rcvr: aSend list: list idx: idx
  | nextSnd |  
  idx > list size ifTrue:[ ^ nil ].  

  (nextSnd := GsComSendNode new)
     rcvr: aSend ;
     stSelector:  #or:  ;
     appendArgument:( 
          self newInlineBlock:[ :blk | | eqeqeqSend nxt  |
            eqeqeqSend := (list at: idx) irWhenExpressionWithLeaf: aLeaf . 
            blk appendStatement: eqeqeqSend .
            nxt := self irNextCaseExprWithLeaf: aLeaf rcvr: eqeqeqSend list: list idx: idx + 1.
            nxt ifNotNil:[ blk appendStatement: nxt ].
            blk 
          ] ) .
   nextSnd optimize . 
   self ir: nextSnd .
   ^ nextSnd 

%


set class RubyWhenNode
category: '*maglev-runtime'
method:
irWhenExpressionWithLeaf: aLeaf
   "used to handle  *list  form of argument to a   when    keyword"
      "ruby_selector_suffix dependent"
   | node arrBuilder |
  arrBuilder := expressionNodes irNode .
  self ir: arrBuilder .
  (node := GsComSendNode new)  rcvr: arrBuilder.
  aLeaf ifNotNil:[
      node    rubySelector:  #'__anySatisfyCaseLeaf#1__'   ;
        appendArgument: (GsComVariableNode new leaf: aLeaf) 
  ] ifNil:[
      node rubySelector: #'__anySatisfyCaseTrue#0__' 
  ].
  self ir: node .
  ^ node 

%


set class RubyWhenNode
category: 'accessing'
method:
nextCase

	 ^ nextCase

%


set class RubyWhenNode
category: 'accessing'
method:
nextCase: aNode
	nextCase := aNode

%


set class RubyWhenNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
  bodyNode walkWithScope: aScope .
  nextCase walkWithScope: aScope .
  expressionNodes  walkWithScope: aScope

%


set class RubyWhenNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:when, ' , expressionNodes _inspect, ', ', bodyNode _inspect , '
<nxtWhen> ', nextCase _inspect  , $]

%

