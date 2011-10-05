
doit
RubyAbstractCallNode subclass: 'RubyDotNode'
	instVarNames: #( exclusive beginNode endNode
	                  beginTmp endTmp fixNumCount isInline)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDotNode
removeallmethods
removeallclassmethods

set class RubyDotNode class
category: '*maglev-ast'
method:
s_a: numDots b: left c: right d: srcOfs
  | res |
  (res := self _basicNew )
     init_a: numDots b: left c: right d: srcOfs .
  ^ res

%


set class RubyDotNode
category: '(as yet unclassified)'
method:
argNodes
  | s |
  ^ (s := endNode) ifNil:[ #() ] ifNotNil:[ { s } ]

%


set class RubyDotNode
category: '*maglev-ast'
method:
as_cond
  | res |
  "omitted   env[label] = :lvar   logic  from Ryan's grammar "
  (res := RubyFlipNode _basicNew) 
     first: beginNode second: endNode isDot3: exclusive ;
      position:  position .
  ^ res

%


set class RubyDotNode
category: 'accessing'
method:
beginNode

	 ^ beginNode

%


set class RubyDotNode
category: 'accessing'
method:
beginNode: aNode
	beginNode := aNode

%


set class RubyDotNode
category: 'accessing'
method:
endNode

	 ^ endNode

%


set class RubyDotNode
category: 'accessing'
method:
endNode: aNode
	endNode := aNode

%


set class RubyDotNode
category: 'accessing'
method:
exclusive

	 ^ exclusive

%


set class RubyDotNode
category: 'accessing'
method:
exclusive: a
	exclusive := a

%


set class RubyDotNode
category: '*maglev-ast'
method:
init_a: numDots b: left c: right d: srcOfs
  exclusive := numDots == 3 .
  beginNode := left .
  endNode := right .
  self position: srcOfs .

%


set class RubyDotNode
category: '*maglev-runtime'
method:
irForEndNode
   | en |
   isInline ifFalse:[ self error:'inconsistent dot3 inline'].
   en := endNode irNode .
   endTmp ifNotNil:[ 
      en := GsComAssignmentNode _basicNew dest:(endTmp leaf) source: en .
      self ir: en .
   ].
   ^ exclusive 
        ifFalse: [ en]
        ifTrue:[ | send |
            (send := GsComSendNode new)
                rcvr:  en ;
                stSelector:  #-  ;
                appendArgument:
                    (GsComLiteralNode new leaf: (GsComLitLeaf new integerLiteral: 1)).
            self ir: send .
            send
        ]

%


set class RubyDotNode
category: '*maglev-runtime'
method:
irForNode
    | node bg en   |
    isInline ifFalse: [ ^ super irForNode].
    bg := beginNode irNode .
    beginTmp ifNotNil:[ 
      bg := GsComAssignmentNode _basicNew dest:(beginTmp leaf) source: bg .
      self ir: bg .
    ].
     (node := GsComSendNode new)
            rcvr: bg ;
            stSelector:  #'to:do:' ;
            appendArgument: self irForEndNode ;
            optimize .
     self ir: node .
     ^ node

%


set class RubyDotNode
category: 'converting'
method:
irForNodeWillBeInline
 
    isInline ifTrue:[ ^ true ].
	^ super irForNodeWillBeInline .

%


set class RubyDotNode
category: '*maglev-ast'
method:
irIterResult 
  | bg en res |
  fixNumCount == 2 ifTrue:[
     ^ self irNode "a Range literal"
  ].
  bg := beginTmp ifNil:[ GsComLiteralNode newInteger: beginNode _value ]
                  ifNotNil:[ GsComVariableNode new leaf: beginTmp leaf ].
  en := endTmp ifNil:[ GsComLiteralNode newInteger: endNode _value ]
                  ifNotNil:[ GsComVariableNode new leaf: endTmp leaf ].
  (res := GsComSendNode new) 
      rcvr:  bg ; 
      stSelector:  self selector ;
      appendArgument:  en .
  self ir: res .
  ^ res 

%


set class RubyDotNode
category: '*maglev-ast'
method:
irNode 
  | res val |
  fixNumCount == 2 ifTrue:[
    val := exclusive ifTrue:[ Range from: beginNode _value limit: endNode _value ]
                  ifFalse:[  Range from: beginNode _value to: endNode _value ].
    val immediateInvariant .
    res := GsComLiteralNode newObject: val .
  ] ifFalse:[
    res := super irNode 
  ].
  ^ res 

%


set class RubyDotNode
category: 'as yet unclassified'
method:
isSameAs: other
	^ other exclusive = self exclusive

%


set class RubyDotNode
category: 'converting'
method:
isSmalltalkSend
	^ true

%


set class RubyDotNode
category: 'converting'
method:
receiverNode
	^ beginNode

%


set class RubyDotNode
category: 'converting'
method:
selector
	^ exclusive ifTrue: [#'_rubyTo_:'] ifFalse: [#'_rubyTo:']

%


set class RubyDotNode
category: 'as yet unclassified'
method:
setForLoopNotInline 
  isInline := false

%


set class RubyDotNode
category: 'as yet unclassified'
method:
walkChildrenWith: aScope
  beginNode walkWithScope: aScope .
  endNode walkWithScope: aScope .

%


set class RubyDotNode
category: '(as yet unclassified)'
method:
walkIterWithScope: aScope
  | cnt fixCls |
  cnt := 0 .
  beginNode class == (fixCls := RubyFixnumNode) ifTrue:[ cnt := 1 ]
                                ifFalse:[ beginTmp := aScope newEvaluationTemp ].
  endNode class == fixCls ifTrue:[ cnt := cnt + 1 ]
                            ifFalse:[ endTmp := aScope newEvaluationTemp ].
  fixNumCount := cnt .
  isInline := cnt > 0 .
  super walkWithScope: aScope   "beginNode is receiver, endNode is args "

%


set class RubyDotNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
  | cnt fixCls |
  cnt := 0 .
  beginNode class == (fixCls := RubyFixnumNode) ifTrue:[ cnt := 1 ].
  endNode class == fixCls ifTrue:[ cnt := cnt + 1 ].
  fixNumCount := cnt .
  super walkWithScope: aScope   "beginNode is receiver, endNode is args "

%


set class RubyDotNode
category: '*maglev-runtime'
method:
_inspect
  | sym |
  sym := exclusive ifTrue:[ ':dot3' ] ifFalse:[ ':dot2' ].
  ^ '[' , sym , ', ' , beginNode _inspect , ', ', endNode _inspect, $]

%

