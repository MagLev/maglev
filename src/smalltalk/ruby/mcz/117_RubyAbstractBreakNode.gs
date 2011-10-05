
doit
RubyAbstractGotoNode subclass: 'RubyAbstractBreakNode'
	instVarNames: #( valueNode walked)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyAbstractBreakNode
removeallmethods
removeallclassmethods

set class RubyAbstractBreakNode class
category: '*maglev-ast'
method:
s_a: val b: srcOfs
  "used for RubyBreakNode, RubyNextNode"
  | res |
  (res := self _basicNew) valueNode: val ; position: srcOfs .
  ^ res

%


set class RubyAbstractBreakNode
category: '(as yet unclassified)'
method:
argNodes
  | val |
  walked := true .
  ^ (val := valueNode) ifNil:[ #() ] ifNotNil:[ { val } ] 

%


set class RubyAbstractBreakNode
category: 'as yet unclassified'
method:
childrenForMatch
	^ { valueNode }

%


set class RubyAbstractBreakNode
category: 'as yet unclassified'
method:
irGotoValueNode
  ^ valueNode ifNil:[ GsComLiteralNode newNil ]
            ifNotNil:[ valueNode irLocalAsgnValue ].

%


set class RubyAbstractBreakNode
category: 'as yet unclassified'
method:
irNode
  |  goto val irLabel targLev  cm loop |
  loop := (cm := RubyCompilerState current) topLoop .
  irLabel := self irGotoTarget: loop  .
  irLabel ifNotNil:[
    targLev := irLabel lexLevel . 
    (cm lexLevelIsInlineWithin: targLev) ifTrue:[
	  (goto := self irGotoNodeFrom: loop to: irLabel) 
       argNode: self irGotoValueNode .
    ].
  ].
  goto ifNil:[ goto := self nonInlineIrNode ].
  self ir: goto . 
  ^ goto

%


set class RubyAbstractBreakNode
category: '*maglev-ast'
method:
is_void_result
  ^ true

%


set class RubyAbstractBreakNode
category: 'accessing'
method:
valueNode

	 ^ valueNode

%


set class RubyAbstractBreakNode
category: 'accessing'
method:
valueNode: aNode
	valueNode := aNode

%


set class RubyAbstractBreakNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
  | loop |
  loop := RubyCompilerState current topLoop .
  loop ifNotNil:[  loop setHasBreakNext ].
  walked ifNil:[
     valueNode walkWithScope: aScope
  ]

%

