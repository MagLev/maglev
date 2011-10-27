
set class RubyInstVarDefinedQNode
category: 'as yet unclassified'
classmethod:
newForIr
  ^ self _basicNew

%


set class RubyInstVarDefinedQNode
category: 'as yet unclassified'
method:
irArgNodes
  ^ { argNode }  "ir args built during ivNode: "

%


set class RubyInstVarDefinedQNode
category: 'as yet unclassified'
method:
irReceiverNode 
  ^  GsComVariableNode newSelf

%


set class RubyInstVarDefinedQNode
category: 'as yet unclassified'
method:
isSmalltalkSend
  ^ true

%


set class RubyInstVarDefinedQNode
category: 'as yet unclassified'
method:
ivNode: instVarNode 
  | accessKind |
  ivNode := instVarNode .
  accessKind := ivNode accessKind .
  accessKind == 0 ifTrue:[ | ivLeaf varOfs  |
	selector :=  #_rubyInstVarDefinedQ:  .
	ivLeaf := instVarNode irLeaf .
	varOfs := ivLeaf varOffset .
	varOfs < 0 ifTrue:[  argNode := instVarNode name asSymbol irLiteralNode "dynamic iv" ]
			   ifFalse:[  argNode := GsComLiteralNode newInteger: varOfs "fixed iv" ].
  ] ifFalse:[
    selector := #rubyInstVarDefined:  .   "instVar in Object or a Behavior"
    argNode := instVarNode name asSymbol irLiteralNode .
  ].

%


set class RubyInstVarDefinedQNode
category: 'as yet unclassified'
method:
selector
  ^ selector

%


set class RubyInstVarDefinedQNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
  " should not exist prior to IR generation pass"
  self error:'RubyInstVarDefinedQNode>>walkWithScope: should not be here'

%

