
doit
RubyAbstractCallNode subclass: 'RubyDynamicDefinedQNode'
	instVarNames: #( litObj selector)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyDynamicDefinedQNode
removeallmethods
removeallclassmethods

set class RubyDynamicDefinedQNode class
category: 'as yet unclassified'
method:
newForIr
  ^ self _basicNew

%


set class RubyDynamicDefinedQNode
category: 'as yet unclassified'
method:
irArgNodes 
  ^ {  GsComVariableNode newSelf  }

%


set class RubyDynamicDefinedQNode
category: 'as yet unclassified'
method:
irReceiverNode
  ^ self ir:( GsComLiteralNode new leaf:(
       GsComLitLeaf new deferredGlobalLiteral: litObj )) 

%


set class RubyDynamicDefinedQNode
category: 'as yet unclassified'
method:
isSmalltalkSend
  ^ true

%


set class RubyDynamicDefinedQNode
category: 'as yet unclassified'
method:
literalObj: anObject 

  litObj := anObject

%


set class RubyDynamicDefinedQNode
category: '*maglev-runtime'
method:
selector
  ^ #definedQconst

%


set class RubyDynamicDefinedQNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
  " should not exist prior to IR generation pass"
  self error:'RubyDynamicDefinedQNode>>walkWithScope: should not be here'

%

