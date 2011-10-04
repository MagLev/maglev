
doit
RubyVCallNode subclass: 'RubyVCallRaiseNode'
	instVarNames: #( irArg)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyVCallRaiseNode
removeallmethods
removeallclassmethods

set class RubyVCallRaiseNode
category: 'as yet unclassified'
method:
asCallNodeForIter
  self error: 'block argument to #raise not supported' .
  ^ nil

%


set class RubyVCallRaiseNode
category: 'as yet unclassified'
method:
irArgNodes 
  "return  the leaf for $! , arg to the currently active exception handler block, or none"
  | aLeaf |
  aLeaf := RubyCompilerState current lastExceptionStack topOrNil .
  aLeaf ifNotNil:[ 
	  irArg ifNotNil:[ self error:' irArgNodes being recomputed' ].
	  irArg := GsComVariableNode new leaf: aLeaf .
	  ^ { irArg } 
  ].
  ^ #() 

%


set class RubyVCallRaiseNode
category: 'as yet unclassified'
method:
selector
  irArg ifNotNil:[  ^ #__reraise  ] .  "__reraise implemented in Kernel.rb"
  ^ super selector

%

