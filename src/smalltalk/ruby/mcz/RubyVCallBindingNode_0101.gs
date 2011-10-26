
set class RubyVCallBindingNode
category: 'as yet unclassified'
method:
asCallNodeForIter
  self error: 'block argument to #binding not supported' .
  ^ nil

%


set class RubyVCallBindingNode
category: '*maglev-runtime'
method:
evalLexPathArg
  ^ RubyEvalLexPathNode _basicNew  

%


set class RubyVCallBindingNode
category: 'as yet unclassified'
method:
hasBlockArg
  ^ hasBlkArg == true

%


set class RubyVCallBindingNode
category: 'as yet unclassified'
method:
irArgNodes
  | arr irMth cst |
  cst := RubyCompilerState current .
  cst installingPrims ifTrue:[ 
	  self error:'creating a Binding not allowed during bootstrap'
  ].
  arr := argsList collect:[ :ea | ea irNode ].
  ( irMth  := cst topMethodDef comIrMethNode) ifNotNil:[
	 "append the real or implicit block leaf, which is last declared arg"
	 arr add: (GsComVariableNode new leaf:( irMth  arguments last)) .
	 hasBlkArg := true 
  ].
  ^ arr 

%


set class RubyVCallBindingNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  RubyCompilerState current topMethodDef setSendsBinding ; setHasInnerEvalOrDef .
  ^ super walkWithScope: aScope 

%

