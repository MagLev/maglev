
doit
RubyAbstractCallNode subclass: 'RubySuperNode'
	instVarNames: #( argsNode iterNode implicitBlockArg)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubySuperNode
removeallmethods
removeallclassmethods

set class RubySuperNode
category: 'as yet unclassified'
method:
argIsSplatAt: idx 
  ^ (argsList _rubyAt: -1) isSplatNode

%


set class RubySuperNode
category: 'converting'
method:
argNodes
	^ argsNode argNodes "iterNode handled by irArgNodes "

%


set class RubySuperNode
category: 'accessing'
method:
argsNode

	 ^ argsNode

%


set class RubySuperNode
category: 'accessing'
method:
argsNode: aNode
	argsNode := aNode

%


set class RubySuperNode
category: 'as yet unclassified'
method:
asCallNodeForIter
   ^ self

%


set class RubySuperNode
category: 'as yet unclassified'
method:
definedQkind 
  ^ #super

%


set class RubySuperNode
category: '(as yet unclassified)'
method:
hasBlockArg
    ^ implicitBlockArg ifTrue:[ true ] ifFalse:[ iterNode ~~ nil ]

%


set class RubySuperNode
category: '(as yet unclassified)'
method:
hasRestArg
  | n |
  ^ (n := argsNode) ~~ nil and: [ n hasRestArg]

%


set class RubySuperNode
category: 'as yet unclassified'
method:
irArgNodes
  | res itr  |
  res := super irArgNodes "handles all but iterNode"  .
  (itr := iterNode ) ifNotNil:[
	res add: itr irNode .
	^ res .
  ].
  implicitBlockArg ifTrue:[ 
    "walkWithScope: has forced an implicit block arg"
    res add: self irImplicitBlockArg 
  ].
  ^ res

%


set class RubySuperNode
category: 'converting'
method:
irReceiverNode
	^ GsComVariableNode new leaf: GsComVarLeaf new initializeSuper

%


set class RubySuperNode
category: 'converting'
method:
irReceiverNodeEach: fullSelArray
  
  fullSelArray at: 1 put: #__superEach:  .
  RubyCompilerState current topCompiler setNeedsSuperEach .
  ^ GsComVariableNode newSelf

%


set class RubySuperNode
category: '(as yet unclassified)'
method:
isSendSuper
  ^ true

%


set class RubySuperNode
category: 'accessing'
method:
iterNode: aNode
	iterNode := aNode

%


set class RubySuperNode
category: 'as yet unclassified'
method:
iterNode_forRp: anIterNode
  iterNode ifNotNil:[
     self signalParseError: 'both block arg and actual block given '
  ].
  iterNode := anIterNode

%


set class RubySuperNode
category: 'converting'
method:
receiverNode
	^ RubySelfNode _basicNew

%


set class RubySuperNode
category: '*maglev-runtime'
method:
selector
  ^ RubyCompilerState current topMethodDef selectorForSuper

%


set class RubySuperNode
category: 'as yet unclassified'
method:
shouldOptimize
  ^ false  "cannot convert to Bc_SEND_CALL, etc"

%


set class RubySuperNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
  | itr |
  (itr := iterNode) ifNil:[ | args |
	 (args := argsNode) ifNotNil:[
      itr := args getClearIter .
      iterNode := itr .
    ].
  ].
  itr ifNil:[
	 aScope inBootstrap ifTrue:[ implicitBlockArg := false ]
                   ifFalse:[   implicitBlockArg := true .
                            RubyCompilerState current topMethodDef setHasBlockArgRef ]. 
  ] ifNotNil:[
    implicitBlockArg := false .
    itr walkWithScope: aScope . "excluded from argNodes result, so walk here"
  ].
  super walkWithScope: aScope 

%


set class RubySuperNode
category: '*maglev-runtime'
method:
_inspect
  ^ '
  [:super, ', argsNode _inspect, ', ', iterNode _inspect  , $]

%

