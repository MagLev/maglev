
set class RubyZSuperNode class
category: '*maglev-ast'
method:
s_a: srcOfs
  | res |
  (res := self _basicNew) position: srcOfs .
  ^ res

%


set class RubyZSuperNode
category: 'as yet unclassified'
method:
argIsSplatAt: idx
  ^ false

%


set class RubyZSuperNode
category: 'as yet unclassified'
method:
asCallNodeForIter
   ^ self

%


set class RubyZSuperNode
category: 'as yet unclassified'
method:
definedQkind
  ^ #super

%


set class RubyZSuperNode
category: 'as yet unclassified'
method:
hasBlockArg
  ^ iterNode ifNotNil:[ true ] ifNil:[ methSelector last == $& ].

%


set class RubyZSuperNode
category: '(as yet unclassified)'
method:
hasRestArg
  | sel sz |

  sel := methSelector .
  sz := sel size. 
  (sel at: sz) == $* ifTrue:[ ^ true ].
  (sz >= 2 and:[ (sel at: sz - 1) == $*]) ifTrue:[ ^ true ].
  ^ false 

%


set class RubyZSuperNode
category: '*maglev-runtime'
method:
irArgNodes
  "replicate the args to the method containing the send of  super"
  | mth arr sel itr |
  mth := RubyCompilerState current topMethodDef . 
  sel := mth methodSelector .
  sel ifNil:[  self signalParseError:
       'super with implicit args supported only in a compile-time def '
  ].
  methSelector := sel .  
  arr := { } .
  mth comIrMethNode arguments do:[:aLeaf |
     arr add: (GsComVariableNode new leaf: aLeaf )
  ].
  (itr := iterNode) ifNotNil:[
    "override implicit block arg with the block passed to super"
    arr at: arr size put: itr irNode
  ].
   ^ arr .

%


set class RubyZSuperNode
category: 'accessing'
method:
irReceiverNode

	^ GsComVariableNode new leaf: GsComVarLeaf new initializeSuper

%


set class RubyZSuperNode
category: 'accessing'
method:
irReceiverNodeEach: fullSelArray
  
  fullSelArray at: 1 put: #__superEach:  .
  RubyCompilerState current topCompiler setNeedsSuperEach .
  ^ GsComVariableNode newSelf

%


set class RubyZSuperNode
category: '(as yet unclassified)'
method:
isSendSuper 
  ^ true

%


set class RubyZSuperNode
category: 'accessing'
method:
iterNode

	 ^ iterNode

%


set class RubyZSuperNode
category: 'accessing'
method:
iterNode: aNode
	iterNode := aNode

%


set class RubyZSuperNode
category: 'as yet unclassified'
method:
iterNode_forRp: anIterNode
  iterNode ifNotNil:[
     self signalParseError: 'both block arg and actual block given '
  ].
  iterNode := anIterNode

%


set class RubyZSuperNode
category: 'accessing'
method:
receiverNode
	^ nil

%


set class RubyZSuperNode
category: '*maglev-runtime'
method:
selector
  ^ RubyCompilerState current topMethodDef selectorForSuper

%


set class RubyZSuperNode
category: 'as yet unclassified'
method:
shouldOptimize
  ^ false  "cannot convert to Bc_SEND_CALL, etc"

%


set class RubyZSuperNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  | itr |
  (itr := iterNode) ifNotNil:[
     "not in argNodes result, so walk here"
     itr walkWithScope: aScope . 
  ].
  RubyCompilerState current topMethodDef setHasBlockArgRef  .
  super walkWithScope: aScope 

%


set class RubyZSuperNode
category: '*maglev-runtime'
method:
_inspect
  | res |
  res := '[:zsuper ' copy.
  iterNode ifNotNil:[
    res add: ', '; add: iterNode _inspect 
  ].
  res add: $] .
  ^ res

%

