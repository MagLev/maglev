
set class RubyVCallBlockGivenNode
category: 'as yet unclassified'
method:
asCallNodeForIter
  self error: 'block argument to  block_given?  not supported' .
  ^ nil

%


set class RubyVCallBlockGivenNode
category: '*maglev-runtime'
method:
irNode
  rcvrIsSelf ifFalse:[ ^ super irNode ].
  ^ RubyBlockGivenNode irNode: self evalRcvr: evalRcvr

%


set class RubyVCallBlockGivenNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  | cst tmd |
  (rcvrIsSelf := rcvrNode class == RubySelfNode ) ifTrue:[
    (tmd := (cst := RubyCompilerState current) topMethodDef) setHasBlockArgRef ifTrue:[
      "inEval"  evalRcvr := true .
    ].
  ].
  super walkWithScope: aScope 

%

