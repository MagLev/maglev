
doit
RubyAbstractCallNode subclass: 'RubyYieldNode'
	instVarNames: #( checkState argsNode evalRcvr
	                  forceSelector)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyYieldNode
removeallmethods
removeallclassmethods

set class RubyYieldNode
category: '*maglev-runtime'
method:
argIsSplatAt: idx
  argsNode isSplatNode ifTrue:[ ^ false ].
  ^ super argIsSplatAt: idx

%


set class RubyYieldNode
category: '(as yet unclassified)'
method:
argNodes
  ^ argsNode ifNil: [#()] ifNotNil: [argsNode list ].  

%


set class RubyYieldNode
category: 'accessing'
method:
argsNode

	 ^ argsNode

%


set class RubyYieldNode
category: 'accessing'
method:
argsNode: aNode
	argsNode := aNode

%


set class RubyYieldNode
category: 'accessing'
method:
checkState

	 ^ checkState

%


set class RubyYieldNode
category: 'accessing'
method:
checkState: a
	checkState := a

%


set class RubyYieldNode
category: '*maglev-runtime'
method:
fullSelector: isStSend
  | fs |
  (fs := forceSelector) ifNotNil:[ 
    lastArgIsBlk := self hasBlockArg .
    fixedArgCount := 1 .
  ] ifNil:[
    fs := super fullSelector: isStSend .   
  ].
  lastArgIsBlk ifTrue:[  self error: 'unexpected lastArgIsBlk for yield'  ]. 
  ^ fs 

%


set class RubyYieldNode
category: '*maglev-runtime'
method:
irArgNodes
      "ruby_selector_suffix dependent"
  | args |  
  (args := argsNode) ifNotNil:[ 
    args class == RubyRpCallArgs ifTrue:[
      args irYieldArgs ifNotNil:[ :yArgs | 
        forceSelector := yArgs at: 1  .
        ^ { yArgs at: 2 }
      ].
    ] ifFalse:[
      args isSplatNode ifTrue:[
        forceSelector := #'call#0*_' .
         ^ { args irYieldStarNode } 
      ].
    ].
  ].
  ^ super irArgNodes 

%


set class RubyYieldNode
category: '*maglev-runtime'
method:
irReceiverNode
  evalRcvr ifNotNil:[ | snd |
    (snd := GsComSendNode new)
       rcvr: (GsComLiteralNode newObject: GsProcess);
       stSelector: #_rubyEvalBlockArg .
    self ir: snd .
    ^ snd
  ] ifNil:[
    ^  self currentScope implicitBlockVar
  ].

%


set class RubyYieldNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		nextPutAll: 'yield';
		parenthesize: argsNode

%


set class RubyYieldNode
category: 'converting'
method:
receiverNode
	^ nil

%


set class RubyYieldNode
category: 'converting'
method:
selector
	^ #call

%


set class RubyYieldNode
category: '*maglev-runtime'
method:
walkWithScope: aScope 
   | mth anod | 
   mth := RubyCompilerState current topMethodDefOrNil .
   mth ifNil:[ self error: 'no enclosing method def for yield' ].
   mth setHasBlockArgRef ifTrue:[ "inEval"  evalRcvr := true ].

   (anod := argsNode) ifNotNil:[
     anod isSplatNode ifTrue:[
       anod walkWithScope: aScope  "because SplatNode>>list  lies "
     ] ifFalse:[
        super walkWithScope: aScope  "because we will use super.irArgNodes"
     ].
   ] ifNil:[
     super walkWithScope: aScope  "because we will use super.irArgNodes"
   ].
   "YieldNode has no receiverNode , so does not need to postWalkForYield "

%


set class RubyYieldNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:yield ,',  argsNode _inspect , $]

%

