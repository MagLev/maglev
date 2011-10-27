
set class RubyRescueNode
category: '*maglev-ast'
classmethod:
s_a: body b: rescuebody c: elsebody d: srcOfs
  | res |
  rescuebody class == RubyRescueBodyNode ifFalse:[
    RubyParserM signalError:'invalid rescuebody for RubyRescueNode' .
    ^ nil
  ].
  (res := self _basicNew)
     init_a: body b: rescuebody c: elsebody d: srcOfs .
  ^ res

%


set class RubyRescueNode
category: 'accessing'
method:
bodyNode

	 ^ bodyNode

%


set class RubyRescueNode
category: 'accessing'
method:
bodyNode: aNode
	bodyNode := aNode

%


set class RubyRescueNode
category: 'accessing'
method:
elseNode

	 ^ elseNode

%


set class RubyRescueNode
category: 'accessing'
method:
elseNode: aNode
	elseNode := aNode

%


set class RubyRescueNode
category: '*maglev-ast'
method:
init_a: body b: rescuebody c: elsebody d: srcOfs 
  bodyNode := body .
  rescueBodyNode := rescuebody .
  elseNode := elsebody .
  self position: (srcOfs ifNil:[ rescuebody position ]).

%


set class RubyRescueNode
category: '*maglev-runtime'
method:
irNode
   | onArg doarg  nextRb blk send |
   onArg := rescueBodyNode irExceptionNode . 
   doarg := rescueBodyNode irBodyNode .
   nextRb := rescueBodyNode nextRescueBody .
   nextRb ifNotNil:[
     onArg := GsComArrayBuilderNode with: onArg .
     doarg := GsComArrayBuilderNode with: doarg .
     [ nextRb ~~ nil ] whileTrue:[
        onArg appendElement: nextRb irExceptionNode .
        doarg appendElement: nextRb irBodyNode .
        nextRb := nextRb nextRescueBody .
     ].
   ].
   blk := self newBlock:[:block |
     bodyNode irNodeListInto: block .
     block 
   ].
   elseNode ifNotNil:[ | elseBlk |
     elseBlk := self newBlock:[:block |
       elseNode irNodeListInto: block .
       block  
     ].
     (send := GsComSendNode new) 
        rcvr: blk ;
        stSelector: (RubyCompilerState current envId == 2 
                       ifTrue:[ #rescue2:do:else: ] ifFalse:[ #rescue1:do:else: ])  ;
        appendArgument:  onArg ;
        appendArgument:  doarg ;
        appendArgument:  elseBlk .
   ] ifNil:[
     (send := GsComSendNode new) 
        rcvr: blk ;
        stSelector: (RubyCompilerState current envId == 2 
                       ifTrue:[ #rescue2:do: ] ifFalse:[ #rescue1:do: ])  ;
        appendArgument:  onArg ;
        appendArgument:  doarg .
   ].
   self ir: send .
   ^ send

%


set class RubyRescueNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		printNode: bodyNode;
		outdent; cr;
		nextPutAll: 'rescue';
		indent; cr;
		printNode: rescueBodyNode

%


set class RubyRescueNode
category: 'accessing'
method:
rescueBodyNode: aRescueBodyNode
	rescueBodyNode := aRescueBodyNode

%


set class RubyRescueNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
   " argNodes inherits from RubyAbstractCallNode and is empty"
  | loop |
  loop := RubyCompilerState current topLoop .
  loop ifNotNil:[  loop setHasBeginRescue  ].
  bodyNode walkWithScope: aScope . 
  elseNode walkWithScope: aScope .
  rescueBodyNode walkWithScope: aScope

%


set class RubyRescueNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:rescue, ', bodyNode _inspect, ', <rescue>', rescueBodyNode _inspect, 
	       ', <else>' , elseNode _inspect , $]

%

