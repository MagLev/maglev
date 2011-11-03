
set class RubyFCallNode
category: '*maglev-runtime'
classmethod:
initialize
  | dict |
  (dict := IdentityKeyValueDictionary new)
    at: #'block_given?' put: RubyBlockGivenNode ;
    at: #binding        put: RubyFCallBindingNode ;
    at: #__callee__     put: RubyFCallCalleeNode ;
    at: #__method__     put: RubyFCallCalleeNode .

   RubyAbstractCallNode evalSelectors do:[ :sym| dict at: sym put: RubyFCallEvalNode ].
   dict immediateInvariant. 
   SpecialRubySelectors := dict .

%


doit
RubyFCallNode initialize.
%


set class RubyFCallNode
category: '*maglev-ast'
classmethod:
s_a: rcvr b: callSym c: args
  | node   nodeCls    | 
  nodeCls :=  SpecialRubySelectors at: callSym otherwise: self .
  (node := nodeCls _basicNew)
        methodName: callSym ;
        argsNodeRp: args ;
        receiverNode: rcvr .
  "caller responsible for node position: "
  ^ node

%


set class RubyFCallNode
category: 'accessing'
method:
argIsArgsCat
  | n |
  ^ (n := argsNode) ifNotNil:[ n isArgsCatNode] ifNil:[ false ]

%


set class RubyFCallNode
category: '(as yet unclassified)'
method:
argNodes
    | itr args |
    args := argsNode fCallArgNodes .
    (itr := iterNode) ifNotNil:[
      (args := args copy) add: itr .
    ].
    ^ args

%


set class RubyFCallNode
category: 'accessing'
method:
argsNode

	 ^ argsNode

%


set class RubyFCallNode
category: 'accessing'
method:
argsNode: aNode
	argsNode := aNode

%


set class RubyFCallNode
category: 'as yet unclassified'
method:
argsNodeRp: args
  args ifNotNil:[
	 argsNode := args .
	 iterNode := args getClearIter .
  ].

%


set class RubyFCallNode
category: 'as yet unclassified'
method:
asCallNodeForIter
   ^ self

%


set class RubyFCallNode
category: 'parsetree-as yet unclassified'
method:
childrenForMatch
	^ super childrenForMatch, {argsNode. iterNode}

%


set class RubyFCallNode
category: '(as yet unclassified)'
method:
hasBlockArg
  ^ iterNode ~~ nil

%


set class RubyFCallNode
category: '(as yet unclassified)'
method:
hasRestArg
  | n |
  ^ (n := argsNode) ~~ nil and: [ n hasRestArg]

%


set class RubyFCallNode
category: 'as yet unclassified'
method:
irDefinedQNode
  ^ self irDefinedQNode_aCall

%


set class RubyFCallNode
category: 'parsetree-as yet unclassified'
method:
isSameAs: other
	^ self name = other name

%


set class RubyFCallNode
category: 'accessing'
method:
iterNode

	 ^ iterNode

%


set class RubyFCallNode
category: 'accessing'
method:
iterNode: aNode
	iterNode := aNode

%


set class RubyFCallNode
category: 'as yet unclassified'
method:
iterNode_forRp: anIterNode
  iterNode ifNotNil:[
     self signalParseError: 'both block arg and actual block given '
  ].
  iterNode := anIterNode

%


set class RubyFCallNode
category: '*maglev-runtime'
method:
methodName
  ^ callName

%


set class RubyFCallNode
category: '*maglev-runtime'
method:
methodName: aSymbol
  callName := aSymbol .
  implicitDollarTilde := self implicitTildeFor: aSymbol 

%


set class RubyFCallNode
category: 'converting'
method:
numArgs
	argsNode ifNil: [^ 0].
	argsNode list ifNil: [^ 0].
	^ argsNode list size

%


set class RubyFCallNode
category: 'printing'
method:
printSourceOn: aStream
	aStream nextPutAll: self name.
	aStream nextPut: $(.
	argsNode ifNotNil: [argsNode printArgsOn: aStream].
	aStream nextPut: $).
	aStream printNode: iterNode

%


set class RubyFCallNode
category: 'converting'
method:
receiverNode
	^ rcvrNode

%


set class RubyFCallNode
category: 'converting'
method:
receiverNode: aNode
  rcvrNode := aNode

%


set class RubyFCallNode
category: '*maglev-runtime'
method:
selector
  ^ callName

%


set class RubyFCallNode
category: '*maglev-runtime'
method:
walkCallArgs: lst withScope: aScope
  | nargs |
  1 to: (nargs := lst size) - 1 do:[:n |
    (lst at: n) walkWithScope: aScope .
  ].
  nargs ~~ 0 ifTrue:[ | lastarg |
    (lastarg := lst at: nargs) walkWithScope: aScope .
    (self hasBlockArg
     or:[ RubyAbstractCallNode lastArgNoToProcSelectors includes: callName]) ifTrue:[
        lastarg postWalkForYield.  "does not need to_proc conversion"
    ].
  ].

%


set class RubyFCallNode
category: '*maglev-runtime'
method:
walkWithScope: aScope

  implicitDollarTilde walkWithScope: aScope .
  super walkWithScope: aScope .

%


set class RubyFCallNode
category: '*maglev-runtime'
method:
_inspect
 |res|
 res := '
  [:fcall, :', callName, ', ', argsNode _inspect .
  iterNode ifNotNil:[
    res add: ', '; add: iterNode _inspect 
  ].
  res add: $] .
  ^ res

%

