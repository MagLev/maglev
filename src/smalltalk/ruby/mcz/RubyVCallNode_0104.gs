
set class RubyVCallNode
category: 'parsetree'
classmethod:
initialize
  | dict |
  (dict := IdentityKeyValueDictionary new)
    at: #raise put: RubyVCallRaiseNode ;
    at: #binding put: RubyVCallBindingNode ;
    at: #'block_given?' put: RubyVCallBlockGivenNode ;
    at: #__callee__ put: RubyVCallCalleeNode ;
    at: #__method__ put: RubyVCallCalleeNode .

   RubyAbstractCallNode evalSelectors do:[ :sym |
       dict at: sym put:  RubyVCallEvalNode 
   ].
   dict immediateInvariant. 
   SpecialRubySelectors := dict .

%


doit
RubyVCallNode initialize.
%


set class RubyVCallNode
category: '*maglev-ast'
classmethod:
s_a: rcvr b: callSym
  | node  nodeCls  |
  nodeCls := SpecialRubySelectors at: callSym otherwise: self .
  (node := nodeCls _basicNew )
       methodName: callSym ;
       receiverNode: rcvr  .
  "caller responsible for node position: "
   ^ node

%


set class RubyVCallNode
category: 'as yet unclassified'
method:
asCallNodeForIter
  | node |
  (node := RubyCallNode _basicNew)
     position: position ;
     methodName: callName ;
     receiverNode: rcvrNode .
  ^ node 

%


set class RubyVCallNode
category: 'as yet unclassified'
method:
irDefinedQNode
  ^ self irDefinedQNode_aCall

%


set class RubyVCallNode
category: '*maglev-runtime'
method:
irForModuleNesting
  ^ rcvrNode irForModuleNesting 

%


set class RubyVCallNode
category: '*maglev-runtime'
method:
irForProcNewZeroArgs
   "the implicit block leaf will be either nil or an ExecBlock at runtime"
      "ruby_selector_suffix dependent"
   | node  | 
   ( node := GsComSendNode new )
        rcvr: self irReceiverNode;
        rubySelector:  #'new#0_&' ;
        appendArgument: self currentScope implicitBlockVar .
    self ir: node .
    ^ node 

%


set class RubyVCallNode
category: '*maglev-runtime'
method:
methodName
  ^ callName

%


set class RubyVCallNode
category: '*maglev-runtime'
method:
methodName: aSymbol
  callName := aSymbol .
  implicitDollarTilde := self implicitTildeFor: aSymbol 

%


set class RubyVCallNode
category: 'printing'
method:
printSourceOn: aStream
	aStream nextPutAll: self name, '()'

%


set class RubyVCallNode
category: '*maglev-runtime'
method:
rcvr_isModuleClass
  ^ rcvrNode isModuleClass

%


set class RubyVCallNode
category: '*maglev-runtime'
method:
rcvr_isProcClass
  ^ rcvrNode isProcClass

%


set class RubyVCallNode
category: 'converting'
method:
receiverNode
	^ rcvrNode

%


set class RubyVCallNode
category: 'converting'
method:
receiverNode: aNode
	 rcvrNode := aNode

%


set class RubyVCallNode
category: '*maglev-runtime'
method:
selector
  ^ callName

%


set class RubyVCallNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
    implicitDollarTilde walkWithScope: aScope .
    super walkWithScope: aScope.
    (RubyAbstractCallNode rcvrNoToProcSelectors includes: callName) ifTrue:[
      rcvrNode postWalkForYield
    ].
    callName == #new ifTrue:[
       rcvrNode isProcClass ifTrue:[ | mth |
         mth := RubyCompilerState current topMethodDefOrNil.
         mth ifNotNil:[ mth setHasBlockArgRef ].
       ].
    ].

%


set class RubyVCallNode
category: '*maglev-runtime'
method:
_inspect
  ^ '
  [:vcall, ', rcvrNode _inspect, ', :', callName , $]

%

