
doit
RubyAbstractCallNode subclass: 'RubyCallNode'
	instVarNames: #( argsNode iterNode receiverNode
	                  evaluationTmpAssoc procNewZeroHasMeth implicitDollarTilde callName)
	classVars: #( SpecialRubySelectors)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyCallNode
removeallmethods
removeallclassmethods

set class RubyCallNode class
category: '*maglev-runtime'
method:
comment
  ^ '
RubyVCallNode  is a call with  zero arguments  .
     
RubyFCallNode   is a call with receiver==self and at least one arg 

RubyCallNode   is a call with explicit receiver . 
 
The following eval variants exist
  RubyVCallEvalNode  (should result in ArgumentError, too few args at runtime)
  RubyFCallEvalNode
  RubyCallEvalNode
'

%


set class RubyCallNode class
category: 'as yet unclassified'
method:
initialize
 | dict |
  dict := IdentityKeyValueDictionary new .
    
  RubyAbstractCallNode evalSelectors do:[ :sym |  dict at: sym put:  RubyCallEvalNode  ].
  dict immediateInvariant. 
  SpecialRubySelectors := dict .

%


doit
RubyCallNode initialize.
%


set class RubyCallNode class
category: '*maglev-ast'
method:
s_a: rcvr b: callSym c: args
  | node nodeCls | 
  nodeCls :=  SpecialRubySelectors at: callSym otherwise: self .
  (node := nodeCls _basicNew)
     methodName: callSym ;
     argsNodeRp: args ;
     receiverNode: rcvr .
  "caller responsible for node position: "
  ^ node

%


set class RubyCallNode
category: 'converting'
method:
argIsArgsCat
  | n |
   ^ (n := argsNode) ifNotNil:[ n isArgsCatNode] ifNil:[ false ]

%


set class RubyCallNode
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


set class RubyCallNode
category: 'accessing'
method:
argsNode

	 ^ argsNode

%


set class RubyCallNode
category: 'accessing'
method:
argsNode: aNode
	argsNode := aNode

%


set class RubyCallNode
category: 'as yet unclassified'
method:
argsNodeRp: args
  args ifNotNil:[
	 argsNode := args .
	 iterNode := args getClearIter .
  ].

%


set class RubyCallNode
category: 'as yet unclassified'
method:
asCallNodeForIter
   ^ self

%


set class RubyCallNode
category: 'parsetree-as yet unclassified'
method:
childrenForMatch
	^ super childrenForMatch

%


set class RubyCallNode
category: '(as yet unclassified)'
method:
hasBlockArg
  ^ iterNode ~~ nil

%


set class RubyCallNode
category: '(as yet unclassified)'
method:
hasRestArg
  | n |
  ^ (n := argsNode) ~~ nil and: [ n hasRestArg]

%


set class RubyCallNode
category: 'as yet unclassified'
method:
irDefinedQNode
  ^ self irDefinedQNode_aCall

%


set class RubyCallNode
category: '*maglev-runtime'
method:
irForModuleNesting
  ^ receiverNode irForModuleNesting 

%


set class RubyCallNode
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


set class RubyCallNode
category: '*maglev-runtime'
method:
irForTypeCoerce3Args: fullSel 
      "ruby_selector_suffix dependent"
   | toCls coercSel tmpLeaf asgnNode isaSelector specSendNode ifNode coercSufix |
   "at this point self.irArgNodes should be IR for  anObj, aLitVar, aSymbol .
    If compiling bootstrap code we always optimize a call such as   
       Type.coerce_to(o , Integer, :to_int)
    to
       o _isInteger ifTrue:[ o ]
          ifFalse:[ Type.__coerce_to_Integer_to_int(o) ]
    If not compiling bootstrap code, we attempt the optimization.
   "
   fullSel == #'coerce_to#3__' ifTrue:[ coercSufix := '' ]
     ifFalse:[  fullSel == #'coerce_to_or_nil#3__' ifTrue:[ coercSufix := '_or_nil' ]
                       ifFalse:[ self error:'invalid coerce selector']].  
   toCls := (irArgNodes at: 2) litVarValueOrNil .
   toCls ifNil:[  
       self installingPrims ifTrue:[   GsFile gciLogServer:
          'warning 2nd arg dynamic constant deoptimizes Type.coerce_to , near ',     self sourcePositionAsString.
        ].
       ^ nil
    ].
   (coercSel  := (irArgNodes at:3) symbolLiteralValue) ifNil:[  
       self signalParseError:'3rd arg to coerce_to must be a Symbol' 
   ].
   isaSelector := GsComSelectorLeaf classToISAselector: toCls.
   isaSelector ifNil:[  
      self installingPrims ifTrue:[   GsFile gciLogServer:
         'warning Type.coerce_to not optimized for ', toCls name , ' near ' , self sourcePositionAsString
       ].
      ^ nil
   ].
  (asgnNode := GsComAssignmentNode _basicNew)     "tmp := firstIrArg "
     dest:  (tmpLeaf := evaluationTmpAssoc leaf )
     source:  (irArgNodes at:1)  .
  self ir: asgnNode .
  ( specSendNode := GsComSendNode new )        " example:   tmp _isInteger"
     rcvr: asgnNode ;
     stSelector: isaSelector .                
  ( ifNode := GsComSendNode new)
     rcvr: specSendNode ;
     stSelector: #ifTrue:ifFalse:   ;
     appendArgument: (self newInlineBlock:[ :blk | 
                blk appendStatement: (GsComVariableNode new leaf: tmpLeaf). blk ]) ;
     appendArgument: (self newInlineBlock:[ :blk | | slowNode slowSel  |
              (slowSel := '__coerce_to_' copy )  addAll: (GsComSelectorLeaf classToRubyClassName: toCls) ;
                  addAll: '_' ; addAll: coercSel ; addAll: coercSufix ; addAll: '#1__'    .
              slowSel := slowSel asSymbol .
              ( slowNode := GsComSendNode new)
                   rcvr: self irReceiverNode ;
                   rubySelector:  slowSel .
              slowNode appendArgument: (GsComVariableNode new leaf: tmpLeaf) .
              self ir: slowNode .
              blk appendStatement: slowNode .
              blk ] );
      optimize .
   self ir: ifNode .
   ^ ifNode 

%


set class RubyCallNode
category: 'accessing'
method:
iterNode

	 ^ iterNode

%


set class RubyCallNode
category: 'accessing'
method:
iterNode: aNode
	iterNode := aNode

%


set class RubyCallNode
category: '*maglev-ast'
method:
iterNode_forRp: anIterNode
  iterNode ifNotNil:[
     self signalParseError: 'both block arg and actual block given'
  ].
  iterNode := anIterNode

%


set class RubyCallNode
category: '*maglev-runtime'
method:
methodName
  ^ callName

%


set class RubyCallNode
category: '*maglev-runtime'
method:
methodName: aSymbol
  callName := aSymbol .
  implicitDollarTilde := self implicitTildeFor: aSymbol .

%


set class RubyCallNode
category: '*maglev-ast'
method:
node_assign_set_rhs: rhs
  rhs ifNil:[ self error:'invalid nil arg to RubyCallNode>>node_assign_set_rhs:' ].
  argsNode _append: rhs .
  ^ self

%


set class RubyCallNode
category: 'converting'
method:
numArgs
	^ argsNode ifNil: [0] ifNotNil: [argsNode list size]

%


set class RubyCallNode
category: 'printing'
method:
printSourceOn: aStream
	aStream
		printNode: receiverNode;
		nextPutAll: (self name asSymbol isInfix ifTrue: [''] ifFalse: ['.']);
		nextPutAll: self name;
		nextPutAll: '('.
		argsNode ifNotNil: [argsNode printArgsOn: aStream].
		aStream nextPutAll: ')';
		printNode: iterNode
	

%


set class RubyCallNode
category: '*maglev-runtime'
method:
rcvr_isModuleClass
  ^ receiverNode isModuleClass

%


set class RubyCallNode
category: 'as yet unclassified'
method:
rcvr_isProcClass
  ^ receiverNode isProcClass

%


set class RubyCallNode
category: 'as yet unclassified'
method:
rcvr_isTypeClass
  ^ receiverNode isTypeClass

%


set class RubyCallNode
category: 'converting'
method:
receiverNode 
	^ receiverNode

%


set class RubyCallNode
category: 'accessing'
method:
receiverNode: aNode
	receiverNode := aNode

%


set class RubyCallNode
category: '*maglev-runtime'
method:
selector
  ^ callName

%


set class RubyCallNode
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
     or:[ RubyAbstractCallNode lastArgNoToProcSelectors includes: callName ]) ifTrue:[
        lastarg postWalkForYield.  "does not need to_proc conversion"
    ].
  ].

%


set class RubyCallNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
    |   nam   |
    implicitDollarTilde walkWithScope: aScope .
    super walkWithScope: aScope . "saves and walks   self argNodes "

    (nam := callName) == #new ifTrue:[
       receiverNode isProcClass ifTrue:[ | mth |
         mth := RubyCompilerState current topMethodDefOrNil.
         mth ifNotNil:[ mth setHasBlockArgRef ].
       ].
    ] ifFalse:[
       (nam at:1) == $c ifTrue:[
         ((nam == #coerce_to or:[nam == #coerce_to_or_nil]) and:[ self rcvr_isTypeClass ]) ifTrue:[
           " allocate a method or block temp in innermost block for use in IR"
           evaluationTmpAssoc := aScope newEvaluationTemp
         ].
       ].
       (RubyAbstractCallNode rcvrNoToProcSelectors includes: nam) ifTrue:[
         receiverNode postWalkForYield .
       ].
    ]

%


set class RubyCallNode
category: '*maglev-runtime'
method:
_inspect
 | res |
 res := '
  [:call, ', receiverNode _inspect, 
          ', :', callName, ', ', argsNode _inspect .
  iterNode ifNotNil:[ res addAll: ', ' ; addAll: iterNode _inspect ].
  res add: $]  .
  ^ res

%

