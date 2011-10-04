
doit
RubyAbstractCallNode subclass: 'RubyOpElementAsgnNode'
	instVarNames: #( argsNode callName receiverNode
	                  valueNode evalTmpAssocs)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-AST'
	options: #()

%

set class RubyOpElementAsgnNode
removeallmethods
removeallclassmethods

set class RubyOpElementAsgnNode class
category: '*maglev-ast'
method:
s_a: rcv b: arefArgs c: opSelTok d: val
  | res asgn_sel |
  asgn_sel := opSelTok symval .
  (res := self _basicNew )
     init_a: rcv b: arefArgs c: opSelTok d: val .
  ^ res

%


set class RubyOpElementAsgnNode
category: '(as yet unclassified)'
method:
argNodes  
  self error:'RubyOpElementAsgnNode>>argNodes should not be here'

%


set class RubyOpElementAsgnNode
category: 'accessing'
method:
argsNode

	 ^ argsNode

%


set class RubyOpElementAsgnNode
category: 'accessing'
method:
argsNode: aNode
	argsNode := aNode

%


set class RubyOpElementAsgnNode
category: 'as yet unclassified'
method:
childrenForMatch
	^ super childrenForMatch copyWith: self argsNode

%


set class RubyOpElementAsgnNode
category: 'as yet unclassified'
method:
definedQkind
  ^ 'assignment'

%


set class RubyOpElementAsgnNode
category: '*maglev-ast'
method:
init_a: rcv b: arefArgs c: opSelTok d: val
  receiverNode := rcv .
  argsNode := arefArgs .
  callName := opSelTok symval .
  valueNode := val .
  self position: opSelTok src_offset 

%


set class RubyOpElementAsgnNode
category: '*maglev-runtime'
method:
irNode 
      "ruby_selector_suffix dependent"
    |  argIrs  argIrsSiz asgnNodes 
       tmpLeafs  nTmps mName blkSel  argl asz |
    argl := argsList .   "from walkWithScope phase"
    argIrs := Array new:( asz := argl size ).
    1 to: asz do:[:n | argIrs at: n put: ( argl at: n) irNode  ].

    tmpLeafs := evalTmpAssocs copy .     nTmps := tmpLeafs size .
    1 to: nTmps do:[:m | tmpLeafs at: m put:(tmpLeafs at: m) leaf ].
    asgnNodes := Array new: nTmps .
    asgnNodes at: 1 put: ( GsComAssignmentNode _basicNew  dest: ( tmpLeafs at: 1)  
                source: self irReceiverNode ).
    1 to: (argIrsSiz := argIrs size)  do:[ :k | 
       asgnNodes at: k + 2  put:( GsComAssignmentNode _basicNew dest: (tmpLeafs at: k + 2)  
                    source: (argIrs at: k)) .
    ].
    mName := callName .
    mName == #'||' ifTrue:[ blkSel := #or: ] 
                ifFalse:[ mName == #'&&' ifTrue:[ blkSel := #and: ]].
    ^ blkSel ifNotNil:[ self irOpElAsgn: asgnNodes tmps: tmpLeafs blkSel: blkSel ]
             ifNil:[ self irOpElAsgn: asgnNodes tmps: tmpLeafs mName: mName ].

%


set class RubyOpElementAsgnNode
category: '*maglev-runtime'
method:
irOpElAsgn: asgnNodes tmps: tmpLeafs blkSel: blkSel
      "ruby_selector_suffix dependent"
  " generated code of form     (get: a) blkSel:[ store: a val: v ]   "

  | getSend nArgs getSel storeSel storeSend testSend saveRes blkBody resLeaf 
    getMask storeMask |
  getSel := #'[]'     . getMask := 0 .
  storeSel := #'[]='  . storeMask := 4 .  "one fixed arg"
  nArgs := asgnNodes size - 2 .
  (getSend := GsComSendNode new)  rcvr:  (asgnNodes at: 1 ) .
  1 to:  nArgs do:[ :k |  getSend appendArgument: (asgnNodes at: k + 2) .
                          getMask := getMask + 4 "add one fixed arg" .
                          storeMask := storeMask + 4 "add one fixed arg"
  ].
  getSend  rubySelector:  (getSel _asSymbolWithRubySuffix: getMask) .

  (storeSend := GsComSendNode new) 
     rcvr: ( GsComVariableNode new leaf: (tmpLeafs at: 1)) ;
     rubySelector: (storeSel _asSymbolWithRubySuffix: storeMask) .
  1 to: nArgs do:[:m | 
      storeSend appendArgument:( GsComVariableNode new leaf: (tmpLeafs at: m + 2 )) 
  ].
  saveRes := GsComAssignmentNode _basicNew dest:( resLeaf := tmpLeafs at: 2) 
                                           source: valueNode irNode .
  storeSend appendArgument: saveRes .
  blkBody := GsComStatementsNode new list:{ storeSend . (GsComVariableNode new leaf: resLeaf) }.
  (testSend := GsComSendNode new)   rcvr: getSend ; 
     stSelector: blkSel ;
     appendArgument: ( self irInlineBlockIr: blkBody ) ;     
     optimize .   "typically blkSel is #or:  or #and:  "
  self ir: testSend ; 
    ir: storeSend; 
    ir: getSend .
  ^ testSend 

%


set class RubyOpElementAsgnNode
category: '*maglev-runtime'
method:
irOpElAsgn: asgnNodes tmps: tmpLeafs mName: mName 
  "generated code of form    store: a val:( r := op: (get: a) with: v) . r   "
      "ruby_selector_suffix dependent"

  | getSend nArgs getSel storeSel storeSend opSend saveRes resLeaf getMask storeMask|
  getSel := #'[]' .    getMask := 0 .
  storeSel := #'[]=' . storeMask := 4 "one fixed arg" .
  nArgs := asgnNodes size - 2 .
  (storeSend := GsComSendNode new)  rcvr:  (asgnNodes at: 1 ) .
  1 to:  nArgs do:[ :k |  storeSend appendArgument: (asgnNodes at: k + 2) .
                          getMask := getMask + 4 "add one fixed arg" .
                          storeMask := storeMask + 4 "add one fixed arg" .
  ].
  storeSend  rubySelector: (storeSel _asSymbolWithRubySuffix: storeMask) .

  (getSend := GsComSendNode new) 
     rcvr: ( GsComVariableNode new leaf: (tmpLeafs at: 1)) ;
     rubySelector: (getSel _asSymbolWithRubySuffix: getMask) .
  1 to: nArgs do:[:m | 
      getSend appendArgument:( GsComVariableNode new leaf: (tmpLeafs at: m + 2)) 
  ].
  (opSend := GsComSendNode new)   
     rcvr: getSend ; 
     rubySelector: (mName _asSymbolWithRubySuffix: 16r4 "#1__") ;
     appendArgument: valueNode irNode . 
  saveRes := GsComAssignmentNode _basicNew dest: (resLeaf := tmpLeafs at: 2) source: opSend .
  storeSend appendArgument: saveRes .

  self ir: opSend ; ir: storeSend; ir: getSend .
  ^ GsComStatementsNode new list:{ storeSend . (GsComVariableNode new leaf: resLeaf) }. 

%


set class RubyOpElementAsgnNode
category: '*maglev-runtime'
method:
methodName
  ^ callName

%


set class RubyOpElementAsgnNode
category: '*maglev-runtime'
method:
methodName: aSymbol
  callName := aSymbol

%


set class RubyOpElementAsgnNode
category: 'converting'
method:
receiverNode
	^ receiverNode

%


set class RubyOpElementAsgnNode
category: 'accessing'
method:
receiverNode: aNode
	receiverNode := aNode

%


set class RubyOpElementAsgnNode
category: 'converting'
method:
selector
	^ '[]='

%


set class RubyOpElementAsgnNode
category: 'accessing'
method:
valueNode

	 ^ valueNode

%


set class RubyOpElementAsgnNode
category: 'accessing'
method:
valueNode: aNode
	valueNode := aNode

%


set class RubyOpElementAsgnNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
  | args sz nTmps evalTmps |
  args := argsNode argNodes .  
  argsList := args . 
  sz  := args size .
  nTmps := 2 "for rcvr, result" . 
  1 to: sz do:[:m | 
     (args at: m) walkWithScope: aScope .
     nTmps := nTmps + 1 .
  ].
  valueNode walkWithScope: aScope .
  receiverNode walkWithScope: aScope .
  evalTmps := Array new: nTmps .
  1 to: nTmps do:[:k |  evalTmps at: k put: aScope newEvaluationTemp ].
  evalTmpAssocs := evalTmps .

%


set class RubyOpElementAsgnNode
category: '*maglev-runtime'
method:
_inspect
  ^  '[:op_asgn1, ', receiverNode _inspect, ', ', argsNode _inspect,
                  ', :',  callName, ', ', valueNode _inspect , $]

%

