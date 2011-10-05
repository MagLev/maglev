
set class RubyOpAsgnNode class
category: '*maglev-ast'
method:
s_a: rcv b: asgnSelTok c: opSelTok d: val
  | res asgn_sel |
  asgn_sel := asgnSelTok symval .
  (res := self _basicNew )
     init_a: rcv b: asgnSelTok c: opSelTok d: val .
  ^ res

%


set class RubyOpAsgnNode
category: 'as yet unclassified'
method:
childrenForMatch
	^ super childrenForMatch, self valueNode

%


set class RubyOpAsgnNode
category: 'as yet unclassified'
method:
definedQkind
  ^ 'assignment'

%


set class RubyOpAsgnNode
category: '*maglev-ast'
method:
init_a: rcv b: asgnSelTok c: opSelTok d: val
  |  asgnSel varAsgnStr |
  operatorCallName := opSelTok symval .
  asgnSel := asgnSelTok symval .
  variableCallName := asgnSel .

  (varAsgnStr := String withAll: asgnSel) add: $= .
  variableAsgnCallName := varAsgnStr asSymbol .

  receiverNode := rcv .
  valueNode := val .
  self position: asgnSelTok src_offset  

%


set class RubyOpAsgnNode
category: '*maglev-runtime'
method:
irNode   "t :=rcvr . t setter: ( (t getter) op: arg  ) .     for all but ||= , &&=  
           t:= rcvr . (v := t getter) or:[ t setter: v ]     for   ||=   "
      "ruby_selector_suffix dependent"
    | tasgn tmpLeaf  get opName opSnd store blkSel getSel storeSel  |  
    tmpLeaf := rcvrTmp leaf . 
    opName := operatorCallName .
    opName == #'||' ifTrue: [ blkSel := #or: ] ifFalse:[ opName == #'&&' ifTrue:[ blkSel := #and: ]].
    getSel := variableCallName .
    storeSel := variableAsgnCallName _asSymbolWithRubySuffix: 16r4 " #1__ " .
    (tasgn := GsComAssignmentNode _basicNew) dest: tmpLeaf source: receiverNode irNode .  self ir: tasgn .
    (get := GsComSendNode new) 
       rubySelector: ( getSel _asSymbolWithRubySuffix: 16r0 "#0__" ). 
    self ir: get .
    (store := GsComSendNode new)  rubySelector: storeSel .      self ir: store .
    blkSel ifNil:[ 
      store rcvr: tasgn  .
      get  rcvr:  (GsComVariableNode new leaf: tmpLeaf) .
      (opSnd := GsComSendNode new)  rcvr: get ;
        rubySelector: (opName _asSymbolWithRubySuffix: 16r4 " #1__ ")  ;
        appendArgument: valueNode irNode .                  self ir: opSnd .
      store  appendArgument: opSnd .
      ^  store .
    ] ifNotNil:[
      get rcvr: tasgn .
      store rcvr: (GsComVariableNode new leaf: tmpLeaf) ;
            appendArgument: valueNode irNode .  
      (opSnd := GsComSendNode new)    rcvr: get ;
         stSelector:  blkSel ;
         appendArgument: ( self irInlineBlockIr: store ) ; optimize .
      ^ self ir: opSnd 
    ]

%


set class RubyOpAsgnNode
category: '*maglev-runtime'
method:
operatorCallName: aSymbol
  operatorCallName := aSymbol 

%


set class RubyOpAsgnNode
category: 'accessing'
method:
receiverNode

	 ^ receiverNode

%


set class RubyOpAsgnNode
category: 'accessing'
method:
receiverNode: aNode
	receiverNode := aNode

%


set class RubyOpAsgnNode
category: 'accessing'
method:
valueNode

	 ^ valueNode

%


set class RubyOpAsgnNode
category: 'accessing'
method:
valueNode: aNode
	valueNode := aNode

%


set class RubyOpAsgnNode
category: '*maglev-runtime'
method:
variableAsgnCallName: aSymbol
   variableAsgnCallName := aSymbol

%


set class RubyOpAsgnNode
category: '(as yet unclassified)'
method:
walkWithScope: aScope
  rcvrTmp := aScope newEvaluationTemp .
  valueNode walkWithScope: aScope .   "argNodes not used/implemented"
  receiverNode walkWithScope: aScope .

%


set class RubyOpAsgnNode
category: '*maglev-runtime'
method:
_inspect
 ^ '[:op_asgn2, ', receiverNode _inspect, 
      ', :', variableAsgnCallName, ', :',  operatorCallName, ', ',  valueNode _inspect  , $]

%

