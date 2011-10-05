
set class RubyAbstractCallNode class
category: 'as yet unclassified'
method:
evalSelectors 
  ^ #( #eval  #module_eval #class_eval #instance_eval )

%


set class RubyAbstractCallNode class
category: '*maglev-runtime'
method:
initialize
  | dict |
  dict := IdentityKeyValueDictionary new .
  #( #new #initialize #initialize_copy #coerce_to #coerce_to_or_nil )
      do:[ :sym |  dict at: sym put: 1 ].

  self evalSelectors do:[ :sym |    dict at: sym put:  2 ].
 
  dict at: #__fixed_instvars put: 3 .

  dict immediateInvariant.
  SpecialSelectorKinds := dict .

  " following selectors are base name, used before suffix for send is computed"
  RcvrNoToProcSelectors := IdentitySet withAll: #(
     #'_equal?'  #'_not_equal?'  #'_is_a?'  #'_kind_of?'  
     #_isInteger #_isSmallInteger #_isFixnum #_isNumber #_isNumeric
     #_isFloat #_isScaledDecimal #_isSymbol #_isExceptionClass #_isExecBlock
     #_isBlock #_isArray #_isOneByteString #_isStringOrSymbol #_isString
     #_isRubyString #_isRubyHash #_isHash #_isRegexp #_isRange 
     #arity  #call #inspect #'[]' ) .

  LastArgNoToProcSelectors := IdentitySet withAll: #(
     #'_equal?'  #'_not_equal?'  #'_is_a?'  #'_kind_of?' ).
         "assume #'call&'  handled by hasBlockArg logic"

%


doit
RubyAbstractCallNode initialize.
%


set class RubyAbstractCallNode class
category: '*maglev-runtime'
method:
lastArgNoToProcSelectors
  ^ LastArgNoToProcSelectors

%


set class RubyAbstractCallNode class
category: '*maglev-runtime'
method:
rcvrNoToProcSelectors
  ^ RcvrNoToProcSelectors

%


set class RubyAbstractCallNode
category: 'as yet unclassified'
method:
argIsArgsCat
  ^ false

%


set class RubyAbstractCallNode
category: 'as yet unclassified'
method:
argIsSplatAt: idx
  ^ (argsList atOrNil: idx) isSplatNode

%


set class RubyAbstractCallNode
category: '*maglev-runtime'
method:
argNodes
    ^ #() 

%


set class RubyAbstractCallNode
category: '*maglev-runtime'
method:
buildArgumentsOn: irNode 
    | nodes rest restArray endIdx hasBlk numIrArgs idx num_args  |
    nodes := irArgNodes .
    numIrArgs := nodes size .
    endIdx := numIrArgs .
    (hasBlk := lastArgIsBlk )  ifTrue: [ endIdx := endIdx - 1 ]. 
    idx := 1 .
    num_args := fixedArgCount . "excludes blk arg"
    [ idx <= num_args ] whileTrue:[
        irNode appendArgument: (nodes at: idx) .
        idx := idx + 1
    ].
    idx <= endIdx ifTrue:[ "the args for  * in excess of 3 colon plus *  args  "
      restArray := GsComArrayBuilderNode new.
      [ idx < endIdx] whileTrue:[
         restArray appendElement: (nodes at: idx) .
         idx := idx + 1
      ].
     (self argIsSplatAt: endIdx)  ifTrue:[ | asend | "Fix Trac686"
        (asend := GsComSendNode new) 
           rcvr: restArray ;
           stSelector: #_rubyAddAll:  ;
           appendArgument: (nodes at: endIdx) .
        irNode appendArgument: asend .
      ] ifFalse:[
        restArray appendElement: (nodes at: endIdx).
        irNode appendArgument: restArray .
      ].
    ].
    hasBlk ifTrue:[  
       irNode appendArgument: (nodes at: numIrArgs)
    ].

%


set class RubyAbstractCallNode
category: 'as yet unclassified'
method:
bypassProtection
  ^ bypassProt == true

%


set class RubyAbstractCallNode
category: 'as yet unclassified'
method:
determineDynamic
  ^ 2

%


set class RubyAbstractCallNode
category: '*maglev-runtime'
method:
evalAddFileLineArgs: args for: aSelector
   | nargs |
   nargs := args size . 
   aSelector == #eval ifTrue:[
     nargs < 4 ifTrue:[
       self evalLineArg ifNotNil:[:lnum | args _rubyAt: 3 put: lnum ].
       nargs < 3 ifTrue:[
	 nargs < 2 ifTrue:[
	   args _rubyAt: 1 put: RubyNilNode newForIr .
	 ].
	 self evalFileArg ifNotNil:[:fn | args _rubyAt: 2 put: fn ].
       ].
     ].
   ] ifFalse:[  "note class_eval handled same as module_eval"
     self hasExplicitBlockArg ifTrue:[
	"do nothing for instance_eval or module_eval of a block" 
     ] ifFalse:[
       "instance_eval of string, module_eval"
       nargs < 3 ifTrue:[
	 self evalLineArg ifNotNil:[:lnum | args _rubyAt: 2 put: lnum ].
	 nargs < 2 ifTrue:[
	   self evalFileArg ifNotNil:[:fn | args _rubyAt: 1 put: fn ].
	 ].
       ] .
     ]  
   ]

%


set class RubyAbstractCallNode
category: '*maglev-runtime'
method:
evalFileArg
   | cst |
   cst := RubyCompilerState current .
   cst topMethodDefOrNil ifNotNil:[  :mth | |  f n |
     f := mth fileName .
     n := mth lineForOffset: position  .
     ^ RubyStrNode s_a: '(eval) from line ', n asString , ' of ' , f .
   ].
   ^ nil

%


set class RubyAbstractCallNode
category: '*maglev-runtime'
method:
evalLexPathArg
  ^ nil

%


set class RubyAbstractCallNode
category: '*maglev-runtime'
method:
evalLineArg
  | pos lnum  |
  (pos := position) _isInteger ifTrue:[ | cst | 
     cst := RubyCompilerState current .
     cst topMethodDefOrNil ifNotNil:[:mth|  "use implementation in RubyRootNode"
       [ lnum := mth lineForOffset: pos
       ] on: ArgumentError do:[:ex | "ignore" ]
     ]
  ].
  ^ lnum ifNotNil:[ RubyFixnumNode newForInt: lnum ].

%


set class RubyAbstractCallNode
category: '*maglev-runtime'
method:
fullSelector: isStSend
    "caller has already computed irArgNodes and numIrArgs for the receiver"
    | sel  numIrArgs  |
    sel := self selector .
                "trc := sel at: 1 equals: 'aruby_selector_prefix'  ."
    numIrArgs := irArgNodes size .
    isStSend ifTrue:[
       fixedArgCount := numIrArgs .
       lastArgIsBlk := false .
       ^ sel asSymbol 
    ] ifFalse:[ |  hasBlk hasRest hasTooMany maxColons num_colons num_args mask  |
        lastArgIsBlk := ( hasBlk := self hasBlockArg)  .
        hasRest := self hasRestArg or:[ self argIsArgsCat ].
        num_colons :=  hasBlk ifTrue:[ numIrArgs - 1 ] ifFalse:[ numIrArgs ] .
        hasRest ifTrue:[ num_colons := num_colons - 1 ].
        maxColons :=  sel == #__perform__se ifTrue:[ 5 "for RubyParser" ] ifFalse:[ 3 ]. 
        (hasTooMany := num_colons > maxColons) ifTrue:[ 
           num_colons := maxColons .  
        ].
        num_args := num_colons .
        num_colons < 0 ifTrue:[ self error:'negative arg count'].
        mask := num_colons * 4 . 
        hasTooMany ifTrue:[ mask := mask bitOr: 16r2  ]
              ifFalse:[ hasRest ifTrue: [ 
                  mask := mask bitOr: 16r2 . num_args := num_args + 1  
              ]].
        hasBlk ifTrue: [  mask := mask bitOr: 16r1  ].
        fixedArgCount := num_args .
        ^ sel _asSymbolWithRubySuffix: mask 
    ].

%


set class RubyAbstractCallNode
category: 'as yet unclassified'
method:
hasBlockArg
	^ false

%


set class RubyAbstractCallNode
category: 'as yet unclassified'
method:
hasRestArg
	^ false

%


set class RubyAbstractCallNode
category: '*maglev-runtime'
method:
implicitTildeFor: aSymbol
  aSymbol == #last_match ifTrue:[  | ref |
    "For  Regexp.last_match   classmethod"
    ( ref := RubyVcGlobalNode _basicNew ) name:  #'$~'  .
     ^ ref
  ].
  ^ nil 

%


set class RubyAbstractCallNode
category: '*maglev-runtime'
method:
irArgNodes
      "return array of IR nodes for the arguments"
    | lst sz res  |
    lst := argsList . "was computed during walkWithScope: phase "
    lst ifNil:[ self error:'AbstractCallNode.irArgNodes - missed walk args']. 
    res := Array new:(sz := lst size) .
    1 to: sz  do:[:n |  res at: n put: ( lst at: n) irArgNode  ].
    ^ res

%


set class RubyAbstractCallNode
category: '*maglev-runtime'
method:
irDefinedQNode_aCall
  "generate code to return either  'method'  or  nil   at runtime"
  | send sel rcvrBlk |
  sel := self methodName .
  rcvrBlk := self newBlock:[:blk | blk appendStatement: self irReceiverNode . blk ].
  (send := GsComSendNode new)
     rcvr: (GsComLiteralNode newObject: RubyConstantRef ) ;
     stSelector:  #_abstractCall:definedQ:  ;
     appendArgument: rcvrBlk ;
     appendArgument: ( GsComLiteralNode newObject: sel ) .
  self ir: send .
  ^ send 

%


set class RubyAbstractCallNode
category: '*maglev-runtime'
method:
irEvalLexPathLiteral
  | snd |
  (snd := GsComSendNode new)
    rcvr: (GsComLiteralNode newObject: RubyCompilerState current rtModuleEvalLexPath) ;
    stSelector: #shallowCopy  .
  ^ self ir: snd

%


set class RubyAbstractCallNode
category: 'as yet unclassified'
method:
irImplicitBlockArg
  | mth |
   mth := RubyCompilerState current topMethodDef comIrMethNode  . 
  ^ mth ifNotNil:[ GsComVariableNode new leaf: mth arguments last ]
        ifNil:[ GsComLiteralNode newNil ]

%


set class RubyAbstractCallNode
category: '*maglev-runtime'
method:
irNode
      "ruby_selector_suffix dependent"
  | node fullSel  numIrArgs isStSend  rcv  firstCh |
  irArgNodes := self irArgNodes .
  numIrArgs := irArgNodes size .   
  isStSend := self isSmalltalkSend .
  fullSel := self fullSelector: isStSend .  
  firstCh := fullSel at: 1 .
  firstCh == $c ifTrue:[
    ((fullSel == #'coerce_to#3__' or:[ fullSel == #'coerce_to_or_nil#3__' ]) 
           and:[ self rcvr_isTypeClass ]) ifTrue:[
      node := self irForTypeCoerce3Args: fullSel .
      node ifNotNil:[ ^ node ]  .
    ].
  ] ifFalse:[
    firstCh == $n ifTrue:[
      ( fullSel == #'new#0__' and:[ self rcvr_isProcClass ])  ifTrue:[
         ^ self irForProcNewZeroArgs 
      ].
      (fullSel == #'nesting#0__' and:[ self rcvr_isModuleClass]) ifTrue:[
         node := self irForModuleNesting .
         node ifNotNil:[ ^ node ]
      ].
    ] ifFalse:[ 
      fullSel == #'each#0_&' ifTrue:[ | fsArr |
         "convert to call to Object>>__each: which catches RubyBreakException"
         fsArr := { #'__each#0_&' } .  
         rcv := self irReceiverNodeEach: fsArr  .  "may alter fsArr at:1"
         fullSel := fsArr at: 1 .
      ] .
    ].
  ].
  rcv ifNil:[ rcv := self irReceiverNode ].
  (node := GsComSendNode new)  rcvr:  rcv .
  isStSend ifTrue:[
    node stSelector: fullSel 
  ] ifFalse:[ 
     node rubySelector: fullSel toSuper: self isSendSuper .
     self bypassProtection ifTrue:[ node setBypassRubyProtection  ]. 
  ].
  self buildArgumentsOn: node .
  self shouldOptimize ifTrue:[ node optimize]. "control-flow optimiz like if;else;end"
  self  ir: node.               
  ^ node

%


set class RubyAbstractCallNode
category: 'as yet unclassified'
method:
irReceiverNode
	^ self receiverNode irEvaluatedRcvrNode

%


set class RubyAbstractCallNode
category: 'as yet unclassified'
method:
irReceiverNodeEach: fullSelArray
  ^ self irReceiverNode

%


set class RubyAbstractCallNode
category: '(as yet unclassified)'
method:
isSendSuper
  ^ false

%


set class RubyAbstractCallNode
category: 'as yet unclassified'
method:
isSmalltalkSend
	^ false

%


set class RubyAbstractCallNode
category: 'as yet unclassified'
method:
methodName
  ^ nil

%


set class RubyAbstractCallNode
category: 'as yet unclassified'
method:
pathArray
  ^ { }  " for dynamicColon2"

%


set class RubyAbstractCallNode
category: '*maglev-runtime'
method:
rcvr_isModuleClass
  ^ false

%


set class RubyAbstractCallNode
category: 'as yet unclassified'
method:
rcvr_isProcClass
  ^ false

%


set class RubyAbstractCallNode
category: 'as yet unclassified'
method:
rcvr_isTypeClass
  ^ false

%


set class RubyAbstractCallNode
category: 'as yet unclassified'
method:
receiverNode
	^ nil

%


set class RubyAbstractCallNode
category: 'as yet unclassified'
method:
shouldOptimize
	^ false

%


set class RubyAbstractCallNode
category: '*maglev-runtime'
method:
walkCallArgs: lst withScope: aScope
  | nargs |
  1 to: (nargs := lst size) - 1 do:[:n |
    (lst at: n) walkWithScope: aScope .
  ].
  nargs ~~ 0 ifTrue:[ | lastarg |
    (lastarg := lst at: nargs) walkWithScope: aScope .
    self hasBlockArg ifTrue:[
      lastarg postWalkForYield.  "does not need to_proc conversion"
    ].
  ].

%


set class RubyAbstractCallNode
category: '*maglev-runtime'
method:
walkEvalCall: aScope
  "Returns a new lexPathVar "
  | cst |
  cst := RubyCompilerState current .
  cst topMethodDef  setSendsBinding ; setHasBlockArgRef ; setHasInnerEvalOrDef .
  ^ ( RubyVcGlobalNode _basicNew ) name: #'__lexPath' 

%


set class RubyAbstractCallNode
category: '*maglev-runtime'
method:
walkFixedInstVars: list forClass: aBody
 | ary sz |
 aBody class == RubyClassBodyNode ifFalse:[
   self signal: ArgumentError with: '__fixed_instvars only legal in a class body'
 ].
 sz := list size .
 sz == 0 ifTrue:[
   self signal: ArgumentError with: '__fixed_instvars requires at least one arg'
 ].
 1 to: sz do:[:j | | elem ecls |
   elem := list at: j  .
   ecls := elem class .
   (ecls == RubySymbolNode or:[ ecls == RubyStrNode]) ifTrue:[ | val |
     val := elem _value .
     ((val _rubyAt1: 0 ) == 64"$@" and:[(val _rubyAt1: 1) ~~ 64 ]) ifFalse:[
       self signal: ArgumentError
          with: 'illegal instVar name, element ' , j asString ,' of list'
     ]
   ] ifFalse:[
     self signal: ArgumentError
        with: 'args to __fixed_instvars must be String or Symbol constants' .
   ]
 ].
 (ary := RubyArrayNode _new) list: list copy .
 (aBody classNode fixedInstVars: ary) ifFalse:[ 
    self signal: ArgumentError with: 'second call to __fixed_instvars found in class body'
 ]

%


set class RubyAbstractCallNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  "subclasses which contain children 
     not returned by either argNodes or receiverNode methods
   must reimplement walkWithScope: "
  | lst mth knd nam cst | 
  self receiverNode walkWithScope: aScope .
  lst := self argNodes .
  self walkCallArgs: lst withScope: aScope .
  self evalLexPathArg ifNotNil:[ :lp |
    (argsList :={ lp }) addAll: lst .  
  ] ifNil:[
    argsList := lst .
  ].
  mth := (cst := RubyCompilerState current) topMethodDefOrNil.  
  knd := SpecialSelectorKinds at: (nam := self methodName) otherwise: 0 .
  "(nam at:1 equals:'__fixed_instvars') ifTrue:[ self pause ].  "
  knd == 0 ifFalse:[
      knd == 1 ifTrue:[ 
         mth ifNotNil:[ | sel |
            sel := mth selectorPrefix . 
            ((nam == #initialize and:[ sel _at:1 equals:'new' ]) or:[ sel _at:1 equals:'__']) ifTrue:[
             "implementations of  #new  and internal methods can invoke private #initialize "
             bypassProt := true .
           ]
        ].       
        nam == #initialize_copy ifTrue:[ bypassProt := true ].
      ] ifFalse:[
         knd == 2 ifTrue:[  
           mth ifNotNil:[ mth setSendsBinding "for an eval" ]
               ifNil:[ self error:'inconsistent special call node kind' ].
         ] ifFalse:[  
           knd == 3 ifTrue:[
             "fixed instvars defn"
             self walkFixedInstVars: lst forClass: mth .
           ] ifFalse:[ self error: 'invalid special call node kind'  ].
      ].
    ].
  ].

%

