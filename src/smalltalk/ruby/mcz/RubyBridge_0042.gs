
set class RubyBridge class
category: '*maglev-runtime'
method:
execMethBridgeTo: aMethod
      "ruby_selector_suffix dependent"
  | br toSuff toSel ch optArgsDescr argsDescrInt toNargs |
  toSel := aMethod selector .
  toSuff :=  toSel suffixIfRubySelector .
  toSuff = '#0*&' ifTrue:[ ^ nil "no bridge needed" ].

  optArgsDescr := aMethod rubyOptArgsBits .
  toNargs := aMethod numArgs .
  argsDescrInt := 0 .
  optArgsDescr ~~ 0 ifTrue:[ | idx |
    idx := 1 .
    [ (optArgsDescr bitAt: idx) == 0 ] whileTrue:[ idx := idx + 1 ]. 
    argsDescrInt := argsDescrInt bitOr:(  16r100 bitShift: idx - 1 ) .
  ].
  toSuff ifNotNil:[
    (ch := toSuff at: 4) == $& ifTrue:[ 
       toNargs := toNargs - 1 .
       argsDescrInt := argsDescrInt bitOr: 16r100000 .
    ]. 
    (ch := toSuff at: 3) == $* ifTrue:[ 
       toNargs := toNargs - 1 .
       argsDescrInt := argsDescrInt bitOr: 16r10000
    ].
  ].
  argsDescrInt := argsDescrInt bitOr: (toNargs bitAnd: 16rFF) .
  br := ((BridgeOptions at: 1) at: 16) copy .
  br setTo: toSel suffix: toSuff argsDesc: argsDescrInt primKind: 0 .
  br from: #'call#0*&' .  "actually _executeInContext:nonBridgeMeth:star:block: "
  ^ br execMethodBridge: optArgsDescr env: 1 .

%


set class RubyBridge class
category: 'as yet unclassified'
method:
genericErrCmSet
	"Return the identity set of generic error bridge compiled methods."
	^ GenericErrCmSet.

%


set class RubyBridge class
category: '*maglev-runtime'
method:
initialize

  BridgeOptions := { } .
  GenericErrCms := { } .
  GenericErrCmSet := IdentitySet new .

%


doit
RubyBridge initialize.
%


set class RubyBridge class
category: '*maglev-runtime'
method:
initializeGenericBridges: envId
  "creates and returns the bridge options for specified environment"
  |  bridges suffixes sz generics comp frSym aBr  |
  comp :=   RubyCompiler new .
  suffixes := self suffixOptions.
  sz := suffixes size .
  bridges := Array new: sz .
  1 to: sz do: [:n |
   frSym := suffixes at: n .
   (aBr := RubyBridge _basicNew) fromSuffix: frSym ;  immediateInvariant .
   bridges at: n put: aBr
  ].
  bridges immediateInvariant .
  BridgeOptions _rubyAt: envId - 1 put: bridges  .
  generics := IdentityKeyValueDictionary new .  "key toSuffix,
        value is Array of genericErrCms or nil for each fromSuffix in suffixOptions "
  1 to: sz do:[ :m | | toSym cmArr |
   toSym := suffixes at: m .
   cmArr := Array new: sz .
   1 to: sz do:[:k | |   cm ir  |
    frSym := suffixes at: k .
    frSym ~~ toSym ifTrue:[
        (aBr := (bridges at: k) shallowCopy) setTo: ('__meth', toSym) asSymbol
                                 suffix: toSym argsDesc: nil primKind: 0 .
        (ir :=  aBr irGenericErrMethod: ('__meth', frSym) asSymbol
                    env: envId ) ifNotNil:[
         ir class: Object ; addMethodProtection: 0 .
           cm :=  comp  compiledMethodForIR: ir .
           cm ifNotNil:[
               cmArr at: k put: cm .
               GenericErrCmSet add: cm .
             ].
        ].
      ].
   ].
     cmArr immediateInvariant .
     generics at: toSym put: cmArr .
  ].
  GenericErrCms _rubyAt: envId - 1 put: generics .
  ^ bridges

%


set class RubyBridge class
category: '*maglev-runtime'
method:
installBridgesFor: rubySel in: aClass argsDescr: argsDescrInt 
    optArgs: optArgsDescr protection: protInt primKind: primKnd env: envId
 "primKnd argument is one of  0:normal 1:primitive 2:primitive_nobridge 
                       4:boot 5:primitive during boot , 6: primitive_nobridge during boot .
  Returns selectorPrefix Symbol of rubySel .
 "
 | prefix destSuffix |
 prefix :=  rubySel rubySelectorPrefixSymbol . 
 destSuffix := (rubySel copyFrom:( prefix size + 1) to: rubySel size ) asSymbol .
 ^ self installBridgesForPrefix: prefix suffix: destSuffix selector: rubySel
    in: aClass argsDescr: argsDescrInt
    optArgs: optArgsDescr protection: protInt primKind: primKnd env: envId

%


set class RubyBridge class
category: '*maglev-runtime'
method:
installBridgesForPrefix: prefix suffix: destSuffix selector: rubySelArg
    in: aClass argsDescr: argsDescrInt
    optArgs: optArgsDescr protection: protInt primKind: primKnd env: envId
 "primKnd argument is one of  0:normal 1:primitive 2:primitive_nobridge
                       4:boot 5:primitive during boot , 6: primitive_nobridge during boot .
  Returns prefix" 
 | suffixes generics sz templates useGenerics  aComp masks rubySel |
 templates := BridgeOptions atOrNil: envId .  "instances with fromSuffix predefined"
 templates ifNil:[
   templates := self initializeGenericBridges: envId
 ].
    "if we have optional args, then we must produce IR for a new bridge evaluate
     the optional args logic.  But if that IR would signal an ArgumentError ,
     and protInt==0, then   we use the precompiled generic method if one was found."
 ( optArgsDescr + protInt) == 0 ifTrue:[ useGenerics := true ].
 generics := (GenericErrCms at: envId) at: destSuffix otherwise: nil .
 suffixes := self suffixOptions . masks := self suffixOptionMasks .
 rubySel := rubySelArg ifNil:[  (prefix , destSuffix) asSymbol ].
 1 to: (sz := suffixes size) do:[ :n | | frSuffix  |
    frSuffix := suffixes at: n .
    frSuffix ~~ destSuffix ifTrue:[ | frSel cm genErrCm  |
      frSel :=  prefix _asSymbolWithRubySuffix: ( masks at: n ) .
      cm :=  primKnd >=4 ifTrue:[ "during bootstrap,  don't overwrite existing variants"
        aClass compiledMethodAt: frSel rubyEnv: envId .
      ].
      cm ifNil:[
        generics ifNotNil:[  genErrCm := generics at: n ].
        useGenerics ifNotNil:[ cm := genErrCm ].
        cm ifNil:[ | br |
          (br := (templates at: n) shallowCopy ) 
             setTo: rubySel suffix: destSuffix argsDesc: argsDescrInt primKind: primKnd .
          br from: frSel .
          aComp ifNil:[ aComp := RubyCompiler new ].
          cm := br cmForOptArgs: optArgsDescr protection: protInt class: aClass 
                     genericErrCm: genErrCm compiler: aComp env: envId .
        ].
          "frSel == #'=~:' ifTrue:[ self pause . SessionTemps current at:#TrapBR put: true
  ]."
        aClass addRubySelector: frSel method: cm  env: envId 
  
      ].
   ].
 ].
 ^ prefix

%


set class RubyBridge class
category: '*maglev-runtime'
method:
irMethod_defineMethStarArgs: selPrefix block: aBlock inClass: aClass  env: envId
   "aBlock must have been produced by _copyForRuby:1   "
      "ruby_selector_suffix dependent"
    | node sel sndSet sndCall argsLeaf blkLit srcStr rtn blkArgLeaf |
    sel := selPrefix _asSymbolWithRubySuffix: 16r3  "(prefix , '#0*&') asSymbol " .
    srcStr := '<a define_method >
' , aBlock _debugSourceForBlock .
    (node := GsComMethNode newRuby)
       environment: envId ; class: aClass ;  selector: sel  ;
       source: srcStr ; sourceOffset: 1 .
   argsLeaf := GsComVarLeaf new methodArg: #args argNumber: 1 .
   blkArgLeaf := GsComVarLeaf new methodArg: #block argNumber: 2 .
    node appendArg:  argsLeaf ; appendArg: blkArgLeaf .
    blkLit := GsComLiteralNode newObject: aBlock .  
    (sndSet := GsComSendNode new)
       rcvr: blkLit ;
       stSelector:  #setSelf:  ;
       appendArgument: ( GsComVariableNode newSelf ) ;
       sourceOffset: 1 . 
    (sndCall := GsComSendNode new)
       rcvr: sndSet ;
       stSelector:  #'_rubyCall:block:' ;
       appendArgument: (GsComVariableNode new leaf: argsLeaf) ;
       appendArgument: (GsComVariableNode new leaf: blkArgLeaf) ;
       sourceOffset: srcStr size .
    rtn := sndCall returnNode .
    rtn sourceOffset: srcStr size  - 5 .
    node  sourceOffset: 1 ;
      appendStatement: rtn ;
      endSourceOffset: srcStr size .
    ^ node

%


set class RubyBridge class
category: '*maglev-runtime'
method:
suffixOptionMasks 
    "result is an array of masks for use as arg to _asSymbolWithRubySuffix: 
      to match the above array of suffixes returned by  RubyBridge(C)>>suffixOptions"
      "ruby_selector_suffix dependent"
    
^ #( 0 4 8 16rC    
     16rD 16rE 16rF
     9 16rA 16rB    5    6    7     1  2  3 )   

%


set class RubyBridge class
category: '*maglev-runtime'
method:
suffixOptions
      "ruby_selector_suffix dependent"
    ^  #( #'#0__' #'#1__' #'#2__' #'#3__'        
          #'#3_&' #'#3*_' #'#3*&' 
          #'#2_&' #'#2*_' #'#2*&'   #'#1_&' #'#1*_' #'#1*&'    #'#0_&' #'#0*_' #'#0*&' )

%


set class RubyBridge class
category: '*maglev-runtime'
method:
_templates: envId
  ^ BridgeOptions _rubyAt: envId - 1

%


set class RubyBridge
category: 'as yet unclassified'
method:
argLeaf: aNumber
	^ leaves at: aNumber ifAbsentPut:[ | sym |
		sym := #( t1 t2 t3 t4 t5 t6 ) atOrNil: aNumber .
		sym ifNil:[ sym := ('t', aNumber asString) asSymbol ].
		GsComVarLeaf new methodArg: sym argNumber: aNumber
	  ]

%


set class RubyBridge
category: 'as yet unclassified'
method:
argNode: aNumber
	^ GsComVariableNode new leaf: (self argLeaf: aNumber)

%


set class RubyBridge
category: '(as yet unclassified)'
method:
blockLeaf
    | idx |
    ^ leaves at: #block ifAbsentPut:
        [idx :=  fromRestArg
                ifTrue: [ fromNargs  + 2]
                ifFalse: [ fromNargs + 1].
        GsComVarLeaf new methodArg: #block argNumber: idx]

%


set class RubyBridge
category: 'as yet unclassified'
method:
blockNode
	^ GsComVariableNode new leaf: self blockLeaf

%


set class RubyBridge
category: '*maglev-runtime'
method:
buildArgumentsOn: irNode argsDescr: optArgsDescr meth: irMethNode 
   "returns boolean tooMany args"
    | inArgsN nFrom moreOutArgs excessInArgs nNeeded argIdx k tooMany firstOpt restUsed |
    nFrom := fromNargs .      tooMany := false . 
    inArgsN := nFrom min: (nNeeded := neededArgs ) .  
    1 to: inArgsN do: [:i |  irNode appendArgument: (self argNode: i).  "args from caller" ].
    moreOutArgs := (nNeeded - inArgsN) max: 0.       argIdx := inArgsN .
    optArgsDescr ~~ 0 ifTrue:[   "callee has default initializers on some args"
      (firstOpt := firstOptArg) ifNil:[             k := 1 .  
         [ k <= moreOutArgs] whileTrue:[ | isOpt |
           isOpt := (optArgsDescr bitAt: argIdx + k) == 1 .
           (isOpt and:[ firstOpt == nil ]) ifTrue:[ 
               firstOpt := argIdx + k.       k := moreOutArgs + 1 "exit while loop" ].
           k := k + 1 .
      ] ].
      firstOpt ifNil:[ firstOpt := SmallInteger maximumValue  ].
      1 to: moreOutArgs do:  [:k | | aargNode isOptArg  |  "needed by callee, not passed by caller"
        argIdx := argIdx + 1 .      restUsed := true .
        isOptArg := (optArgsDescr bitAt: argIdx ) == 1 .
        aargNode := self restArgNode: k forOptArg: isOptArg beforeFirstOpt: (argIdx < firstOpt) .
        irNode appendArgument: aargNode .  ].
    ] ifFalse:[  "no default initializers on callee's args"
        1 to: moreOutArgs do:  [:k | | aargNode   |   "needed by callee, not passed by caller"
          argIdx := argIdx + 1 .       restUsed := true .
          aargNode := self restArgNode: k forOptArg: false beforeFirstOpt: true .
          irNode appendArgument: aargNode .
    ].  ].
    excessInArgs := ( nFrom - inArgsN) max: 0.
    needsRestArg ifTrue:[   "callee wants  a *arg  as last arg"
       irNode appendArgument: 
         (self buildRestAfter: inArgsN extraIn: excessInArgs extraOut: moreOutArgs). 
    ] ifFalse:[  
       excessInArgs ~~ 0 ifTrue:[  tooMany := true ]
         ifFalse:[ restUsed ifNil:[ fromRestArg ifTrue:[ self buildCheckRestEmpty: irMethNode]]].
    ].
    needsBlockArg ifTrue:[  "callee wants a &blk  as last arg"    
       fromBlockArg ifTrue: [irNode appendArgument: self blockNode ]
                  ifFalse: [irNode appendArgument: GsComLiteralNode newNil ]
    ] ifFalse:[
       "NOT USED:  optional block arg not allowed to primitive_nobridge"
       "((primKnd bitAnd:16r2) ~~ 0 and:[fromBlockArg]) ifTrue:[ tooMany := true ].  "
    ].
     ^ tooMany

%


set class RubyBridge
category: '*maglev-runtime'
method:
buildBodyOn: irNode argsDescr: optArgsDescr 
   "returns boolean isErr  if the bridge method will raise an ArgumentError"
    | send err tooMany tooFew isErr |
    send := GsComSendNode new .
    tooMany := self buildArgumentsOn: send argsDescr: optArgsDescr meth: irNode .
    tooFew := missingArgsError.
    (tooMany or:[ tooFew == true]) ifTrue:[  
      send := self irArgumentErrorNode: tooMany == true .
      isErr := true .
    ] ifFalse:[ 
       send rcvr:  (GsComVariableNode new leaf: (GsComVarLeaf new initializeSelf));
             rubySelectorInBridge: to .
       isErr := false .
       irNode nonBridgeSelector: to .
    ].
    irNode appendStatement: send returnNode .
    ^ isErr 

%


set class RubyBridge
category: '*maglev-runtime'
method:
buildCheckRestEmpty: irMethNode
  | sizNode tstNode ifNode blkNode   |
  (sizNode := GsComSendNode new) 
     rcvr:  self restNode ;
     stSelector: #size .
  ( tstNode := GsComSendNode new )
     rcvr: sizNode ;
     stSelector:  #==  ;
     appendArgument: (GsComLiteralNode newInteger: 0) .
  ( blkNode := GsComBlockNode new) 
     lexLevel: 1 ;
     appendStatement: ( self irArgumentErrorNode:  true"too many" ) .
  ( ifNode := GsComSendNode new )
     rcvr: tstNode ;
     stSelector:  #ifFalse:  ;
     appendArgument: blkNode ;
     optimize .

  irMethNode appendStatement: ifNode .

%


set class RubyBridge
category: '*maglev-runtime'
method:
buildExecBodyOn: irNode argsDescr: optArgsDescr 
  | send err tooMany tooFew isErr |
  (send := GsComSendNode new )
     rcvr: (GsComVariableNode new leaf: (self argLeaf: 2 "dest method")) ;
     stSelector: #_executeInContext:args: ;
     appendArgument: (GsComVariableNode new leaf: (self argLeaf: 1 "contextObj"));
     appendArgument:(   self buildExecRest: optArgsDescr ).
  irNode appendStatement: send returnNode .

%


set class RubyBridge
category: '*maglev-runtime'
method:
buildExecRest: optArgsDescr 
    | moreOutArgs nNeeded argIdx k firstOpt restUsed aryB |
    aryB := GsComArrayBuilderNode _basicNew initialize . 
    fromNargs ~~ 0 ifTrue:[ self error:'expect 0 incoming colons'].
    fromRestArg ifFalse:[ self error:'expected incoming star' ].
    nNeeded := neededArgs .
    moreOutArgs := nNeeded  . argIdx := 0 .
    optArgsDescr ~~ 0 ifTrue:[   "callee has default initializers on some args"
      (firstOpt := firstOptArg) ifNil:[             k := 1 .  
         [ k <= moreOutArgs] whileTrue:[ | isOpt |
           isOpt := (optArgsDescr bitAt: argIdx + k) == 1 .
           (isOpt and:[ firstOpt == nil ]) ifTrue:[ 
               firstOpt := argIdx + k.       k := moreOutArgs + 1 "exit while loop" ].
           k := k + 1 .
      ] ].
      firstOpt ifNil:[ firstOpt := SmallInteger maximumValue  ].
      1 to: moreOutArgs do:  [:k | | aargNode isOptArg  |   "args needed by callee, not passed by caller"
        argIdx := argIdx + 1 .      restUsed := true .
        isOptArg := (optArgsDescr bitAt: argIdx ) == 1 .
        aargNode := self restArgNode: k forOptArg: isOptArg beforeFirstOpt: (argIdx < firstOpt) .
        aryB appendElement: aargNode .  
      ].
    ] ifFalse:[  "no default initializers on callee's args"
        1 to: moreOutArgs do:  [:k | | aargNode   |   "args needed by callee, not passed by caller"
          argIdx := argIdx + 1 .       restUsed := true .
          aargNode := self restArgNode: k forOptArg: false beforeFirstOpt: true .
          aryB appendElement: aargNode .
        ].  
    ].
    needsRestArg ifTrue:[   "callee wants  a *arg  as last arg"
        aryB appendElement: (self buildRestAfter: 0 extraIn: 0 extraOut: moreOutArgs) .
    ].
    needsBlockArg ifTrue:[  "callee wants a &blk  as last arg"
      "callee wants a &blk  as last arg"    
       fromBlockArg ifTrue: [ aryB appendElement: self blockNode ]
                  ifFalse: [ self error:'expected incoming block' ].
    ].
    needsRestArg ifTrue:[
      ^ aryB  "buildRestAfter:... done above"
    ] ifFalse:[ | send |
      "add remaining args and let too many args be signaled by execMethod primitive"
       (send := GsComSendNode new) rcvr: aryB ;
          stSelector:  #addAll:excludingFirst: ;
          appendArgument: self restNode ;
          appendArgument: (GsComLiteralNode newInteger: moreOutArgs) .
       ^ send
    ].

%


set class RubyBridge
category: '(as yet unclassified)'
method:
buildMethodArgumentsOn: irNode
    1 to: fromNargs  do: [:i | irNode appendArg: (self argLeaf: i)].
    fromRestArg ifTrue: [irNode appendArg: self restLeaf ].
    fromBlockArg ifTrue: [irNode appendArg: self blockLeaf]

%


set class RubyBridge
category: '*maglev-runtime'
method:
buildRestAfter: inArgsUsed extraIn: excessInArgs  extraOut: moreOutArgs
  | aryB snd |
  excessInArgs ~~ 0 ifTrue:[ 
    aryB := GsComArrayBuilderNode new.
    1 to:  excessInArgs do: [:m | aryB appendElement: (self argNode: inArgsUsed + m )  ].
    fromRestArg ifTrue:[
     (snd := GsComSendNode new) rcvr: aryB .
      moreOutArgs == 0 ifTrue:[
        snd  stSelector:  #_rubyAddAll: ; appendArgument: self restNode .
      ] ifFalse:[
       snd stSelector: #addAll:excludingFirst: ;
          appendArgument: self restNode ;
          appendArgument: (GsComLiteralNode newInteger: moreOutArgs) .
      ].
      ^ snd
    ] ifFalse:[
      ^ aryB
    ]
  ] ifFalse:[
    moreOutArgs == 0 ifTrue:[
      fromRestArg ifTrue:[ 
        ^ GsComVariableNode new leaf: (self restLeaf) 
      ] ifFalse:[
        ^ GsComArrayBuilderNode new
      ].
    ] ifFalse:[
      fromRestArg ifTrue:[
        aryB := GsComArrayBuilderNode new.
        (snd := GsComSendNode new) rcvr: aryB ;
          stSelector:  #addAll:excludingFirst: ;
          appendArgument: self restNode ;
          appendArgument: (GsComLiteralNode newInteger: moreOutArgs) .
        ^ snd
      ] ifFalse:[
        ^ GsComArrayBuilderNode new 
      ].
    ].
  ]. 

%


set class RubyBridge
category: '*maglev-runtime'
method:
cmForOptArgs: optArgsDescr protection: protInt class: aClass genericErrCm: genericErrCm 
    compiler: aComp env: envId
   | node cm isErr   | 
   (node := GsComMethNode newRuby)
     environment: envId ;
     selector_forBridge: from env: envId ;
     setRubyBridge ; 
     source: 
'<a Ruby bridge method>
  ' .
  self buildMethodArgumentsOn: node .
  isErr := self buildBodyOn: node argsDescr: optArgsDescr .
  (isErr and:[ genericErrCm ~~ nil and:[ protInt==0]]) ifTrue:[  ^ genericErrCm ] .

  node class: aClass ; 
         addMethodProtection: protInt .
  cm := aComp compiledMethodForIR: node .
  "cnt := System _sharedCounter: 1 incrementBy: 1 .  " " debugging stat"
  " GsFile gciLogServer:'bridge ' , cnt asString , ':  '  ,  fromSuffix , ' --> ' ,  to 
               , (isErr ifTrue:[ '   err '] ifFalse:[ ' ' ])  ."
  ^ cm

%


set class RubyBridge
category: '*maglev-runtime'
method:
execMethodBridge: optArgsDescr env: envId
   | node cm | 
   (node := GsComMethNode newRuby)
     environment: envId ;
     selector: from  ; sourceOffset: 1 ;
     source: 
'<an execMethod  bridge method>
  ' .
  "inline buildMethodArgumentsOn:"
  node appendArg: (self argLeaf: 1 "ctx") ; appendArg:(self argLeaf: 2 "nonBrMeth") ;
     appendArg: self restLeaf ; appendArg: self blockLeaf .

  self buildExecBodyOn: node argsDescr: optArgsDescr .
  node class: RubyUnboundMeth .
  cm := RubyCompiler new  compiledMethodForIR: node .
  ^ cm

%


set class RubyBridge
category: 'as yet unclassified'
method:
from: aSymbol
  from := aSymbol  . 
     "fromSuffix and other related from-values should already be set"

%


set class RubyBridge
category: '*maglev-runtime'
method:
fromSuffix
  ^ fromSuffix

%


set class RubyBridge
category: '*maglev-runtime'
method:
fromSuffix:  aSymbol
      "ruby_selector_suffix dependent"

  fromNargs := (aSymbol codePointAt: 2) - 48"$0" .
  fromRestArg :=  (aSymbol at: 3) == $* .
  fromBlockArg := (aSymbol at: 4) == $& .

%


set class RubyBridge
category: '*maglev-runtime'
method:
irArgumentErrorNode: tooManyBool
  | send sel  |
  sel := tooManyBool ifTrue:[ #signalTooManyArgs ] ifFalse:[ #signalTooFewArgs ].
  ( send := GsComSendNode new)
         rcvr: ( GsComLiteralNode newObject: ArgumentError ) ;
         stSelector:   sel .
  ^ send

%


set class RubyBridge
category: '*maglev-runtime'
method:
irGenericErrMethod: fromSelector env: envId
  | node isErr |
  (node := GsComMethNode newRuby)
     environment: envId ;
     selector: fromSelector ;
     setRubyBridge ;
     source: 
'<a Ruby bridge method>
  ' .
  self buildMethodArgumentsOn: node .
  isErr := self buildBodyOn: node argsDescr: 0"default optArgDescr" .
    ^ isErr ifTrue:[ node ] ifFalse:[  nil ].
    

%


set class RubyBridge
category: 'as yet unclassified'
method:
noArgNode
	^ GsComLiteralNode new leaf: (GsComLitLeaf new specialLiteral:  UndefinedObject noArgNil )

%


set class RubyBridge
category: '(as yet unclassified)'
method:
numArgs
   ^ fromNargs

%


set class RubyBridge
category: '*maglev-runtime'
method:
restArgNode: argNum forOptArg:  isOptArg beforeFirstOpt: beforeFirstOpt
  "build arg for an arg needed by callee,  not passed by caller.
   isOptArg is true if the argument has a 
   default value assignment in the parameter declaration.
   argNum is one based "
  | stSel stArg |
  fromRestArg ifFalse: [
   ^ isOptArg ifTrue:[  self noArgNode  ]
       ifFalse:[ (needsRestArg or:[ beforeFirstOpt]) ifTrue:[ missingArgsError := true ].
                 GsComLiteralNode newNil ].
  ].
  stArg := argNum .
  isOptArg ifTrue:[ 
     stSel := #atOrNoArg: .  
     ( fromRestArg and:[ needsRestArg ~~ true ])  ifTrue:[ 
           stArg := stArg bitOr: ((neededArgs + 1) bitShift: 8)   "encode maxArgs"
     ].
  ] ifFalse:[ 
     beforeFirstOpt ifTrue:[
       stSel := #atOrMissingArgErr: . 
       ( fromRestArg and:[ needsRestArg ~~ true ])  ifTrue:[ 
           stArg := stArg bitOr: ((neededArgs + 1) bitShift: 8)   "encode maxArgs"
       ].
     ] ifFalse:[ 
       stSel := #atOrNil: 
   ] ].
  ^ GsComSendNode new
      rcvr: self restNode ;  "at runtime rcvr will be an Array"
      stSelector:  stSel  ;
    appendArgument: (GsComLiteralNode newInteger: stArg)

%


set class RubyBridge
category: 'as yet unclassified'
method:
restLeaf
	^ leaves at: #rest ifAbsentPut:
		[GsComVarLeaf new methodArg: #rest argNumber: self numArgs + 1]

%


set class RubyBridge
category: 'as yet unclassified'
method:
restNode
	^ GsComVariableNode new leaf: self restLeaf

%


set class RubyBridge
category: '*maglev-runtime'
method:
setTo: aSymbol suffix: aSuffix argsDesc: argsDescrInt primKind: primArg
  "aSuffix is suffix of aSymbol, i.e. suffix of the non-bridge method
  argsDescrInt is from argsNode of a RubyMethodDefNode's argsNode "
      "ruby_selector_suffix dependent"

  leaves := IdentityKeyValueDictionary new .
  to := aSymbol .
  toSuffix := aSuffix .
  primKnd := primArg .
  argsDescrInt ifNotNil:[ | fopt |
        neededArgs := argsDescrInt bitAnd: 16rFF .
        fopt := (argsDescrInt >> 8) bitAnd: 16rFF .
        fopt == 0 ifTrue:[ fopt := SmallInteger maximumValue ].
        firstOptArg := fopt .
        needsBlockArg := (argsDescrInt  bitAnd: 16r100000) ~~ 0 .
        needsRestArg := (argsDescrInt  bitAnd: 16r10000) ~~ 0 .  
  ] ifNil:[ 
    neededArgs := (aSuffix codePointAt: 2) - 48"$0" .
    needsRestArg  :=  (aSuffix at: 3) == $* .
    needsBlockArg :=  (aSuffix at: 4) == $& .
  ].

%

