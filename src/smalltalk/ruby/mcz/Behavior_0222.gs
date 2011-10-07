
set class Behavior
category: '*maglev-runtime'
classmethod:
traceRubyMethodDicts: aBoolean

  "Set value of the class variable which controls tracing of changes to
   enviroment 1 method dicts.  tracing is via GsFile gciLogServer:   "

  aBoolean _validateClass: Boolean .
  Error signal:'TraceRubyMethodDicts disabled' .

%


set class Behavior
category: '*maglev-runtime'
method:
addRubyInstVar: aSymbol

  "Adds a new instance variable named aSymbol to the receiver. 
 caller must have gotten true from  isModifiable_oops_notModule "

| offset ivn |
aSymbol _isSymbol ifFalse:[ ArgumentTypeError signal:'instVar name is not a Symbol'].
(ivn := instVarNames)  .
( ivn includesIdentical: aSymbol ) ifTrue:[
    ^ self _error: #rtErrAddDupInstvar args:{ aSymbol }
].
self _incrementInstVars: 1 .
offset := self instSize .  "the offset of the new instance variable"
ivn == #() ifTrue:[ 
  ivn := { } .  instVarNames := ivn .
].
ivn insertObject: aSymbol at: offset .
constraints == #() ifFalse:[ constraints insertObject: Object  at: offset ].
self _refreshClassCache: false .
  "no recompilation of methods nor handling of subclasses needed"
^ self

%


set class Behavior
category: '*maglev-runtime'
method:
addRubySelector: selectorSymbol env: envId smalltalkMethod: stMethod
  ^ self addRubySelector: selectorSymbol method: stMethod env: envId

%


set class Behavior
category: '*maglev-runtime'
method:
addRubySelector: selectorSymbol method: aGsMethod env: envId 
  "aGsMethod may be a SmallInt Ruby protection override"
  | md pmd tmeth pmeth  |
  md := self transientMethodDictForStoreEnv: envId .
  tmeth := md at: selectorSymbol otherwise: nil .
  md at: selectorSymbol put: aGsMethod .
  (RubyCompilerState current persistenceMode and:[ self _persistable]) ifTrue:[
     pmd := self persistentMethodDictForStoreEnv: envId .
     pmeth := pmd at: selectorSymbol otherwise: nil .
     pmeth == tmeth ifTrue:[ pmeth := nil ].
     pmd at: selectorSymbol put: aGsMethod .
     self _codeChangedForEnv: envId .
  ].
  tmeth ifNotNil:[ tmeth _isSmallInteger ifFalse:[
    self _refreshLookupCache: nil oldMethod: tmeth  env: envId "clear breakpoints"
  ]].
  self _refreshLookupCache: selectorSymbol oldMethod: pmeth   env: envId .

" selectorSymbol == #aa_class_method ifTrue:[ self pause ]."
 "TraceRubyMethodDicts ifTrue:[
    GsFile gciLogServer: self name, '  addRubySelector: ' , selectorSymbol printString ,
        ' methSel ', aGsMethod selector , '  methId:', aGsMethod asOop asString 
  ]."

%


set class Behavior
category: '*maglev-runtime'
method:
baseCompiledMethodAt: aSelector environmentId: envId

"Returns the compiled method associated with the argument aSelector (a String),
 not searching in the PackagePolicy .  Returns nil if no method found."

^ self compiledMethodAt: aSelector environmentId: envId otherwise: nil usePackages: false .

%


set class Behavior
category: '*maglev-runtime'
method:
compiledMethodAt: aSymbol rubyEnv: envId

  "does not consider the package policy"
  | cls meth md |
  (md := self transientMethodDictForEnv: envId) ifNotNil:[
    meth := md at: aSymbol otherwise: nil .
    meth ifNotNil:[ ^ meth ].
  ]. 
  (md := self persistentMethodDictForEnv: envId ) ifNotNil:[
    meth := md at: aSymbol otherwise: nil .
    meth ifNotNil:[ ^ meth ].
  ].
  (format bitAnd: 16r14000) == 0 ifTrue:[ ^ nil ].
  cls := primaryCopy "RUBY_VIRTUAL|MODULE_inclSelf"  .
  cls ifNil:[  ^ nil ].
  
  (md := cls transientMethodDictForEnv: envId) ifNotNil:[
    meth := md at: aSymbol otherwise: nil . 
    meth ifNotNil:[ ^ meth ].
  ].
  (md := cls persistentMethodDictForEnv: envId ) ifNotNil:[
    meth := md at: aSymbol otherwise: nil . 
  ].
  ^ meth

%


set class Behavior
category: '*maglev-compiling'
method:
compileMethod: source category: cat environmentId: anEnvironmentId

	^ self compileMethod: source
      dictionaries: System myUserProfile symbolList
      category: cat
      environmentId: anEnvironmentId

%


set class Behavior
category: '*maglev-runtime'
method:
defineMethod: aSymbol block: blockArg
  "a ruby primitive"
  ^ self defineMethod: aSymbol block: blockArg env: 1"__callerEnvId"

%


set class Behavior
category: '*maglev-runtime'
method:
defineMethod: aSymbol block: blockArg  env: envId
  | newSym ir cm comp protInt aBlock  optBits newBlkMeths res |
      "ruby_selector_suffix dependent"
  "do not  aSymbol rubySelectorPrefix - fix Trac 913 "
  newSym := aSymbol _asSymbolWithRubySuffix: 16r3 " (prefix , '#0*&') asSymbol " .
  "take a copy of block and it's VariableContext"
  aBlock := blockArg _copyForRuby: aSymbol newBlockMethsInto: (newBlkMeths := { } ) .
  ir := RubyBridge irMethod_defineMethStarArgs: aSymbol block: aBlock inClass: self  
            env: envId .
  ir addMethodProtection: (protInt := self rubyMethodProtection: envId  ).
  comp := RubyCompiler new .
  blockArg _fileAndLine ifNotNil:[ :fl | ir fileName: (fl at: 1) ; lineNumber: (fl at: 2) ].
  cm := comp compiledMethodForIR: ir .
  1 to: newBlkMeths size do:[:j | 
     (newBlkMeths at: j) _rubyInClass: cm ; immediateInvariant "Trac748"
  ].
  RubyBridge installBridgesForPrefix: aSymbol suffix: #'#0*&' selector: newSym
     in: self argsDescr: nil 
     optArgs: (optBits := cm rubyOptArgsBits)  protection: protInt primKind: 0 env: envId .
  (self allModuleMethodsEnabled: envId) ifTrue:[ | mmMod |
     (mmMod := self moduleMethodsModule)  addRubySelector: newSym method: cm env: envId .
     RubyBridge installBridgesForPrefix: aSymbol suffix: #'#0*&' in: mmMod  argsDescr: nil 
         optArgs: optBits  protection: protInt primKind: 0 env: envId .
  ].
  self addRubySelector: newSym  method: cm env: envId  .
  (res := RubyUnboundMeth _basicNew) method: cm env: envId selPrefix: aSymbol .
  ^ res

%


set class Behavior
category: '*maglev-runtime'
method:
defineMethod: aSymbol method: aMethod
    "a ruby primitive"
  ^ self defineMethod: aSymbol method: aMethod env: 1"__callerEnvId"

%


set class Behavior
category: '*maglev-runtime'
method:
defineMethod: aSymbol method: aMethod env: envId
  | mCls newCm protInt mSel mPrefix  newSym cm mSelSiz prefixSiz res |
    "Return a RubyUnboundMeth encapsulating a copy of aMethod,
     with selector of the copy changed to match aSymbol. " 
      "ruby_selector_suffix dependent"
  (aMethod isKindOf: RubyUnboundMeth) ifFalse:[
    ArgumentTypeError signal:'method argument is not a Method'
  ].
  cm := aMethod nonBridgeGsMethod: envId .
  mCls := cm inClass .
  self allInstVarNames =  mCls allInstVarNames ifFalse:[
     ArgumentError signal: 'define_method , arg''s class has different fixed instvars than self'
  ].
  newCm := cm _copyForClass: self aliasFrom: nil to: nil comment:'
METHOD COPIED by define_method' .  
  mPrefix := (mSel := newCm selector ) rubySelectorPrefix .
  mSelSiz := mSel size .
  prefixSiz := mPrefix size .
  newSym := aSymbol .  "Fix Trac 913"
  prefixSiz < mSelSiz  ifTrue:[ 
    newSym := newSym , (mSel copyFrom: prefixSiz + 1  to: mSelSiz) .
    newSym := newSym asSymbol 
  ].
  newCm environmentId ~~ 0 ifTrue:[ 
    newCm selector: newSym ;
       setRubyProtection: ( protInt := self rubyMethodProtection: envId  ) . 
  ] ifFalse:[ 
    "cm was a smalltalk method from RHS of a ruby primitive, can't change selector" 
    protInt := 0 .
  ]. 
  newCm immediateInvariant .
  RubyBridge installBridgesFor: newSym in: self argsDescr: nil 
     optArgs: newCm rubyOptArgsBits  protection: protInt primKind: 0 env: envId . 
  self addRubySelector: newSym method: newCm env: envId  .
  (res := RubyUnboundMeth _basicNew) method: newCm env: envId selPrefix: mPrefix .
  ^ res

%


set class Behavior
category: '*maglev-runtime'
method:
installClassPrimitive: rubySymbol
  "a ruby primitive"
  ^ self class installPrimitive: rubySymbol selector: rubySymbol withBridges: true 
                env: 1"__callerEnvId"

%


set class Behavior
category: '*maglev-runtime'
method:
installClassPrimitive: rubySymbol selector: selString
  "a ruby primitive"
  ^ self class installPrimitive: rubySymbol selector: selString withBridges: true 
                env: 1"__callerEnvId"

%


set class Behavior
category: '*maglev-runtime'
method:
installClassPrimitiveNobridge: rubySymbol
  "a ruby primitive"
  ^ self class installPrimitive: rubySymbol selector: rubySymbol withBridges: false 
         env: 1"__callerEnvId" 

%


set class Behavior
category: '*maglev-runtime'
method:
installClassPrimitiveNobridge: rubySymbol selector: selString
  "a ruby primitive"
  ^ self class installPrimitive: rubySymbol selector: selString withBridges: false  
                env: 1"__callerEnvId"

%


set class Behavior
category: '*maglev-runtime'
method:
installClassPrimitiveNobridgeEnv: rubySymbol sel: prefix suffix: suffix 
  | envId sel |
  envId := 1"__callerEnvId" .
  sel := prefix , envId asString , suffix .
  ^ self class installPrimitive: rubySymbol selector: sel withBridges: false
                env: envId stEnv: envId

%


set class Behavior
category: '*maglev-runtime'
method:
installPrimitive: rubySymbol
  "a ruby primitive"
    ^ self installPrimitive: rubySymbol selector: rubySymbol withBridges: true 
                env: 1"__callerEnvId"

%


set class Behavior
category: '*maglev-runtime'
method:
installPrimitive: rubySymbol selector: selString
  "a ruby primitive"
  ^ self installPrimitive: rubySymbol selector: selString withBridges: true 
                env: 1"__callerEnvId"

%


set class Behavior
category: '*maglev-runtime'
method:
installPrimitive: rubySymbol selector: selString withBridges: bridgeBool env: envId
  ^ self installPrimitive: rubySymbol selector: selString withBridges: bridgeBool 
	 env: envId stEnv: 0

%


set class Behavior
category: '*maglev-runtime'
method:
installPrimitive: rubyName selector: selString withBridges: bridgeBool env: envId
  stEnv: stEnvId
  "For a primitive with last arg a block, the rubyName should be of
   the form   'select&' as in Array.rb .   "
      "ruby_selector_suffix dependent"

  | selector rNameSiz rubySel  stMeth lastCh firstCh stNargs desc |
  rubyName containsSeparator ifTrue:[ self error:'illegal whitespace in rubySelector for primitive'].
  selector := selString asSymbol.
  stMeth := self lookupSelector: selector .  "env 0 lookup"
  stMeth ifNil:[
    (stEnvId > 0 or:[ self == UndefinedObject or:[ self == Boolean]]) ifTrue:[ | arr |
       "look in envId also, or handle  rubymethods   in NilTF.gs "
       arr := self lookupSelector: selector rubyEnv: envId .
       arr ifNotNil:[ stMeth := arr at: 1 ].
    ].
    stMeth ifNil:[
      self error:'smalltalk method  ' , selString , ' not found in ' , self name ,
        '  for ruby primitive ' , rubyName  .
      ^ self
    ].
  ].
  rNameSiz := rubyName size .
  lastCh := rubyName at: rNameSiz .
  firstCh := rubyName at: 1 .     "need to exclude binary selectors like **  "
  stNargs := stMeth numArgs .
  (rNameSiz > 1 and:[ (lastCh == $& or:[ lastCh == $* ])
               and:[ firstCh isLetter or:[ firstCh == $_ ] ] ] ) ifTrue:[
    | nameSuffixSize |
    lastCh == $& ifTrue:[  
      (rubyName at: rNameSiz - 1 ) == $* 
             ifTrue:[ desc := 3 . nameSuffixSize := 2 ]   " *& "
	     ifFalse:[ desc := 1 . nameSuffixSize := 1 ] " _& "
    ] ifFalse:[ 
      lastCh == $* ifTrue:[
        desc := 2 . nameSuffixSize := 1  "*_"
      ] ifFalse:[
        desc := 0 . nameSuffixSize := 0  " __ " 
      ]
    ].
    rubySel := rubyName copyFrom: 1 to:  rNameSiz - nameSuffixSize .
    desc := ((stNargs - nameSuffixSize) bitShift: 2 ) + desc .
  ] ifFalse:[
    rubySel := rubyName .       "compute desc for fixed args "
    desc :=  stNargs bitShift: 2 .
  ].
  rubySel := rubySel _asSymbolWithRubySuffix: desc .
  stMeth rubyOptArgsBits ~~ 0 ifTrue:[ self error:'unexpected arg initializers' ].
  RubyCompiler new installBridgeMethodsFor: rubySel in: self
                  argsDescr: nil optArgs: 0 protection: 0
                  primKind: ( bridgeBool ifTrue:[ 1] ifFalse:[ 2 ]  ) env: envId .
  self addRubySelector: rubySel env: envId smalltalkMethod: stMeth  .
  RubyContext default trackRubyPrimitive: stMeth inClass: self rubySel: rubySel env: envId .

%


set class Behavior
category: '*maglev-runtime'
method:
installPrimitiveEnv: rubySymbol sel: prefix suffix: suffix
  "a ruby primitive"
  | envId sel |
  envId := 1"__callerEnvId" .
  sel := prefix , envId asString , suffix . 
  ^ self installPrimitive: rubySymbol selector: sel withBridges: true env: envId stEnv: envId

%


set class Behavior
category: '*maglev-runtime'
method:
installPrimitiveNobridge: rubySymbol
  "a ruby primitive"
    ^ self installPrimitive: rubySymbol selector: rubySymbol withBridges: false 
                env: 1"__callerEnvId"

%


set class Behavior
category: '*maglev-runtime'
method:
installPrimitiveNobridge: rubySymbol selector: selString
  "a ruby primitive"
  ^ self installPrimitive: rubySymbol selector: selString withBridges: false  
                env: 1"__callerEnvId"

%


set class Behavior
category: '*maglev-runtime'
method:
installPrimitiveNobridgeEnv: rubySymbol sel: prefix suffix: suffix
  | envId sel |
  envId := 1"__callerEnvId" .
  sel := prefix , envId asString , suffix .
  ^ self installPrimitive: rubySymbol selector: sel withBridges: false env: envId stEnv: envId

%


set class Behavior
category: '*maglev-runtime'
method:
isModifiable_oops_notModule
  | ivn |
  self isInvariant ifTrue:[ ^ false ].

  ((ivn := instVarNames) ~~ #() and:[ ivn isInvariant]) ifTrue:[ ^ false ].

  ^ (format bitAnd: 1) == 0 "neither bytes nor special"     

%


set class Behavior
category: '*maglev-runtime'
method:
lookupRubyCallStarB: aSymbol env: envId
  | sym arr |
  sym := aSymbol prefixIfRubySelector _asSymbolWithRubySuffix: 16r3 "(s, '#0*&') asSymbol" .
  arr := self lookupSelector: sym rubyEnv: envId .
  ^ arr ifNotNil:[ arr at: 1 "ignore method protection details" ]

%


set class Behavior
category: '*maglev-runtime'
method:
lookupSelector: aSymbol rubyEnv: envId 
  "does not consider the package policy
   Returns nil or an Array { cm . protectionOverride }"
  | cls cm override |
  cls := self . 
  [ 
    (cm := cls compiledMethodAt: aSymbol rubyEnv: envId) ifNotNil:[
      cm _isSmallInteger ifTrue:[
         override ifNil:[ override := cm ].
      ] ifFalse:[
        ^ { cm . override ifNil:[ 0 ] }
      ].
    ].
    "include virtual classes for ruby modules. "
    cls := cls rubySuperclass: envId .
    cls ~~ nil 
  ] whileTrue .
  ^ nil

%


set class Behavior
category: '*maglev-runtime'
method:
methodDefined: aSymbol rubyEnv: envId
  "does not consider the package policy.
   Returns nil or an Array { cm . protectionOverride }"
  ^ self lookupSelector: aSymbol rubyEnv: envId

%


set class Behavior
category: '*maglev-runtime'
method:
nonBridgeRubySelectorsInto: aSet hiddenInto: hiddenSet protection: protInt  env: envId
  "adds non-bridge ruby selectors of receiver to the set aSet .
   protInt specifies filtering, per GsNMethod>>rubyMethodProtection
     or -1 to return both public and protected. "
      "ruby_selector_suffix dependent"
  | sels |
  sels := self selectorsForEnvironment: envId .
  1 to: sels size do:[:idx | | mth aSel prefix |
    aSel := sels at: idx .
    mth := self compiledMethodAt: aSel rubyEnv: envId .
    mth ifNotNil:[
     "by using prefix, we trim the annotation characters.  Need to be careful of names like
     '&' and '&&', which are proper ruby method names.  This should be
      safe, since we don't strip if the method name has only one character
      (protects '&') and if the name is something like '&&', then it is a
      binary operator, and will be '&&:', and we trim $& before $:, which
      protects the '&&'.  Use rubySelectorPrefix from the .mcz .
     "
       prefix := aSel prefixIfRubySelector .
       (hiddenSet includes: prefix ) ifFalse:[
         mth _isSmallInteger ifTrue:[  
           mth <= METH_lu_skipPersistentMd ifTrue:[ 
             hiddenSet add: prefix .
           ] ifFalse:[ | override |
             override := METH_prot_override_public - mth.
             (override == protInt or:[ protInt == -1 and:[ override <= 1]]) ifTrue:[
               aSet add: prefix
             ] ifFalse:[
               hiddenSet add: prefix .
             ]
           ]
         ] ifFalse:[
            mth isRubyBridgeMethod ifFalse:[ | mp |
             (mp := mth rubyMethodProtection) == protInt ifTrue:[
               aSet add: prefix .
             ] ifFalse:[ 
               (protInt == -1 and:[ mp <= 1]) ifTrue:[
                  aSet add: prefix "returning both protected and public methods"
                ] ifFalse:[
                  hiddenSet add: prefix .
                ].
             ].
           ].
         ].
       ].
    ].
  ].

%


set class Behavior
category: '*maglev-runtime'
method:
persistentMethodDictForEnv: envId
"result will be nil if no methods exist for specified environmentId."
| mds md |
(mds := methDicts) _isArray ifTrue:[ | idx |
  md := mds atOrNil: (envId*4 + 1)
] ifFalse:[
  envId == 0 ifTrue:[ md := mds ].   
].
md ifNil:[
 (format bitAnd: 16r14000) ~~ 0 ifTrue:[  
     "GC_MODULE_inclSelf_CLASS | GC_RUBY_VIRTUAL_CLASS"
    md := primaryCopy persistentMethodDictForEnv: envId
 ]
].
^ md

%


set class Behavior
category: '*maglev-runtime'
method:
persistentMethodDictForStoreEnv: envId
  | mds dict ofs |
  ofs := envId*4 + 1 .
  (mds := methDicts) _isArray ifTrue:[
    dict := mds atOrNil: ofs .
  ] ifFalse:[
    envId == 0 ifTrue:[ dict := mds ]
  ].
  dict ifNotNil:[ ^ dict ].
  dict := GsMethodDictionary new . 
  self persistentMethodDictForEnv: envId put: dict .
  (methDicts atOrNil: 1) ifNotNil:[ "a class with existing smalltalk behavior"
    self isRubySingletonClass ifFalse:[
      RubyContext default trackRubyClass: self thisClass env: envId  . "for ruby context reset"
    ].
  ].
  ^ dict

%


set class Behavior
category: '*maglev-runtime'
method:
removeRubySelector: selectorSymbol env: envId

" returns self "
 
 | md pmd tmeth pmeth  |
 md := self transientMethodDictForEnv: envId .
 md ifNotNil:[
   tmeth := md removeKey: selectorSymbol otherwise: nil .
 ].
 pmd := self persistentMethodDictForEnv: envId.
 pmd ifNotNil:[
   RubyCompilerState current persistenceMode ifTrue:[ 
     pmeth := pmd removeKey: selectorSymbol otherwise: nil .
     pmeth ifNotNil:[  
       self _codeChangedForEnv: envId.
     ].
     pmeth == tmeth ifTrue:[ pmeth := nil ].
   ] ifFalse:[
     (pmd includesKey: selectorSymbol) ifTrue:[
       md ifNil:[ md := self transientMethodDictForStoreEnv: envId ].
       md at: selectorSymbol put: 4 "method-removed transiently" .
       pmeth := 4 .
     ].
   ]
 ].
 tmeth ifNotNil:[
  self _refreshLookupCache: selectorSymbol oldMethod: tmeth  env: envId
 ].
 pmeth ifNotNil:[
   self _refreshLookupCache: selectorSymbol oldMethod: pmeth  env: envId
 ].
"TraceRubyMethodDicts ifTrue:[
     GsFile gciLogServer: self name, '  removeRubySelector: ' , 
    selectorSymbol printString ].
"

%


set class Behavior
category: '*maglev-runtime'
method:
rubyAlias: newName from: oldName
  "a ruby primitive.
   target of alias may be private, the NoMethodError is thrown
     on attempt to execute, not at the point of aliasing "
      "ruby_selector_suffix dependent"
   | cls oldSymbol newSymbol oldSel envId cst selectors |
   envId := 1"__callerEnvId" .
   oldSymbol := oldName asSymbol .
   newSymbol := newName asSymbol .
   cst := RubyCompilerState current .
   (RubyCompiler reimplementationAllowed: newSymbol for: self cst: cst ) ifFalse:[
      NameError signal:'aliasing of ' ,  newSymbol , ' not allowed '
   ].
   oldSel := oldSymbol _asSymbolWithRubySuffix: 16r3 " #0*& " .
   cls := self whichClassIncludesSelector: oldSel rubyEnv: envId .
   cls ifNil:[ NameError signal: 'alias_method:  no method found for ', oldSymbol ].
   
   selectors := cls rubySelectorsWithPrefix: oldSymbol env: envId .
   selectors do: [:oldSelector | | cm |
    
     cm := cls compiledMethodAt: oldSelector rubyEnv: envId .
     cm ifNotNil:[
        | mCopy newSelector suffix commStr |
        suffix := oldSelector rubySelectorSuffix .
        newSelector := (newSymbol, suffix) asSymbol .
        "copy even if not a bridge, so protection can be changed on the copy"
        commStr := '
METHOD COPIED by alias' .
        cm isRubyBridgeMethod ifTrue:[
          mCopy := cm _copyForClass: self aliasFrom: oldSymbol to: newSymbol comment: commStr.
        ] ifFalse:[
          "no bytecode level changes, just copy so we can change protection. Trac 919"
          mCopy := cm _copyForClass: self aliasFrom: nil to: nil comment: commStr.
        ].
        mCopy environmentId ~~ 0 ifTrue:[
          mCopy selector: newSelector . "for 1.8.7"
          mCopy _rubyInClass: cls ."preserve original for use by SEND_RUBY_SUPER, Trac676"
        ].
        mCopy immediateInvariant .
        self addRubySelector: newSelector method: mCopy env: envId .
    ] .
  ] .

%


set class Behavior
category: '*maglev-runtime'
method:
rubyAncestorModulesNames
  | arr res sz  |
  arr := self theNonMetaClass rubyIncludedModules .
  res := Array new: (sz := arr size).
  1 to: sz do:[:n | res at: n put: (arr at: n) name ].
  ^ res

%


set class Behavior
category: '*maglev-runtime'
method:
rubyClassInstVarDefined: aSymbol env: envId 
	| ns assoc   |
   ns := self nameSpace: envId .
   ns ifNotNil:[ assoc := ns resolveConstant: aSymbol ] .
   assoc ifNil:[ ^ nil ].
   assoc _valueNoAction  ifNotNil:[ ^ #'instance-variable' ] .
   ^ nil 
  

%


set class Behavior
category: '*maglev-runtime'
method:
rubyClassInstVarNamed: aSymbol env: envId 
  | ns assoc   |
   ns := self nameSpace: envId .
   ns ifNotNil:[ assoc := ns resolveConstant: aSymbol ] .
   assoc ifNil:[ ^ nil ].
   ^ assoc _value

%


set class Behavior
category: '*maglev-runtime'
method:
rubyClassInstVarNamed: aSymbol put: anObject env: envId
  | ns |
  ns := self transientNameSpaceForStore: envId .
  ^ ns at: aSymbol runtimePut: anObject

%


set class Behavior
category: '*maglev-runtime'
method:
rubyClassVarDefined: aString
  "a ruby primitive "
  ^ self theNonMetaClass _rubyClassVarDefined: aString asSymbol env: 1"__callerEnvId"

%


set class Behavior
category: '*maglev-runtime'
method:
rubyClassVarGet: aString
  "a ruby primitive "
  ^ self theNonMetaClass _rubyClassVarGet: aString asSymbol  env: 1"__callerEnvId"

%


set class Behavior
category: '*maglev-runtime'
method:
rubyClassVarNames
  "a ruby primitive.
   Return an Array of Strings, the elements are names of
   ruby class variables"

^ self theNonMetaClass _rubyClassVarNames: 1"__callerEnvId" 

%


set class Behavior
category: '*maglev-runtime'
method:
rubyInstvarAt: aString
  "a ruby primitive."
  | envId |
  envId := 1"__callerEnvId" .
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString at: 1) == $@ ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name'
  ]. 
  " @_st_ prefix not supported yet for class inst vars "
  ^ self rubyClassInstVarNamed: aString asSymbol  env: envId

%


set class Behavior
category: '*maglev-runtime'
method:
rubyInstvarAt: aString env: envId
  "used in implementation of inspect. "
  |  sym |
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString at: 1) == $@ ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name'
  ].
  sym := aString asSymbol .
  ^ self rubyClassInstVarNamed: sym env: envId

%


set class Behavior
category: '*maglev-runtime'
method:
rubyInstvarAt: aString put: aValue env: envId
  |  sym |
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString at: 1) == $@ ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name for ', self rubyName
  ].
  sym := aString asSymbol .
  ^ self rubyClassInstVarNamed: sym put: aValue env: envId

%


set class Behavior
category: '*maglev-runtime'
method:
rubyInstVarDefined: aSymbol
  "called from generated code"
  ^ self rubyClassInstVarDefined: aSymbol env: 1"__callerEnvId" 
    

%


set class Behavior
category: '*maglev-runtime'
method:
rubyIvDefined: aString
  "a ruby primitive"
  | rubyName |
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString at: 1) == $@ ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name'
  ].
  rubyName := aString asSymbol .
  ^ (self rubyClassInstVarDefined: rubyName env: 1"__callerEnvId" ) ~~ nil
  

%


set class Behavior
category: '*maglev-runtime'
method:
rubyMethodDefined: aSymbol protection: protInt
  "a ruby primitive"
      "ruby_selector_suffix dependent"
  | arr envId |
  envId := 1"__callerEnvId" .
  arr := self methodDefined: (aSymbol prefixIfRubySelector _asSymbolWithRubySuffix: 16r3 " #0*&")
            rubyEnv: envId .
  arr ifNil:[ ^ false ].
  protInt ~~ -1 ifTrue:[ | override cm |
    override := arr at: 2 .
    override == 0 ifFalse:[
      override := METH_prot_override_public - override . "convert 7..5 to 0..2"
      ^ override == protInt 
    ].
    cm := arr at: 1 .
    ^ cm rubyMethodProtection == protInt 
  ].
  ^ true

%


set class Behavior
category: '*maglev-runtime'
method:
rubyMethodFor: aSymbol
  "a ruby primitive.
  Returns a non-bridge instance of GsNMethod, or signals a NameError"
      "ruby_selector_suffix dependent"
  | cm envId arr sym selPrefix |
  envId := 1"__callerEnvId" .
  "inline  lookupRubyCallStarB:env: "
  selPrefix := aSymbol prefixIfRubySelector .
  sym := selPrefix _asSymbolWithRubySuffix: 16r3 " #0*& "  .
  arr := self lookupSelector: sym rubyEnv: envId .
  arr ifNotNil:[
    (arr at: 2) == METH_lu_undef_ed ifTrue:[ 
      NameError signal:'method ', aSymbol , ' has been undef-ed'
    ].
    cm := arr at: 1 .
    cm isRubyBridgeMethod ifTrue:[ | nonBrMeth |
      nonBrMeth := cm _nonBridgeMethod .
      nonBrMeth ifNil:[ NameError signal:'cannot find non-bridge method for ' , aSymbol ].
      cm := nonBrMeth.
    ].
    ^ cm 
  ] ifNil:[
    NameError signal:'no method ''' , aSymbol prefixIfRubySelector , 
            ''' for class ''' , (self @ruby1:name ) , '''' 
  ].

%


set class Behavior
category: '*maglev-runtime'
method:
rubyMethodFor: aSymbol env: envId
  "Returns an instance of RubyMeth, or signals a NameError.  Caller should
     send object: to the RubyMeth"
      "ruby_selector_suffix dependent"
  | cm m |
  cm := self lookupRubyCallStarB: aSymbol env: envId .
  cm ifNotNil:[ 
    cm isRubyBridgeMethod ifTrue:[ | nonBrMeth |
      nonBrMeth := cm _nonBridgeMethod .
      nonBrMeth ifNil:[ NameError signal:'cannot find non-bridge method for ' , aSymbol ].
      cm := nonBrMeth.
    ].
  ] ifNil:[ NameError signal:'no method ''' , aSymbol prefixIfRubySelector ,
             ''' for class ''' , (self @ruby1:name ) , '''' 
  ].
  (m := RubyMeth _basicNew) method: cm env: envId selPrefix: aSymbol prefixIfRubySelector;
      bridge: (RubyBridge execMethBridgeTo: cm) .
  ^ m 

%


set class Behavior
category: '*maglev-runtime'
method:
rubyMethodFor: aSymbol instanceMethod: aBool
  "a ruby primitive.
  Returns a non-bridge instance of GsNMethod, or signals a NameError"
      "ruby_selector_suffix dependent"
  | cm envId arr sym selPrefix target |
  envId := 1"__callerEnvId" .
  "inline  lookupRubyCallStarB:env: "
  selPrefix := aSymbol prefixIfRubySelector .
  sym := selPrefix _asSymbolWithRubySuffix: 16r3 " #0*& " .
  aBool ifTrue:[ target := self ] ifFalse: [ target := (self virtualClass) ] .
  arr := target lookupSelector: sym rubyEnv: envId .
  arr ifNotNil:[
    (arr at: 2) == METH_lu_undef_ed ifTrue:[ 
      NameError signal:'method ', aSymbol , ' has been undef-ed'
    ].
    cm := arr at: 1 .
    cm isRubyBridgeMethod ifTrue:[ | nonBrMeth |
      nonBrMeth := cm _nonBridgeMethod .
      nonBrMeth ifNil:[ NameError signal:'cannot find non-bridge method for ' , aSymbol ].
      cm := nonBrMeth.
    ].
    ^ cm 
  ] ifNil:[
    NameError signal:'no method ''' , aSymbol prefixIfRubySelector , 
            ''' for class ''' , (self @ruby1:name ) , '''' 
  ].

%


set class Behavior
category: '*maglev-runtime'
method:
rubyRemoveIv: aString
  "a ruby primitive"
  | rubyName envId ns |
  envId := 1"__callerEnvId" .
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString at: 1) == $@ ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name'
  ].
  rubyName := aString asSymbol .
  ns := self nameSpace: envId .
  ns ifNotNil:[ | assoc |
     assoc := ns resolveConstant: rubyName   .
    assoc ifNotNil:[ | val |
      (val := assoc _valueNoAction ) ifNotNil:[ 
          ns removeKey: rubyName .  
         ^ val
       ].
    ].
  ].
  NameError signal:'instance variable ' , rubyName , ' not defined'

%


set class Behavior
category: '*maglev-runtime'
method:
rubyRemoveMethod: aSymbol
  "a ruby primitive"
  | cm envId sel | 
  envId := 1"__callerEnvId" .
  sel := aSymbol _asSymbolWithRubySuffix: 16r3 " #0*& " .
  cm := self compiledMethodAt: sel rubyEnv: envId .
  cm ifNotNil:[
    cm _isSmallInteger ifTrue:[
       cm <= METH_lu_skipPersistentMd ifTrue:[
         NameError signal:'remove_method , ', aSymbol , ' has already been undef-ed'
       ].
    ].
    (self rubySelectorsWithPrefix: aSymbol env: envId) do: [ :eaSelector |
        self removeRubySelector: eaSelector env: envId .
    ] .
    self isRubyModuleFunctions ifTrue:[
      self primaryCopy @ruby1:singleton_method_removed: aSymbol .
      ^ self
    ].
    self isRubySingletonClass ifTrue:[
      self @ruby1:singleton_method_removed: aSymbol  .
      ^ self
    ].
    self @ruby1:method_removed: aSymbol  .
  ] ifNil:[
    NameError signal:'remove_method , ' , aSymbol , ' not found'
  ]

%


set class Behavior
category: '*maglev-runtime'
method:
rubySelectorsWithPrefix: prefix env: envId
    "Return set of all selectors matching the given prefix. Prefix may be a string or symbol"
      "ruby_selector_suffix dependent"
    |prefixSymbol sels|
    prefixSymbol := prefix asSymbol .
    sels := IdentitySet new .
    (self selectorsForEnvironment: envId) do:[ :ea |
        (ea prefixIfRubySelector)  == prefixSymbol ifTrue:[ 
            sels add: ea asSymbol .
        ]
    ] .
    ^ sels .

%


set class Behavior
category: '*maglev-runtime'
method:
rubyUnboundMethodFor: aSymbol
  "a ruby primitive.
  Returns an instance of RubyUnboundMeth, or signals a NameError"
      "ruby_selector_suffix dependent"
  | cm m envId arr sym bridge selPrefix |
  envId := 1"__callerEnvId" .
  "inline  lookupRubyCallStarB:env: "
  selPrefix := aSymbol prefixIfRubySelector .
  sym := selPrefix _asSymbolWithRubySuffix: 16r3 " #0*& " .
  arr := self lookupSelector: sym rubyEnv: envId .
  arr ifNotNil:[
    (arr at: 2) == METH_lu_undef_ed ifTrue:[ 
      NameError signal:'method ', aSymbol , ' has been undef-ed'
    ].
    cm := arr at: 1 .
    cm isRubyBridgeMethod ifTrue:[ | nonBrMeth |
      nonBrMeth := cm _nonBridgeMethod .
      nonBrMeth ifNil:[ NameError signal:'cannot find non-bridge method for ' , aSymbol ].
      cm := nonBrMeth.
      bridge := RubyBridge execMethBridgeTo: cm .
    ]
  ] ifNil:[
    NameError signal:'no method ''' , aSymbol prefixIfRubySelector , 
            ''' for class ''' , (self @ruby1:name ) , '''' 
  ].
  (m := RubyUnboundMeth _basicNew) method: cm env: envId selPrefix: selPrefix ; 
    bridge: bridge .
  ^ m 

%


set class Behavior
category: '*maglev-runtime'
method:
rubyUndefMethod: aName
  "a ruby primitive"
  | sel cm envId baseSel |
  envId := 1"__callerEnvId" .
  baseSel := aName asSymbol .
  sel := baseSel _asSymbolWithRubySuffix: 16r3 " #0*& " .
  cm := self compiledMethodAt: sel rubyEnv: envId .
  cm ifNil:[
     (self whichClassIncludesSelector: sel rubyEnv: envId) ifNil:[
        NameError signal:'undef_method, ', aName, ' not found'
     ]
  ] ifNotNil:[
     cm == METH_lu_undef_ed ifTrue:[ 
       NameError signal:'undef_method, ', aName , ' has already been undef-ed'
     ].
    (self rubySelectorsWithPrefix: baseSel env: envId) do: [ :eaSelector |
        self removeRubySelector: eaSelector env: envId .
    ] .
    self isRubyModuleFunctions ifTrue:[
      self primaryCopy @ruby1:singleton_method_undefined: baseSel .
      ^ self
    ].
    self isRubySingletonClass ifTrue:[
      self @ruby1:singleton_method_undefined: baseSel .
      ^ self
    ].
    self @ruby1:method_undefined: baseSel .
  ].
  RubyBridge suffixOptions do:[:sufix | | sel |   "fix Trac422"
     sel :=  (baseSel , sufix) asSymbol .
     self addRubySelector: sel method: 3"method-hidden" env: envId 
  ].

%


set class Behavior
category: '*maglev-runtime'
method:
whichClassIncludesSelector: aString rubyEnv: envId

  "does not consider the package policy, 
   ignores protection and protection overrides "
  | cls aSymbol |
  aSymbol := Symbol _existingWithAll: aString .
  aSymbol ifNil:[ ^ nil ].
  cls := self .
  [ | cm |
    (cm := cls compiledMethodAt: aSymbol rubyEnv: envId) ifNotNil:[
       cm _isSmallInteger ifFalse:[ ^ cls ].
       cm == METH_lu_undef_ed ifTrue:[ ^ nil "stop lookup here"].
    ].
    "include virtual classes for ruby modules. "
    cls := cls rubySuperclass: envId .
    cls ~~ nil
  ] whileTrue .
  ^ nil

%


set class Behavior
category: '*maglev-runtime'
method:
_moduleEval1: lexPathNotUsed block: aBlock
    "A ruby primitive.  lexPath not used .
    Evaluate aBlock with the block's self and arg set to this module"
  | defStk envId cld |
  envId := 1"__callerEnvId" .
  cld := GsProcess _current _clientData .
  (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: self .
  cld at: 7 put: self " _rubyThreadDataAt: 7 put: " .
  ^ [ | val |
     aBlock ifNotNil:[ | blk |
        blk := aBlock setSelf: self .
        val := blk @ruby1:value: self  
     ] .
     val
   ] ensure:[
     defStk pop: self
   ]

%


set class Behavior
category: '*maglev-runtime'
method:
_moduleEvalString: aString with: vcGlobalsArr args: argsArr
    "A ruby primitive."
   | defStk envId  lexSelfStk aBinding cst rtModuStk cld |
  envId := 1"__callerEnvId" .
  cld := GsProcess _current _clientData .
  (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: self .
  cld at: 7 put: self " _rubyThreadDataAt: 7 put: " .

  aBinding := argsArr at: 1 .
  cst := RubyCompilerState current .
  lexSelfStk := cst lexicalSelfStack .
  lexSelfStk push: nil . " fix Trac908a" 
  rtModuStk := cst rtModuleStack .
  rtModuStk push: self .
  ^ [ | file line | 
       aBinding setModuleEval .
       file := argsArr atOrNil: 2 .
       file _stringCharSize == 0 ifTrue:[ file := nil ].
       line := argsArr atOrNil: 3 .
       line _isSmallInteger ifFalse:[ line := 1 ].
       RubyCompiler new evaluateString: aString binding: aBinding with: vcGlobalsArr 
       fileName: file lineNumber: line  env: envId 
    ] ensure: [
      defStk pop: self .
      lexSelfStk pop: nil .
      rtModuStk pop: self .
    ]

%


set class Behavior
category: '*maglev-runtime'
method:
_rubyInstvarAt: descrArray
 "A ruby primitive, and called from generated code"
  "descrArray is { stSymbol . rubySymbol . cachedClass . cachedOffset } "
  ^ self rubyClassInstVarNamed: (descrArray at: 2  "rubyName" ) env: 1"__callerEnvId"

%


set class Behavior
category: '*maglev-runtime'
method:
_rubyInstvarAt: descrArray put: aValue privateSize: privateSize  
  "called from generated code.
   descrArray is { stSymbol . rubySymbol . cachedClass . cachedOffset }
   Returns aValue "

  ^ self rubyClassInstVarNamed: (descrArray at:2)  put: aValue env: 1"__callerEnvId"

%


set class Behavior
category: '*maglev-runtime'
method:
_rubySubclassOf: aClass env: envId
  "return true if self is a subclass of aClass in specified environment"
| cls |
cls := self .
[ true ] whileTrue:[
  cls == aClass ifTrue:[ ^ true ].
  cls := cls rubySuperclass: envId .
  cls ifNil:[ ^ false ].
].

%

