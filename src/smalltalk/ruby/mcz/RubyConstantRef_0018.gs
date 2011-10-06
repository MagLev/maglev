
set class RubyConstantRef
category: '*maglev-runtime'
classmethod:
_abstractCall: rcvrBlock definedQ: selectorSym
    "returns  'method'  if receiver responds to aSymbol, nil otherwise
     flags say don't cache result in code_gen, ruby lookup, env 1"

  | rcvr |
  rcvr := [ rcvrBlock value ] onException: Exception do:[ :ex | ^ nil  ].
  ^ (rcvr _respondsTo: selectorSym private: true flags: 16r00101) 
       ifTrue:[ 'method' copy ] 
  

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
c3name: aSymbol
  "used by RubyColon3Node "
  lexPathSize := 1 .
  self size: 3 ;
       at: 1 + 2 put: aSymbol ;
       at: 1 + 1 put: nil "evalSelfCls" ;
       at: 1 put: Object .
  "globalAssoc left as nil" 

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
dyn_definedQinContext: anObject
 "called from generated code"
 | idx mySize assoc envId |
    "evaluate  :: terms"   
envId := 1"__callerEnvId" .
anObject ifNil:[ ^ nil ] .
assoc := anObject  rubyConstAssociationAtOrNil: (self at: 1) env: envId .
assoc ifNil:[ ^ nil ].
mySize := self size .   
idx := 2 .
[ idx <= mySize ] whileTrue:[ | val |
  assoc isDefined ifNil:[ ^ nil ]. 
  val := assoc _valueNoAction .
  assoc := val rubyConstAssociationAtOrNil: (self at: idx) env: envId .
  assoc ifNil:[ ^ nil ].
  idx := idx + 1
].
^ assoc definedQ

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
dyn_resolveInContext: anObject
  "called from generated code"
  | idx mySize assoc val envId aModule sym |
"evaluate  :: terms"  
aModule := anObject .
assoc := anObject  rubyConstAssociationAt: (sym := self at: 1)  env: (envId := 1"__callerEnvId") . 
mySize := self size .   
idx := 2 .
[ idx <= mySize ] whileTrue:[ | prev |
	aModule := assoc _valueFor: sym inClass: aModule env: envId . "possible autoload"
  assoc := aModule rubyConstAssociationAt: (sym := self at: idx) env: envId .
  idx := idx + 1
].
"no setGlobalAssoc...  , result is not cachable due to first term not a constant name"
val := assoc _valueFor: sym inClass: aModule env: envId . "possible trigger of autoload"

^ val

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
globalVarName: aSymbol
  self at: 1 put: aSymbol

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
initialize
  lexPathSize := 0 .

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
irDefinedQNode
  "maybe not used ?"
  | node |
  (node := GsComSendNode new)
       stSelector:  #definedQconst ;
       rcvr: (GsComLiteralNode newObject: self  ) .
  ^ node

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
lexPathSize
  ^ lexPathSize

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
name: aSymbol c2lexPath: lexPath evalSelfCls:  selfCls 
  "used by RubyColon2Node   "
  | lpSize  cnt newSiz |
  cnt := 0 .
  lpSize := lexPath size . 
  lexPathSize := lpSize .  
  self size: (newSiz := lpSize + 2) ;
       at: newSiz     put: aSymbol ;
       at: lpSize + 1 put: selfCls .
  lpSize ~~ 0 ifTrue:[  
    self replaceFrom: 1 to: lpSize with: lexPath startingAt: 1 .
  ].
  "globalAssoc left as nil"

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
notFoundValue
  ^ NameError signal: 'unresolved constant ' , self pathToString

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
pathToString
  | idx last str |
  idx := lexPathSize + 1 .
  last := self size  .
  str := String withAll: (self at: idx) .
  idx := idx + 1 .
  [ idx <= last ] whileTrue:[
    str add:  '::' ; add: (self at: idx) .
    idx := idx + 1 .
  ].
  ^ str

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
resolveGlobalVarAsgn: aValue
  "called from generated code."
| assoc |
assoc :=  self resolveGlobalVarAssoc .
assoc ifNotNil:[
  assoc rubyGlobalVarValue: aValue .
] ifNil:[ | trc dict rns aSymbol envId |
  envId := 1 "__callerEnvId " .
  aSymbol := self at: 1 .
  dict := SessionTemps current at: #RUBY_traceGlobalVars otherwise: nil .
  dict ifNotNil:[
    (trc := dict at: aSymbol otherwise: nil ) ifNotNil:[
      1 to: trc size do:[ :n | | blk |
        blk := trc at: n .
        blk @ruby1:value: aValue  
      ]
    ].
  ].
  rns := Object transientNameSpaceForStore: envId .
  (assoc := rns associationAt: aSymbol otherwise: nil) ifNil:[
    assoc := RubyGlobalVarAssociation newWithKey: aSymbol .
    rns addTransientAssociation: assoc .
  ].
  assoc rubyGlobalVarValue: aValue .
  trc ifNotNil:[ self setGlobalAssoc_noMarkDirty: assoc ].
].
^ aValue

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
resolveGlobalVarValue
  "called from generated code.
   returns nil or the value of the global variable. "

| assoc |
assoc :=  self resolveGlobalVarAssoc .
assoc ifNil:[
  assoc := Object rubyGlobalVarAssoc: (self at:1) env: 1"__callerEnvId" .
  self setGlobalAssoc_noMarkDirty: assoc . "cache it"
].
^ assoc globalVarValue .

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
setDynamicTypeError
  lexPathSize := nil

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
_constantMissing: aSymbol in: aClass env: envId 
  | val assoc |
  val := aClass @ruby1:const_missing: aSymbol  .
  (assoc := RubySymbolAssociation newWithKey: aSymbol) _value: val .
  ^ assoc

%


set class RubyConstantRef
category: '*maglev-runtime'
method:
_valueNoAction
  ^ nil

%

