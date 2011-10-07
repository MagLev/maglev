
set class RubyGlobalVarNode
category: 'as yet unclassified'
classmethod:
initialize: envId
  "executed during bootstrap"
  envId == 1 ifTrue:[ self _initialize ].
  self sessionInitialize

%


set class RubyGlobalVarNode
category: 'as yet unclassified'
classmethod:
newForRp
  ^ self _basicNew

%


set class RubyGlobalVarNode
category: '*maglev-runtime'
classmethod:
rubyAlias: newKey from: oldKey
  | dict arr |
  dict := SessionTemps current at:#RubyGlobalNodesDict .
  (arr := dict at: oldKey otherwise: nil ) ifNotNil:[
    "subsequent refs to newKey will get same code generation as oldKey,
     and access the one association for variable named oldKey"
    dict at: newKey put: arr .
    ^ true
  ].
  ^ false

%


set class RubyGlobalVarNode
category: '*maglev-runtime'
classmethod:
sessionInitialize
  "executed at each initialization of transient state"
 
  SessionTemps current at:#RubyGlobalNodesDict put: (SpecialGlobalNodesDict copy).


%


set class RubyGlobalVarNode
category: '*maglev-ast'
classmethod:
s_a: aSymbol
  | arr cls sym node |
  arr := (SessionTemps current at:#RubyGlobalNodesDict) at: aSymbol otherwise: nil .
  arr ifNotNil:[ cls := arr at: 2 . sym := arr at: 1] 
         ifNil:[ cls := RubyGlobalVarNode . sym := aSymbol ].
  (node := cls newForRp )
     name: sym .
  ^ node

%


set class RubyGlobalVarNode
category: '*maglev-runtime'
classmethod:
_initialize
  "executed during env 1 bootstrap"
  | dict | 
  dict := SymbolKeyValueDictionary new .
 { { #'$!' . RubyGlobalLastException . RubyGlobalLastExceptionAsgn } . 
    { #'$@' . RubyGlobalLastExcBackTrace . RubyGlobalNotAssignable } . 
    { #'$~' . RubyVcGlobalNode . RubyVcGlobalLastMatchAsgn } . 
    { #'$_' . RubyVcGlobalNode . RubyVcGlobalAsgNode } . 
    { #'$?' . RubyGlobalProcessState . RubyGlobalNotAssignable } . 
    { #'$stdout' . RubyGlobalVarNode . RubyGlobalAsgnStdoutNode } . 
    { #'$stderr' . RubyGlobalVarNode . RubyGlobalAsgnStdoutNode } . 
    { #'$stdin' . RubyGlobalVarNode . RubyGlobalAsgnStdinNode   } }
  do:[:arr|
    dict at:(arr at:1) put: arr
  ].
  SpecialGlobalNodesDict := dict .

%


set class RubyGlobalVarNode
category: '(as yet unclassified)'
method:
definedQkind
  ^  #'global-variable'

%


set class RubyGlobalVarNode
category: 'as yet unclassified'
method:
determineDynamic
   ^ 2

%


set class RubyGlobalVarNode
category: '*maglev-runtime'
method:
irDefinedQNode
  " generated code does not use a RubyConstantRef to cache the ref"
  | snd|
  (snd := GsComSendNode new)
     rcvr: (GsComLiteralNode newObject: Object) ;
     stSelector: #rubyGlobalVarDefinedQ: ;
      appendArgument: (GsComLiteralNode newObject: name asSymbol) .
  ^ self ir: snd

%


set class RubyGlobalVarNode
category: '*maglev-runtime'
method:
irNode
  | snd cref |
  (cref := RubyConstantRef new) globalVarName: name asSymbol .
  (snd := GsComSendNode new)
     rcvr: (GsComLiteralNode newObject: cref) ;
     stSelector: #resolveGlobalVarValue .
  ^ self ir: snd

%


set class RubyGlobalVarNode
category: 'accessing'
method:
name

	 ^ name

%


set class RubyGlobalVarNode
category: 'accessing'
method:
name: aString
	name := aString

%


set class RubyGlobalVarNode
category: 'printing'
method:
printSourceOn: aStream
	aStream nextPutAll: name

%


set class RubyGlobalVarNode
category: '*maglev-runtime'
method:
_inspect
  ^ '[:gvar, :', name , $]

%

