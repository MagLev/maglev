
set class RubyStaticScope class
category: '*maglev-runtime'
method:
initialize
  TraceLocals := 0  "0 is no trace , 1 is tracing"

%


doit
RubyStaticScope initialize.
%


set class RubyStaticScope class
category: '*maglev-runtime'
method:
traceLocals: anInt  
   "anInt   0 is no trace , 1 is tracing"

   TraceLocals := anInt 

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
addVariableNames: anArray
  1 to: anArray size do:[:n | 
	  self locationForName: (anArray at: n )
  ].

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
argLeafAtIndex: aNumber
  | nA aLeaf |
  aNumber <= (nA := self numArgs) ifTrue: [
    aLeaf := self _argLeafAtOffset: aNumber .
    aLeaf isArg ifFalse:[ self error:'not an arg leaf'].
  ] ifFalse: [
    self error:'invalid use of argLeafAtIndex:  '.
  ].
  ^ aLeaf

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
buildTempsOn: aNode
  | start vNames anAssoc aLeaf |
  (start := self numArgs + 1 ) to: (vNames := variableNames) size do:[:n |
	anAssoc := vNames at: n .
	aLeaf := self _leafForVarAssoc: anAssoc at: n .
	aLeaf isTemp ifTrue:[  
       aNode appendTemp: aLeaf . 
     ].
  ].

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
clearVariableNames
   variableNames := { } .
   

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
detectMismatchWith: other
	^ (self matches: other) ifFalse: [{self. other}]

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
enclosingScope

	 ^ enclosingScope

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
enclosingScope: aScope
    enclosingScope := aScope .
    inBootstrap := aScope inBootstrap  .
    variableNames := { } . "inline clearVariableNames"

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
extraArgs
	^ extraArgs ifNil: [0]

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
extraArgs: aNumber
   "no changes to variableNodes array ; only sender is RubyArgsNode,  
    and it adds extra args to variableNames after normal ones. "
  extraArgs := aNumber .
  TraceLocals >= 1 ifTrue:[ 
	  self trace: 'set extraArgs to ' , aNumber asString  .
  ].

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
inBootstrap
  ^ inBootstrap 

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
inBootstrap: aBoolean 
   inBootstrap := aBoolean

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
isArgumentScope

	 ^ isArgumentScope

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
isArgumentScope: a
	isArgumentScope := a

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
leafAt: aRubyVarLocation 
   "creates a VarLeaf if needed."
   | depth |
   depth := aRubyVarLocation depth. 
  ^ (self scopeAt: depth) leafForVarInfo: aRubyVarLocation varInfo .

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
leafForVarAssoc: anAssoc 
  | ofs aLeaf  nA |
  ofs := variableNames indexOfIdentical: anAssoc .
  ofs == 0 ifTrue:[ self error: ' assoc not found for leaf' ].
  ^ self _leafForVarAssoc: anAssoc at: ofs 

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
leafForVarInfo: anInfo 
  | ofs aLeaf  nA |
  ofs := variableNames indexOfIdentical: anInfo .
  ofs == 0 ifTrue:[ self error: ' VarInfo not found for leaf' ].
  ^ self _leafForVarAssoc: anInfo at: ofs 

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
locationForExistingName: aName depth: aNumber
  | vAssoc res  |
  vAssoc := self _findVariable: aName .
  vAssoc ifNotNil:[
    (res := RubyVarLocation _basicNew) varInfo: vAssoc depth: aNumber scope: self .
    ^ res
  ] ifNil:[
    ^ enclosingScope ifNotNil:[
        enclosingScope locationForExistingName: aName depth: aNumber + 1  
    ]
  ]

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
locationForName: aName
  | res |
  aName _isSymbol ifFalse:[  self error:'temp/arg name not a symbol'].
  res := self locationForExistingName: aName depth: 0.
  res ifNotNil:[ ^ res ].
 
  ^ self newVarLocation: aName 

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
matches: other
	other species = self species ifFalse: [^ false].
	requiredArgs = other requiredArgs ifFalse: [^ false].
	self extraArgs = other extraArgs ifFalse: [^ false].
	self variableNameKeys = other variableNameKeys ifFalse: [^ false].
	^ true

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
nameSpace
  ^ nameSpace  "a RubyNameSpace, or a Module"

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
newEvaluationTemp
  | nam idx vInfo tNames ofs vnams |
  idx := 1 .
  tNames := #( aTmp1 aTmp2 aTmp3 aTmp4 aTmp5 aTmp6 ) .
  [ true ] whileTrue:[
    nam := tNames atOrNil: idx .
    nam ifNil:[ nam := ( ':aTmp' , idx asString ) asSymbol  ] . 
    (vInfo := self _findVariable:  nam) ifNil:[  
       ^ self _addVarInfo: RubyScopeVarInfo name: nam kind: #evaluationTmp
    ].
    idx := idx + 1
  ].

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
newVarLocation: aName 
  | vInfo res encl |
  enclosingScope class == RubyEvalScope ifTrue:[
    res := enclosingScope newVarLocation: aName .
    res ifNotNil:[ ^ res ].
  ].
  vInfo := self _addVarInfo: RubyScopeVarInfo name: aName kind: #normal .
  TraceLocals >= 1 ifTrue:[  self trace:'added ' , aName , ' to variableNames '].
  res := RubyVarLocation _basicNew varInfo: vInfo depth: 0 scope: self .
  ^ res

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
nonInheritingChildOf: aScope
    inBootstrap := aScope inBootstrap  .
    variableNames := { } . "inline clearVariableNames" 
    requiredArgs := 0 .

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
numArgs
	^ requiredArgs + self extraArgs

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
requiredArgs

	 ^ requiredArgs

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
requiredArgs: aNumber
	requiredArgs := aNumber

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
restArg

	 ^ restArg

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
restArg: aNumber
	restArg := aNumber

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
restArgs
	^ restArg = 1 ifTrue: [1] ifFalse: [0]

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
scopeAt: aDepth
	|  scope |
	scope := self.
	aDepth timesRepeat: [scope := scope enclosingScope].
	^ scope  

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
trace: aString

  GsFile gciLogServer: 'Scope ' ,  self asOop asString , ': ' , aString

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
variableNameKeys

  ^ variableNames collect:[:assoc | assoc key ]

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
_addVarInfo: infoClass name: nam kind: kindSym
   "infoClass is RubyScopeVarInfo or subclass thereof"
    | vnams ofs vInfo |
  vnams := variableNames .
  ofs := vnams size + 1 .
  vInfo := infoClass new: nam kind: kindSym ofs: ofs .
  vnams at: ofs put:  vInfo .
  ^ vInfo

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
_argLeafAtOffset: aNumber
  ^ self _leafForVarAssoc: (variableNames at: aNumber) at: aNumber

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
_findVariable: aSymbol 
  |  anAssoc vNames  |
  1 to: (vNames := variableNames) size do:[:j |
	(anAssoc := vNames at: j) key == aSymbol ifTrue:[  ^ anAssoc ]
  ].
  ^ nil  

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
_leafForVarAssoc: varInfo at: anOffset
   | aLeaf nA  |
   (aLeaf := varInfo leaf) ifNil:[  | nam |
      nam := varInfo key .
      varInfo kind == #blockArg ifTrue:[
        aLeaf := self newArgLeafNamed: nam number: -1  
     ] ifFalse:[
        anOffset <= (nA := self numArgs) ifTrue:[
          aLeaf := self newArgLeafNamed: nam number: anOffset 
        ] ifFalse: [
           aLeaf := self  newTempLeafNamed: nam  .
        ].
     ].
     varInfo leaf: aLeaf .
  ].
  ^ aLeaf

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
_nameSpace: aNameSpace
   nameSpace := aNameSpace  "a RubyNameSpace, or a Module"

%


set class RubyStaticScope
category: '*maglev-runtime'
method:
_newLeaf: nam offsetInScope: anOffset kind: kind
  | aLeaf nA  |
  kind == #blockArg ifTrue:[
    aLeaf := self newArgLeafNamed: nam number: -1  
  ] ifFalse:[
    anOffset <= (nA := self numArgs) ifTrue:[
    aLeaf := self newArgLeafNamed: nam number: anOffset 
    ] ifFalse: [
      aLeaf := self  newTempLeafNamed: nam  .
    ].
  ].
  ^ aLeaf

%

