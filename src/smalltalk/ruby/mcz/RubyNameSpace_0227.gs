
set class RubyNameSpace class
category: '*maglev-runtime'
method:
comment
^ '
  instVars
    parent - a RubyNameSpace , or nil for the top scope
    name   - aString, for debugging
    myClass - the Class for which cls.rubyNameSpace==thisRubyNameSpace
        the RubySymbolAssociation key==self.name , value=aClassOrModule
                is in the parent scope
    assocInParent - the RubySymbolAssociation in the parent scope such that
         assoc.value.rubyNameSpace == self    (for a Module or class) or
         assoc.value == self                    (for Module compiled but not defined? yet)

    deferList - an Array (non-nil only in the root scope)
			
    methodProtection  0 - none, 1 protected, 2 private ,
       captures most recent zero-arg  private or protected directive for myClass
			
'

%


set class RubyNameSpace class
category: '*maglev-runtime'
method:
delim
  ^ '::'  "PathDelim hardcoded"

%


set class RubyNameSpace class
category: '*maglev-runtime'
method:
initialize
  self _addClassVar: #TraceCount value: 0 ;
      _addClassVar: #TraceGlobals value: 0  ;
      _addClassVar:#TrapLookup value: false .

%


doit
RubyNameSpace initialize.
%


set class RubyNameSpace class
category: '*maglev-runtime'
method:
initTopScope: envId
    "returns the transient top level name space"
  | tns pns |
  pns := RubyNameSpace new .
  pns initializeForModule: Object  env: envId .

  tns := RubyTransientNameSpace new .
  tns initializeForModule: Object persistentCopy: pns env: envId .
  Object _setTopNameSpace: tns persistent: pns env: envId .
  
  TraceGlobals >=2 ifTrue:[ GsFile gciLogServer: 'initialized root scope: ' , self describe ].
  ^ tns 

%


set class RubyNameSpace class
category: 'as yet unclassified'
method:
pathToString: aPath
  | res delim pSize |
  (pSize := aPath size) == 0 ifTrue:[ ^ '' ].
  (res := String new) addAll: (aPath at: 1).
  delim := self delim  .
  2 to: pSize do:[:j | 
    res addAll: delim ; addAll: (aPath at:j)
  ].
  ^ res

%


set class RubyNameSpace class
category: '*maglev-runtime'
method:
traceFile: prefix name: aName
  
  TraceGlobals >=1 ifTrue:[ 
     GsFile gciLogServer: prefix , (RubyFile pathForTrace: aName)
  ]

%


set class RubyNameSpace class
category: '*maglev-runtime'
method:
traceGlobals
   ^ TraceGlobals

%


set class RubyNameSpace class
category: '*maglev-runtime'
method:
traceGlobals: anInt 
  "0 means no trace, 1 means trace deferred globals, 2 trace all"
  TraceGlobals := anInt  .  
  TraceCount  := 0 .

%


set class RubyNameSpace class
category: '*maglev-runtime'
method:
trapLookup: aBool
  TrapLookup := aBool 

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
addAssociation: anAssociation
| key pns |
"Adds the argument to the receiver."
(anAssociation isKindOf: SymbolAssociation) ifFalse:[
    ArgumentTypeError signal:' expected a kindOf SymbolAssocation'
].
anAssociation _objectSecurityPolicy: self objectSecurityPolicy .
self _at: (key := anAssociation key) put: anAssociation .
"callers responsible for replicating store to persistentCopy"

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
at: aSymbol classVarPut: aValue 
  | assoc pns |
moduleFrozen ifTrue:[ ArgumentTypeError signal:'attempt to modify a frozen module/class'].
assoc := self _at: aSymbol otherwise: nil .    "aSymbol == #Y ifTrue:[ self pause ]."
assoc ifNotNil:[ "found in tns"
  assoc _value: aValue  "store to tns"
] ifNil:[   | newAssoc |
  newAssoc := RubyClassVarAssociation newWithKey: aSymbol .
  newAssoc _value: aValue .
  self addAssociation: newAssoc 
].

^ aValue

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
at: aKey compilePut: aValue 
  "used by RubyFile>>load and RubyNameSpace>>childScopeAt:isDefine: .
   no replication of store to persistent copy. " 
| anAssoc |
anAssoc := self _at: aKey otherwise: nil .  
anAssoc ifNil:[ | newAssoc |
    newAssoc := RubySymbolAssociation newWithKey: aKey .  
    aValue ifNotNil:[ newAssoc _compileTimeValue: aValue ].
    self _at: aKey put: newAssoc.    
] ifNotNil:[
  anAssoc _compileTimeValue: aValue
].
^aValue

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
at: aKey put: aValue 

self error:'use at:runtimePut:  or at:compilePut: '

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
at: aSymbol runtimePut: aValue 
  | assoc pns |
moduleFrozen ifTrue:[ ArgumentTypeError signal:'attempt to modify a frozen module/class'].
assoc := self _at: aSymbol otherwise: nil .    "aSymbol == #Y ifTrue:[ self pause ]."
assoc ifNotNil:[ 
  assoc _value: aValue  
] ifNil:[   | newAssoc |
  newAssoc := RubySymbolAssociation newWithKey: aSymbol .
  newAssoc _value: aValue .
  self addAssociation: newAssoc 
].
^ aValue

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
at: aSymbol runtimePutInvariant: aValue 
  "used in bootstrap only, receiver should be a peristent name space"
| anAssoc |

anAssoc := self resolveConstant: aSymbol .
anAssoc == nil ifTrue:[ | newAssoc |
    newAssoc := RubySymbolAssociation newWithKey: aSymbol .
    newAssoc _value: aValue .
    newAssoc immediateInvariant .
    self addAssociation: newAssoc.
] ifFalse:[  
  anAssoc _value: aValue; immediateInvariant 
].
^aValue

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
bootConstantLookup: aSym env: env  
   "compileTime resolveConstant, returns an Association or nil"
  | ns |
  ns := self .
  [ ns ~~ nil ] whileTrue:[ | par |
    (ns resolveConstant: aSym) ifNotNil:[: assoc | ^ assoc ].
    ns :=  ns parent .
  ].
  ^ nil

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
classOrNameSpaceAt: aSymbol inModule: aModule 
  | assoc val | 
  assoc := self resolveConstant: aSymbol .
  assoc ifNotNil:[  
    assoc := assoc _classFor: aSymbol inModule: aModule env: envId .   "may trigger autoload"
    assoc ifNil:[  "re-resolve after autoload"
      assoc := self resolveConstant: aSymbol .
    ].
    assoc ifNotNil:[ val := assoc _valueNoAction ]. "for boot, no check of isDefined"
  ].
  val ifNil:[ ^ nil ].
  val isNameSpace ifTrue:[   | cls |
    cls := val myClass .
    cls ifNil:[ ^ val "return a name space" ].
    cls isBehavior ifTrue:[ ^ cls ].
  ] ifFalse:[
    val isBehavior ifTrue:[ ^ val ] .
  ].
  ArgumentTypeError signal: 'value is not a Module or Class'

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
delim
	^ self class delim .

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
describe
   "For debugging and topaz printing only"
  | path done scop nxt |
  path := { } .
  scop := nil .
  nxt := self .
  [ scop == nxt or: [nxt == nil] ] whileFalse:[
	 scop := nxt .
	 path insertAll: { scop name }  at: 1.
	 nxt := scop parent .
  ].  
  ^ '(RubyNameSpace ' , (self class pathToString: path) , ' lev ' , path size asString , ')'     

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
envId
  ^ envId

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
freezeModule
 
  RubyCompilerState current persistenceMode ifTrue:[
    moduleFrozen := true 
  ].

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
fullName
  | par nm |
  ((par := self parent) == nil or:[ par myClass == Object]) ifTrue:[ 
    nm := name .
    ^ String withAll: (nm ifNil:['']) 
  ].
  nm := par fullName . 
  nm addAll: '::' ; addAll: name .
  ^ nm

%


set class RubyNameSpace
category: '*maglev-cextensions'
method:
fullNameSymbol
  | par nm |
  ((par := parent) == nil or:[ par myClass == Object]) ifTrue:[ 
    nm := name .
    ^ nm ifNil:[ #'' ]
  ].
  nm := par fullName . 
  nm addAll: '::' ; addAll: name .
  ^ nm asSymbol .

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
initializeChildOf: parentScop name: sym 
  | assoc ps |
  name := sym .                 
  self parent: (ps := parentScop transientCopy)  .   
  envId := parentScop envId .
  
  "myClass, persistentCopy   left as nil" 
  methodProtection := 0 . 
  moduleFrozen := false .
  ^ self  

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
initializeForModule: aModule   env: envArg
  envId := envArg .
  methodProtection := 0 .
  myClass := aModule .
  (aModule == Object and:[ parent ~~ nil ]) ifTrue:[
      self error:'Object''s name space parent non-nil'
  ].
  name := aModule rubyNameForModuleInit .
  moduleFrozen := false .
  "parent left as nil"
  "caller responsible for parent "

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
initialValueForGlobal: aSym
  | res |
  aSym == #'$/'  ifTrue:[  
    "Note, #'$-0'  is translated to #'$/' by   RubyNameSpace>>synonimFor: "
    res := String new:1 .  res at: 1 put: (Character lf) .
    res immediateInvariant .
    ^ res
  ].
  (aSym == #'$:' or: [ aSym ==#'$"' ]) ifTrue: [  ^ { } ] .
  (aSym == #'$VERBOSE' or: [ aSym == #'$DEBUG' ]) ifTrue: [  ^ false ] .
  TraceGlobals >= 2 ifTrue:[
    GsFile gciLogServer:' global ' , aSym printString ,  ' has initial value nil'
  ].
  ^ nil

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
isNameSpace
  ^ true

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
methodProtection
  ^ methodProtection

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
methodProtection: anInt 
  "anInt :  0==public, 1==protected, 2==private"
  "(anInt == 2 or:[ methodProtection == 2]) ifTrue:[ self pause ]. "

  ^ methodProtection := anInt 
  "not reflected to persistent name space "

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
moduleFrozen
  ^ moduleFrozen

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
myClass
  ^ myClass 

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
name
  ^ name

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
nameSpace
   ^ self

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
parent
  "return parent scope per name scoping "

  ^ parent 

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
parent: aNameSpace
  
  "can't assert that Object's name space has parent == nil   because
      parent is installed before myClass when parsing.  Trac 607" 
 
  aNameSpace ifNotNil:[
    aNameSpace class == RubyNameSpace ifFalse:[
      self error:'invalid name space'
    ]
  ].
  parent == aNameSpace ifFalse:[ "avoid unnecessary write"
    parent := aNameSpace
  ]

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
parent: aTransientNs name: aSymbol
  | pns n |
  (n := name) == aSymbol ifFalse:[ 
    n == #'' ifFalse:[ self error:'parent:name: - inconsistent name' ].
    name := aSymbol .
  ].
  self parent: aTransientNs .

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
parentFor: aName
  "Returns a scope for specified name, 
   or returns nil if caller should start search at top scope using aName.
   used during compile time constant resolution for bootstrap code."
  | ns sym  |
  sym := aName asSymbol .
  ns := self parent .
  [ ns ~~ nil ] whileTrue:[ | val next |
    val := ns _childScopeAt: sym .
    val ifNotNil:[ ^ val  ].
    ns := ns parent .
  ].
  ^ nil

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
removeConst: aSymbol
  | assoc  |
  moduleFrozen ifTrue:[ ArgumentTypeError signal:'attempt to modify a frozen module/class'].
  RubyCompilerState current persistenceMode ifTrue:[
     assoc := self _removeKey: aSymbol ifAbsent:[ nil ].
  ] ifFalse:[
    assoc := self associationAt: aSymbol otherwise: nil .
    assoc ifNotNil:[
      ArgumentTypeError signal:'cannot remove constant from persistent module in transient mode'.
    ]
  ].
  assoc ifNil:[
     NameError signal: 'remove_const, ' , aSymbol , ' not found '
  ].
  ^ assoc _valueNoAction

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
removeKey: aSymbol
  |  pm |
  moduleFrozen ifTrue:[ ArgumentTypeError signal:'attempt to modify a frozen module/class'].
  pm := RubyCompilerState current persistenceMode .
  pm ifTrue:[
    self _removeKey: aSymbol ifAbsent:[] .
  ] ifFalse:[
    (self includesKey: aSymbol) ifTrue:[
         NameError signal:'cannot remove ' , aSymbol , 
        ' , persistenceMode==false and variable is persistent' 
    ].
  ].
 

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
resolveConstant: aSymbol
  "runtime constant resolution, returns a RubySymbolAssociation or nil"
  ^ self _at: aSymbol otherwise: nil . 

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
rubyAlias: newKey from: oldKey
  "runtime support for GlobalVarAliasNode, receiver should be
    transient name space for Object "
  | oldAssoc newAssoc  |
  (oldKey at:1) == $$ ifFalse:[ ArgumentError signal:'expected a global variable name'].
  (newKey at:1) == $$ ifFalse:[ ArgumentError signal:'expected a global variable name'].
  (RubyGlobalVarNode rubyAlias: newKey from: oldKey) ifFalse:[
    "not a global with special code generation"
    (self rubyGlobalVarAssoc:  newKey ) aliasTo: (self rubyGlobalVarAssoc: oldKey ) .
  ].
  ^ nil

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
rubyAutoload: sym file: fileToLoad
  | assoc |
  fileToLoad size == 0 ifTrue:[ 
     ArgumentError signal:'empty filename passed to autoload'
  ].
  (sym size == 0 or:[ (sym at: 1) isUppercase not ]) ifTrue:[
     NameError signal:'invalid constant name passed to autoload'
  ].
  assoc := self resolveConstant: sym .
  assoc ifNotNil:[
    assoc isDefined ifNotNil:[ ^ nil ].
    assoc class == RubyAutoloadAssociation ifFalse:[
      "remove key->NameSpace assoc created during walkWithScope for a Module"
      self removeKey: sym .
      assoc := nil .
    ].
  ].
  assoc ifNil:[ self addPersistentAssociation: 
             (RubyAutoloadAssociation newWithKey: sym file: fileToLoad) .
  ].
  ^ nil .

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
rubyConstAssociationAt: aSymbol
  | anAssoc |
  (anAssoc := self resolveConstant: aSymbol ) ifNil:[
      ^ NameError signal:'constant ', aSymbol , ' , not found'
  ].
  ^ anAssoc

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
rubyGlobalVar: aSymbol
  " for transient name spaces only"
  | assoc |
  (assoc := self _at: aSymbol otherwise: nil) ifNil:[ | val |
    assoc := RubyGlobalVarAssociation newWithKey: aSymbol .
    assoc globalVarValue: ( val := self initialValueForGlobal: aSymbol )  .
    self addAssociation: assoc .
    ^ val
  ] ifNotNil:[
    ^ assoc globalVarValue
  ]

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
rubyGlobalVar: aSymbol put: aValue
  " for transient name spaces only. returns an Association"
  | assoc |
  (assoc := self _at: aSymbol otherwise: nil) ifNil:[
     assoc := RubyGlobalVarAssociation newWithKey: aSymbol .
     self addAssociation: assoc .
  ].
  assoc globalVarValue: aValue .
  ^ assoc 

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
rubyGlobalVarAssoc: aSymbol
  " for transient name spaces only"
  | assoc |
  (assoc := self _at: aSymbol otherwise: nil) ifNil:[  
    assoc := RubyGlobalVarAssociation newWithKey: aSymbol .
    assoc globalVarValue: ( self initialValueForGlobal: aSymbol )  .
    self addAssociation: assoc .
  ].
  ^ assoc 

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
scopeAtPath: aPath  env: eid  
  "Search upwards from receiver for a scope for which aPath is a suffix
   of the scope's full path."
  | scop nam |
 
  (aPath at: 1) = RubyNameSpace delim ifTrue:[ "a path beginning with :: "
      ^ (Object transientNameSpaceForStore: eid) 
          scopeAtPath: (aPath copyFrom: 2 to: aPath size)  env: eid
  ].
  scop := self scopeForPrefix: aPath isDefine: true env: eid  .
  ^ scop childScopeAt: aPath last isDefine: true env: eid 

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
scopeForPrefix: aPath isDefine: definBool env: eid 
  | pSize scop idx fst |
  
  (pSize := aPath size) > 1 ifTrue:[
    scop := self childScopeAt: (fst := aPath first asSymbol) isDefine: false env: eid .
    idx := 2 .
    scop ifNil:[
		scop := self parentFor: fst .
		scop ifNil:[ scop := Object . idx := 1 ].  
    ].
    [ idx <= (pSize - 1) ] whileTrue:[
       scop := scop childScopeAt: (aPath at: idx) isDefine: definBool env: eid .
       idx := idx + 1.
       scop ifNil:[  ^ nil ].
    ]
  ] ifFalse:[
     scop := self 
  ].
  ^ scop

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
transientCopy
  | cls |
  ^ (cls := myClass) ifNil:[ self error:'transient copy not available' ]
                ifNotNil:[ cls transientNameSpaceForStore: envId ]

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
_childScopeAt: aName
  |  sym ns assoc |
  sym := aName asSymbol .
  assoc := self resolveConstant: sym .
  assoc ifNotNil:[ ns := assoc _value ].  
  ns ifNotNil:[
    ns isBehavior ifTrue:[ | aCls |
       aCls := ns .
       ns := aCls transientNameSpaceForStore: envId .
    ].
  ].
  ^ ns

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
_name: aSymbol
 
  "used in bootstrap only to override smalltalk class names"
  name := aSymbol 

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
_parent
  ^ parent

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
_rubyAlias: newKey fromAssoc: oldAssoc
  "for use in VM initialization only"
   (self rubyGlobalVarAssoc:  newKey ) aliasTo: oldAssoc 

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
_rubyConstants
  ^ self _rubyConstants: nil .

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
_rubyConstants: aSet
  | names |
  names := aSet ifNil:[ IdentitySet new ].
  self associationsDo:[ :assoc |
       (assoc isDefined ~~ nil and:[ assoc class ~~ RubyClassVarAssociation]) ifTrue:[
             names add: assoc key
       ].
  ].
    ^ names .

%


set class RubyNameSpace
category: '*maglev-runtime'
method:
_rubyConstantsFreeze
  | pns |
  self associationsDo:[ :assoc |
    assoc isDefined ifNotNil:[ assoc _freezeConstant ]
  ].
  

%

