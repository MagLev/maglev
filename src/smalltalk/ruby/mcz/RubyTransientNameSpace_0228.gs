
set class RubyTransientNameSpace
category: 'as yet unclassified'
method:
addPersistentAssociation: assoc
  "adds assoc by identity to both transient and
  persistent name spaces, per the persistenceMode "
  | pns aSymbol |
  aSymbol := assoc key .
  moduleFrozen ifTrue:[ ArgumentTypeError signal:'attempt to modify a frozen module/class'].
  (self _at: aSymbol otherwise: nil ) ifNotNil:[
    ArgumentTypeError signal:'key already in transient name space'
  ].
  self addAssociation: assoc .
  ((pns := persistCopy) ~~ nil and:[RubyCompilerState current persistenceMode]) ifTrue:[
    (pns  _at: aSymbol otherwise: nil ) ifNotNil:[
      ArgumentTypeError signal:'key already in persistent name space'
    ].
    pns addAssociation: assoc
  ].
  ^ assoc

%


set class RubyTransientNameSpace
category: '*maglev-runtime'
method:
addTransientAssociation: anAssociation
| key |
"Adds the argument to the receiver."
moduleFrozen ifTrue:[ ArgumentTypeError signal:'attempt to modify a frozen module/class'].
(anAssociation isKindOf: SymbolAssociation) ifFalse:[
        ArgumentTypeError signal:' expected a kindOf SymbolAssocation'
].
self _at: (key := anAssociation key) put: anAssociation .

%


set class RubyTransientNameSpace
category: 'as yet unclassified'
method:
at: aSymbol classVarPut: aValue 
  |  pns |
super at: aSymbol classVarPut: aValue .

((pns := persistCopy) ~~ nil and:[RubyCompilerState current persistenceMode]) ifTrue:[
  pns at: aSymbol classVarPut: aValue 
].
^ aValue


%


set class RubyTransientNameSpace
category: 'as yet unclassified'
method:
at: aSymbol runtimePut: aValue 
  | assoc pns |
super at: aSymbol runtimePut: aValue .

((pns := persistCopy) ~~ nil and:[RubyCompilerState current persistenceMode]) ifTrue:[
  pns at: aSymbol runtimePut: aValue 
].
^ aValue

%


set class RubyTransientNameSpace
category: 'as yet unclassified'
method:
at: aSymbol transientRuntimePut: aValue 
| assoc pns |
moduleFrozen ifTrue:[ ArgumentTypeError signal:'attempt to modify a frozen module/class'].
assoc := self _at: aSymbol otherwise: nil .
assoc ifNotNil:[ "found in tns"
  assoc _value: aValue  "store to tns"
] ifNil:[   | newAssoc |
  newAssoc := RubySymbolAssociation newWithKey: aSymbol .
  newAssoc _value: aValue .
  self addAssociation: newAssoc 
].
^ aValue

%


set class RubyTransientNameSpace
category: 'as yet unclassified'
method:
bootAddConstAssociation: aName env: eid 
  "only used in bootstrap, returns a persistent association"
  | assoc sym ns |
  RubyCompilerState current persistenceMode ifFalse:[
    self error:'bootAddConstAssociation: , expected persistenceMode==true' .
  ]. 
  ns := persistCopy ifNil:[ self ].
  sym := aName asSymbol. 
  assoc := ns _at: sym otherwise: nil . 
  assoc ifNil:[ | val | 
     assoc:= RubySymbolAssociation newWithKey: sym . 
     ns addAssociation: assoc .
     TraceGlobals >= 2 ifTrue:[
        GsFile gciLogServer:'added key ', sym , ' to  ' , self describe 
     ].
  ].
  ^ assoc

%


set class RubyTransientNameSpace
category: 'as yet unclassified'
method:
childScopeAt: sym  isDefine: definBool env: eid 
    |  ns assoc  child |
  assoc := self resolveConstant: sym .
  assoc ifNotNil:[ child  := assoc _value ].  
  child ifNotNil:[ ^ child ]. 
  definBool ifFalse:[ ^ nil ].

  ns := self class new initializeChildOf: self name: sym .

  assoc := self resolveConstant: sym .
  assoc ifNotNil:[
    "We can have an already existing autoload association"
    assoc _valueNoAction ifNotNil:[ self error:'value already defined in child scope'].
    (assoc isCommitted and:[ persistCopy ~~ nil ]) ifTrue:[ 
	   self at: sym compilePut: ns  "must store to transient name space, Trac 585"
	] ifFalse:[
	   assoc _compileTimeValue: ns .
	].
  ] ifNil:[ 
    self at: sym compilePut: ns 
  ].
  TraceGlobals >=2 ifTrue:[ 
      GsFile gciLogServer:'initialized scope: ' , ns describe . 
  ].
  ^ ns

%


set class RubyTransientNameSpace
category: '*maglev-runtime'
method:
copyAssociationsToPns
  | pns |
  "caller checks persistenceMode"
  (pns := persistCopy) ifNotNil:[
    self associationsDo:[:assoc |  pns addAssociation: assoc ].
  ].

%


set class RubyTransientNameSpace
category: '*maglev-runtime'
method:
freezeModule
  | pns |
  moduleFrozen := true .
  (pns := persistCopy) ifNotNil:[  
    RubyCompilerState current persistenceMode ifTrue:[
      pns freezeModule 
    ].
  ].

%


set class RubyTransientNameSpace
category: 'as yet unclassified'
method:
initializeForModule: aModule persistentCopy: pns  env: envArg
  self persistentCopy: pns .
  self initializeForModule: aModule  env: envArg .
  pns ifNotNil:[
	 moduleFrozen := pns moduleFrozen .
	 name := pns name .  "ensure use of override of smalltalk name"
  ].

%


set class RubyTransientNameSpace
category: '*maglev-runtime'
method:
parent
  | p |
  (p := parent) ifNotNil:[ ^ p ].
  (p := persistCopy)  ifNotNil:[ ^ p parent ].
  ^ nil

%


set class RubyTransientNameSpace
category: '*maglev-runtime'
method:
parent: aNameSpace
  | pns par |
  (par := parent) ifNotNil:[ 
     par ==  aNameSpace ifFalse:[ 
       self error:'module transientNameSpace already has a parent'.
     ]
  ].
  "can't assert that Object's name space has parent == nil   because
      parent is installed before myClass when parsing.  Trac 607" 
  parent := aNameSpace .  
  (pns := persistCopy) ifNotNil:[ 
    RubyCompilerState current persistenceMode ifTrue:[
      pns parent: (aNameSpace ifNotNil:[ aNameSpace persistentCopy ])
    ]
  ]

%


set class RubyTransientNameSpace
category: 'as yet unclassified'
method:
persistentCopy
 
  ^  persistCopy 

%


set class RubyTransientNameSpace
category: 'as yet unclassified'
method:
persistentCopy: aNameSpace
 
  persistCopy := aNameSpace

%


set class RubyTransientNameSpace
category: 'as yet unclassified'
method:
removeConst: aSymbol
  | assoc pns passoc |
  moduleFrozen ifTrue:[ ArgumentTypeError signal:'attempt to modify a frozen module/class'].
  assoc := self _removeKey: aSymbol ifAbsent:[ nil ].
  (pns := persistCopy) ~~ nil ifTrue:[
     RubyCompilerState current persistenceMode ifTrue:[
       passoc := pns _removeKey: aSymbol ifAbsent:[ nil ].
       assoc ifNil:[ assoc := passoc ].
     ] ifFalse:[
       passoc := pns associationAt: aSymbol otherwise: nil .
       passoc ifNotNil:[
         ArgumentTypeError signal:'cannot remove constant from persistent module in transient mode'.
       ]
     ]
  ].
  assoc ifNil:[
     NameError signal: 'remove_const, ' , aSymbol , ' not found '
  ].
  ^ assoc _valueNoAction

%


set class RubyTransientNameSpace
category: '*maglev-runtime'
method:
removeKey: aSymbol
  | pns |
  moduleFrozen ifTrue:[ 
      ArgumentTypeError signal:'attempt to modify a frozen module/class'].
  self _removeKey: aSymbol ifAbsent:[] .
       "callee, i.e.  pns removeKey , checks persistenceMode "
  (pns := persistCopy) ifNotNil:[  pns removeKey: aSymbol ].

%


set class RubyTransientNameSpace
category: 'as yet unclassified'
method:
resolveConstant: aSymbol
  "runtime constant resolution, returns a RubySymbolAssociation or nil"
  | assoc pns |
  assoc := self _at: aSymbol otherwise: nil .
  assoc ifNil:[
    (pns := persistCopy ) ifNotNil:[ assoc := pns _at: aSymbol otherwise: nil ].
  ].
  ^ assoc

%


set class RubyTransientNameSpace
category: 'as yet unclassified'
method:
transientCopy
  ^ self

%


set class RubyTransientNameSpace
category: 'as yet unclassified'
method:
_name: aSymbol
  | pns |
  "used in bootstrap only to override smalltalk class names"
  super _name: aSymbol .
  (pns := persistCopy)  ifNotNil:[ pns _name: aSymbol ].
	

%


set class RubyTransientNameSpace
category: 'as yet unclassified'
method:
_rubyConstants
  "Return an IdentitySet of the names of the constants defined"
  | names pns   |
  names := super _rubyConstants: nil .
  (pns := persistCopy) ifNotNil:[  pns _rubyConstants: names ].
  ^ names 

%


set class RubyTransientNameSpace
category: 'as yet unclassified'
method:
_rubyConstantsFreeze
  | pns |
  super _rubyConstantsFreeze .
  ((pns := persistCopy) ~~ nil and:[RubyCompilerState current persistenceMode]) ifTrue:[
    pns _rubyConstantsFreeze .
  ].

%

