
set class Metaclass3
category: '*maglev-runtime'
classmethod:
_rubyNew1: aSuperCls do: aBlock
  "a ruby primitive"
  | newCls | 
  newCls := aSuperCls _primSubclass: #''  
      instVarNames: #() 
      format: (aSuperCls format bitOr: GC_ClassCreationRuby  )
      constraints: #()
      classVars: nil 
      poolDictionaries: #() .
  newCls immediateInvariant . 
  aBlock ifNotNil:[ | defStk cld |
    cld := GsProcess _current clientData .
    (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: newCls .
    cld at: 7 put: newCls " _rubyThreadDataAt: 7 put: " .
    [ | blk |
      blk := aBlock setSelf: newCls .
      blk @ruby1:value  .
    ] ensure:[
      defStk pop: newCls
    ]
  ].
  ^ newCls

%


set class Metaclass3
category: '*maglev-runtime'
classmethod:
_rubyNew1: aSuperCls instVars: ivNames do: aBlock
  "a ruby primitive"
  | newCls | 
  newCls := aSuperCls _primSubclass: #''  
      instVarNames: ivNames
      format: (aSuperCls format bitOr: GC_ClassCreationRuby  )
      constraints: #()
      classVars: nil 
      poolDictionaries: #() .
  newCls immediateInvariant . 
  aBlock ifNotNil:[ | blk |
     blk := aBlock setSelf: newCls .
     blk @ruby1:value  .
  ].
  ^ newCls

%


set class Metaclass3
category: '*maglev-runtime'
method:
addModuleMethodIfEnabled: aSelector env: envId
  self isMeta ifTrue:[ ^ self "do nothing" ].  
  ^ super addModuleMethodIfEnabled: aSelector env: envId 

%


set class Metaclass3
category: '*maglev-runtime'
method:
allModuleMethodsEnabled: envId
  ^ false

%


set class Metaclass3
category: '*maglev-runtime'
method:
classForConstantLookup: envId forModuleEval: isModuleEvalBool
  isModuleEvalBool ifTrue:[  "fix Trac 484"
	 self isMeta ifTrue:[ ^ self ].
	 ^ self class  
  ] ifFalse:[
    self isMeta ifTrue:[ ^ destClass ].
    ^ self
  ].

%


set class Metaclass3
category: '*maglev-runtime'
method:
clsMethodDefnTarget
  ^ self class

%


set class Metaclass3
category: '*maglev-runtime'
method:
is_aModule
  ^ false

%


set class Metaclass3
category: '*maglev-runtime'
method:
methodDefined: aSymbol rubyEnv: envId
  "does not consider the package policy.
   Returns nil or an Array { cm . protectionOverride }."
  ^ self lookupSelector: aSymbol rubyEnv: envId

%


set class Metaclass3
category: '*maglev-runtime'
method:
moduleIncludeSelf: envId
  self shouldNotImplement: #moduleIncludeSelf:

%


set class Metaclass3
category: '*maglev-runtime'
method:
newRubySubclass: aString  instancesPersistent: ipersistBool

| result cst fmt |
(self subclassesDisallowed) ifTrue: [
   ^ self _error: #classErrSubclassDisallowed
].
(self instancesInvariant) ifTrue: [
  "assumes instancesInvariant:false  from caller"
  ^ self _error: #classErrInvariantSuperClass
].
fmt := format bitOr: GC_ClassCreationRuby .
        "assumes instancesInvariant:false  from caller"
ipersistBool ifFalse:[ fmt := fmt bitOr: GC_NON_PERSISTENT_MASK ].
result := self _subclass: aString asSymbol
          instVarNames: #()
          format: fmt   
          constraints: #()
          classVars: nil
          poolDictionaries: nil  .
             "result has
                 classInstVars: #()
                 poolDictionaries: ()
                 inDictionary: nil
                 inClassHistory: nil
                 description: nil
                 isModifiable: true "

(self class instSize > result class instSize) ifTrue: [
  self error: 'smalltalk class instance variables not supported for Ruby classes'
].
^ result

%


set class Metaclass3
category: '*maglev-runtime'
method:
newRubySubclass: aString  instancesPersistent: ipersistBool fixedIvs: ivList

| result cst fmt |
(self subclassesDisallowed) ifTrue: [
   ^ self _error: #classErrSubclassDisallowed
].
(self instancesInvariant) ifTrue: [
  "assumes instancesInvariant:false  from caller"
  ^ self _error: #classErrInvariantSuperClass
].
fmt := format bitOr: GC_ClassCreationRuby .
        "assumes instancesInvariant:false  from caller"
ipersistBool ifFalse:[ fmt := fmt bitOr: GC_NON_PERSISTENT_MASK ].
result := self _subclass: aString asSymbol
          instVarNames: ivList 
          format: fmt   
          constraints: #()
          classVars: nil
          poolDictionaries: nil  .
             "result has
                 classInstVars: #()
                 poolDictionaries: ()
                 inDictionary: nil
                 inClassHistory: nil
                 description: nil
                 isModifiable: true "

(self class instSize > result class instSize) ifTrue: [
  self error: 'smalltalk class instance variables not supported for Ruby classes'
].
^ result

%


set class Metaclass3
category: '*maglev-runtime'
method:
rubyAncestors
  "a ruby primitive"
  | arr cls  envId |
  envId := 1"__callerEnvId" .
  arr := { self }.
  cls := self .
  [ true ] whileTrue:[
     "ancestors of a Class stops at Kernel"
     cls := cls rubySuperclass:  envId .
     cls ifNil:[ ^ arr ].
     cls isRubyVirtual ifTrue:[ | pcopy |
       arr add: (pcopy := cls rubyPrimaryCopy).
       pcopy == Kernel ifTrue:[ ^ arr ]
     ] ifFalse:[
       (cls nameSpace: envId) ifNotNil:[ arr add: cls ]
                "ifNil:[ do not include a smalltalk class] ".
     ].
  ].

%


set class Metaclass3
category: '*maglev-runtime'
method:
rubyConstAssociationAtOrNil: aSymbol env: envId
  | rns assoc cls |
  cls := self .
  [ cls ~~ nil ] whileTrue:[ 
    "probe both normal and virtual classes"
     (rns := cls nameSpace: envId) ifNotNil: [
       (assoc := rns resolveConstant: aSymbol) ifNotNil:[
          ^ assoc
       ] .
     ] .
     cls := cls rubySuperclass: envId .
  ].
  ^ nil

%


set class Metaclass3
category: '*maglev-runtime'
method:
rubyFullName: envId
  "called from Smalltalk code only"
  | ns nam |
  (ns := self nameSpace: envId) ifNotNil:[ ^ ns fullName ].
  ((nam := name) isNil or: [nam size == 0]) ifTrue: [
    self isMeta ifTrue: [ ^ (destClass rubyFullName: envId), ':Class' ].
    ^ '#<Class:0x', self asOop hex, '>' ].
  ^ String withAll: nam

%


set class Metaclass3
category: '*maglev-cextensions'
method:
rubyFullNameSymbol: envId
  "called from Smalltalk code only"
  | ns nam |
  self isMeta ifTrue:[  ^ #'' ].
  (ns := self nameSpace: envId) ifNotNil:[ ^ ns fullNameSymbol ].
  (nam := name) ifNil:[ ^ #'' ].  "Ruby meta classes have no name"
  ^ nam asSymbol

%


set class Metaclass3
category: '*maglev-runtime'
method:
rubyIncludedModules
  "a ruby primitive"
  | arr cls envId |
  envId := 1"__callerEnvId" .
  arr := {  }.
  cls := self rubySuperclass: envId .
  [ true ] whileTrue:[
     cls ifNil:[ ^ arr ] .
     "do not stop at Object for Classes"
     cls isRubyVirtual ifTrue:[ | primary |
          (primary := cls rubyPrimaryCopy) class == Module ifTrue:[
               arr add: primary
           ].
     ].
     cls := cls rubySuperclass: envId
  ].

%


set class Metaclass3
category: '*maglev-runtime'
method:
rubyMethods: includeSuper protection: protInt
  "a ruby primitive"
  self isMeta ifTrue:[ ^ IdentitySet new ].
  ^ self class rubyMethods: includeSuper protection: protInt env: 1"__callerEnvId"

%


set class Metaclass3
category: '*maglev-runtime'
method:
rubyMethods: includeSuper protection: protInt env: envId
  "Return an IdentitySet of Symbols "
| set curClass  hidden  |
  set := IdentitySet new . hidden := IdentitySet new .
  curClass := self .
  [ curClass ~~ nil ] whileTrue: [
    curClass nonBridgeRubySelectorsInto: set hiddenInto: hidden protection: protInt env: envId .
    includeSuper ifTrue:[ curClass := curClass rubySuperclass: envId  ]
                ifFalse:[ curClass := nil "terminate loop" ].
  ].
  ^  set

%


set class Metaclass3
category: '*maglev-runtime'
method:
rubyNameForModuleInit
 self isMeta ifTrue:[ ^ nil ] 
           ifFalse:[ ^ name ]

%


set class Metaclass3
category: '*maglev-runtime'
method:
setProtection: protInt classmethods: selectorsArray
  "a ruby primitive"
  | sz count |
  (sz := selectorsArray size) ~~ 0 ifTrue:[
    | cls |
    cls := self isMeta ifTrue:[ self ] ifFalse:[ self class ] .
    count := cls setProtection: protInt methods: selectorsArray 
          env: 1"__callerEnvId" longSels: nil .
    count == sz ifFalse:[ 
      sz == 1 ifTrue:[ NameError signal: (selectorsArray at: 1), ' not found' ].
      NameError signal: 'one or more selectors not found'.
    ].
  ].
  ^ self 

%


set class Metaclass3
category: '*maglev-runtime'
method:
_checkIncludeRubyModule: aModule 
  "a ruby primitive"
  aModule class == Module ifFalse:[
    ((aModule isKindOf: Module) and:[ (aModule isKindOf: Metaclass3) not]) ifFalse:[
      ArgumentTypeError signal:'argument to include is not a Module'
    ].
  ].
  aModule == self ifTrue:[
    ArgumentError signal:'cannot include a module in itself'.
  ].
  ^ true

%


set class Metaclass3
category: '*maglev-runtime'
method:
_classForRubyClassVar

  self isMeta ifTrue:[ ^ destClass ].
  ^ self

%


set class Metaclass3
category: '*maglev-runtime'
method:
_includeRubyModule: aModule 
  "a ruby primitive"
  ^ self addRubyVirtualSuperclass: aModule forMeta: self isMeta env: 1"__callerEnvId" .

%


set class Metaclass3
category: '*maglev-runtime'
method:
_rubyInspect
  "a ruby primitive"
  ^ self _rubyInspect: 1"__callerEnvId" 

%


set class Metaclass3
category: '*maglev-runtime'
method:
_rubyInspect: envId
  "called from smalltalk code"
   self isMeta ifTrue:[  | res |
     res := '#<Class:' copy .
     res add: (destClass rubyFullName: envId  ) ;
         add:  $>  .
     ^ res
   ] ifFalse:[
     ^ self rubyFullName: envId 
   ]


%

