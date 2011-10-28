
set class Object
category: '*maglev-runtime'
classmethod:
addException: anException
  | res |
  self == Object ifFalse:[ ArgumentTypeError signal:'expected anException or Object'].
  (res := ExceptionSet new)
     addException: Exception ;
     addException: anException .
  ^ res 

%


set class Object
category: '*maglev-cextensions'
classmethod:
cextGlobalVarAt: stringArg  put: aValue
  | assoc aString |
  aString := stringArg .
  aString size == 0 ifTrue:[ ^ nil ].  "rb_gv_set() checks for size 0"
  (aString at: 1) == $$ ifFalse:[
    aString := '$' , aString
  ]. 
  assoc := Object rubyGlobalVarAssoc: aString asSymbol env: 1 .
  assoc globalVarValue: aValue .
  ^ aValue

%


set class Object
category: '*maglev-cextensions'
classmethod:
cextGlobalVarGet: stringArg
  | sym assoc aString |
  aString := stringArg .
  aString size == 0 ifTrue:[ ^ nil ].
  (aString at: 1) == $$ ifFalse:[
    aString := '$' , aString 
  ].
  sym := Symbol _existingWithAll: aString .
  sym ifNil:[ ^ nil ].
  ^ (Object rubyGlobalVarAssoc: sym env: 1 ) globalVarValue

%


set class Object
category: '*maglev-cextensions'
classmethod:
cextGlobalVariables: envId 
  | ns arr |
  ns := Object transientNameSpace: envId .
  arr := { } .
  ns keysAndValuesDo:[ :aKey :aVal |  
    (aKey at: 1) ==  $$  ifTrue:[ arr add: (String withAll: aKey)].
  ].
  "  #'$?'  not currently included  , it is a runtime dynamic value"
  ^ arr 

%


set class Object
category: '*maglev-runtime'
method:
addRubyClassVar: aSymbol  value: aValue  mref: modulRef 
  "called from generated code"
   ^ modulRef _classForRubyClassVar  addRubyClassVar: aSymbol value: aValue env: 1"__callerEnvId"

%


set class Object
category: '*maglev-runtime'
method:
addRubySingletonClass: envId

"Insert a new singleton class in the receiver's class hierarchy.
 Returns receiver, or signals an error.
 The new class is persistable.  "

| vCls prevCls |
prevCls := self virtualClass .
vCls := self _addRubySingletonClass: true envId: envId .
vCls _isOneByteString ifTrue:[
  ArgumentTypeError signal: 'add singleton class disallowed, ', vCls .
].
"env 1 method dicts left as nil"
"name spaces left as nil."
vCls persistentRubySuperclass: envId put: prevCls .
vCls transientRubySuperclass: envId put: prevCls .
  "new singleton class has empty method dicts, so no need to clear
     lookup caches yet"
^ self

%


set class Object
category: '*maglev-runtime'
method:
classForConstantLookup: envId forModuleEval: aBoolean 
  "actual usage of this method
    is to get class for constant definition for top level of an eval"
  | cls |
  cls := self virtualClass .
  [ cls isRubySingletonClass or:[ cls isRubyModuleInclude] ] whileTrue:[
    cls := cls rubySuperclass: envId .
  ].
  ^ cls

%


set class Object
category: '*maglev-runtime'
method:
clsMethodDefnTarget
  ^ self class

%


set class Object
category: '*maglev-runtime'
method:
instanceEvalString: aString with: vcGlobalsArr args: argsArr
    "A ruby primitive.
     instance_eval comes here.
     Evaluate aString with self set to this object"
  | defStk envId lexSelfStk  defnTarget  selfCls cld |
  envId := 1"__callerEnvId" . 
  selfCls := self virtualClass   .
  defnTarget := self _singletonClass: envId .
  cld := GsProcess _current _clientData .
  (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: defnTarget .
  cld at: 7 put: defnTarget " _rubyThreadDataAt: 7 put: " .

  selfCls == Object ifTrue:[ selfCls := nil ].  "don't need to include top-level"
  (lexSelfStk := RubyCompilerState current lexicalSelfStack ) push: selfCls .
  ^ [  | file line |
      file := argsArr atOrNil: 2 .
      file _stringCharSize == 0 ifTrue:[ file := nil ].
      line := argsArr atOrNil: 3 .
      line _isSmallInteger ifFalse:[ line := 0 ].
      RubyCompiler new evaluateString: aString binding: (argsArr at: 1) with: vcGlobalsArr 
        fileName: file lineNumber: line  env: envId
    ] ensure:[
      defStk pop: defnTarget.
      lexSelfStk pop: selfCls .
    ]

%


set class Object
category: '*maglev-runtime'
method:
isNameSpace
  ^ false

%


set class Object
category: '*maglev-runtime'
method:
is_aModule
  ^ false

%


set class Object
category: '*maglev-runtime'
method:
nameSpace: envId
  self class == Object ifTrue:[
    ^ Object transientNameSpaceForStore: envId
  ].
  ArgumentTypeError signal:'left side of :: is not a class/module ' .
  ^ nil

%


set class Object
category: '*maglev-runtime'
method:
nameSpaceOrNil: envId
  self class == Object ifTrue:[
    ^ Object transientNameSpaceForStore: envId
  ].
  ^ nil

%


set class Object
category: '*maglev-runtime'
method:
not
	^ false

%


set class Object
category: '*maglev-runtime'
method:
rubyConstAssociationAt: aSymbol
  ArgumentTypeError signal: 'left side of :: is neither a class nor module'

%


set class Object
category: '*maglev-runtime'
method:
rubyConstAssociationAt: aSymbol env: envId
  ArgumentTypeError signal: 'left side of :: is neither a class nor module' .
  ^ nil

%


set class Object
category: '*maglev-runtime'
method:
rubyConstAssociationAtOrNil: aSymbol env: envId
  ArgumentTypeError signal: 'left side of :: is neither a class nor module' .
  ^ nil

%


set class Object
category: '*maglev-runtime'
method:
rubyConstAt: aName env: envId put: aValue
  ArgumentTypeError signal:'left side of :: is not a class/module '

%


set class Object
category: '*maglev-runtime'
method:
rubyConstAt: aSymbol put: aValue
  ArgumentTypeError signal:'left side of :: is not a class/module '

%


set class Object
category: '*maglev-runtime'
method:
rubyConstDecl: aSymbol put: aValue
   "called from generated code"
  ^ self virtualClass rubyConstAt: aSymbol env: 1"__callerEnvId" put: aValue .

%


set class Object
category: '*maglev-runtime'
method:
rubyEval1: lexPath block: aBlock
  "A ruby primitive, for instance_eval.  lexPath is ignored .
   Evaluate aBlock with the block's self set to this object"
  | stk defnTarget cld |
  cld := GsProcess _current _clientData .
  stk := cld at: 3 . " _rubyThreadDataAt: 3 , meth def target stack"
  stk push: (defnTarget := self _singletonClass: 1 ).
  cld at: 7 put: defnTarget . "_rubyThreadDataAt: 7 put: "
  ^ [ | val |
      aBlock ifNotNil:[ | blk |
        blk := aBlock setSelf: self . "copies aBlock if needed"
        val := blk @ruby1:value: self  .   
      ] .
      val
    ] ensure:[
      stk pop: defnTarget
    ]

%


set class Object
category: '*maglev-runtime'
method:
rubyInstvarAt: aString
  "a ruby primitive. "

  | sym stSym |
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString _rubyAt1: 0) == 64"$@" ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name'
  ].
  ((aString _rubyAt1: 1) == 95"$_" and:[ aString at: 2 equals:'_st_' ]) ifTrue:[
    stSym := (aString copyFrom: 6 to: aString size) asSymbol .
    sym := stSym
  ] ifFalse:[
    sym := aString asSymbol .
    stSym := (aString copyFrom: 2 to: aString size) asSymbol .
  ].
  ^ self _rubyInstvarAt: { stSym . sym . nil . 0 }

%


set class Object
category: '*maglev-runtime'
method:
rubyInstvarAt: aString put: aValue 
  "a ruby primitive "
  ^ self rubyInstvarAt: aString put: aValue env: 1"__callerEnvId" 

%


set class Object
category: '*maglev-runtime'
method:
rubyInstvarAt: aString put: aValue env: envId
  "Returns aValue"
  | sym  stSym |
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString _rubyAt1: 0) == 64"$@" ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name', self class rubyName
  ].
  ((aString _rubyAt1: 1) == 95"$_" and:[ aString at: 2 equals:'_st_' ]) ifTrue:[
    stSym := (aString copyFrom: 6 to: aString size) asSymbol .
    sym := stSym
  ] ifFalse:[
    sym := aString asSymbol .
    stSym := (aString copyFrom: 2 to: aString size) asSymbol
  ].
  ^ self _rubyInstvarAt: { stSym . sym . nil . 0 } put: aValue
             privateSize:  self rubyPrivateSize

%


set class Object
category: '*maglev-runtime'
method:
rubyInstVarDefined: aSymbol 
  "support method for generated code"
  ^ self _rubyInstVarDefinedQ: aSymbol

%


set class Object
category: '*maglev-runtime'
method:
rubyInstvarNames

"A ruby primitive.
 Returns an Array of Strings which are names
 of named and dynamic instVars of the receiver visible to Ruby.
 names in the result start with '@' ."

| arr sz res  |
arr :=  self _instvarNamesAfter: self rubyPrivateSize .
res := Array new: (sz := arr size) .
1 to: sz do:[ :n | | sym |
   sym := arr at: n .
   (sym at: 1) == $@
     ifTrue:[  res at: n put:( String withAll: sym)]
     ifFalse:[ | nam symSiz |
       nam := String new: (symSiz := sym size) + 5 .
       nam replaceFrom: 1 to: 5 with: '@_st_' startingAt: 1 .
       nam replaceFrom: 6 to: symSiz + 5 with: sym startingAt: 1 .
       res at: n put: nam
   ].
].
^ res

%


set class Object
category: '*maglev-runtime'
method:
rubyIvDefined: aString
  "a ruby primitive"
  | sym |
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString at: 1) == $@ ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name'
  ].
  ((aString at: 2) == $_ and:[ aString at: 2 equals:'_st_' ]) ifTrue:[
    sym := (aString copyFrom: 6 to: aString size) asSymbol
  ] ifFalse:[
    sym := aString asSymbol
  ].
  ^ self _rubyInstvarDefined: { sym . sym . nil . 0 }

%


set class Object
category: '*maglev-runtime'
method:
rubyMethod: aSymbol
  "a ruby primitive, and called from generated code.
   Returns an instance of RubyMeth , or signals a NameError"
   |  m  |
   m := self virtualClass rubyMethodFor: aSymbol env: 1"__callerEnvId" .
   m object: self .
   ^ m

%


set class Object
category: '*maglev-runtime'
method:
rubyMethods: includeSuper protection: protInt
  "a ruby primitive"
  ^ self virtualClass rubyMethods: includeSuper protection: protInt 
            env: 1"__callerEnvId"

%


set class Object
category: '*maglev-runtime'
method:
rubyPrint: aString
	GsFile gciLogClient: aString

%


set class Object
category: '*maglev-runtime'
method:
rubyRemoveIv: aString
  "a ruby primitive"
  | sym descr |
  aString _isOneByteString ifFalse:[
    NameError signal:'instance variable name is not a String'
  ].
  (aString at: 1) == $@ ifFalse:[
    NameError signal: aString , ' is not allowed as instance variable name'
  ].
  ((aString at: 2) == $_ and:[ aString at: 2 equals:'_st_' ]) ifTrue:[
    sym := (aString copyFrom: 6 to: aString size) asSymbol
  ] ifFalse:[
    sym := aString asSymbol
  ].
  descr := { sym . sym . nil . 0 } . 
  (self _rubyInstvarDefined: descr) == false ifFalse:[ | val |
      val := self _rubyInstvarAt: descr .
      self _rubyInstvarAt: descr put: _remoteNil privateSize: self rubyPrivateSize .
      ^ val
  ] ifTrue:[
     NameError signal:'instance variable ' , sym , ' not defined'
  ]

%


set class Object
category: '*maglev-runtime'
method:
rubySelectorForFrame: anInteger
	^ (GsProcess _methodInFrameContents: (GsProcess _frameContentsAt: anInteger)) selector

%


set class Object
category: '*maglev-runtime'
method:
rubySingletonClass
  " a ruby primitive"
  ^ self _singletonClass: 1"__callerEnvId" 

%


set class Object
category: '*maglev-runtime'
method:
rubySingletonClassForExtend
  "a ruby primitive"
  | envId cls |
  envId := 1 "__callerEnvId" .
  self class == Module ifTrue:[   "a  class<<self   within a  module "
    cls := self moduleMethodsModuleOrNil ifNil:[
       cls := self _rubyModuleIncludeSelfEnv: envId
    ].
  ] ifFalse:[
    self isBehavior ifTrue:[
       cls := self isMeta ifTrue:[ self _singletonClassFor: envId ]
                  ifFalse:[ self virtualClass "extending metaclass" ].
    ] ifFalse:[ cls := self _singletonClassFor: envId ].
  ].
  ^ cls

%


set class Object
category: '*maglev-runtime'
method:
rubySingletonMethods: includeModules protection: protInt
  "a ruby primitive"
  ^ self rubySingletonMethods: includeModules protection: protInt 
     env: 1"__callerEnvId"

%


set class Object
category: '*maglev-runtime'
method:
rubySingletonMethods: includeModules protection: protInt env: envId
  "Return an IdentitySet of Symbols, the names of the singleton methods for receiver.
   If receiver has no singleton methods, return an empty array.
   If includeModules is true, then also include methods from mixed in
   modules."
  | curClass set hidden |
  set := IdentitySet new .  hidden := IdentitySet new .
  curClass := self virtualClass .

  "For a class, the singleton class is the meta class"
  self isBehavior ifTrue: [
    self theMetaClass nonBridgeRubySelectorsInto: set hiddenInto: hidden protection: protInt env: envId.
    includeModules ifTrue: [ | c |
      c := curClass .
      [ c ~~ nil and: [ c ~~ Object and: [ c ~~ Object theMetaClass ] ] ] whileTrue: [
      c nonBridgeRubySelectorsInto: set hiddenInto: hidden protection: protInt env: envId .
      c := c rubySuperclass: envId  .
      ] .
    ] .
  ] .

  [ curClass ~~ nil and: [ curClass isRubyVirtual ] ] whileTrue: [
    (curClass _isIncludableInSingletons: includeModules) ifTrue:[ 
	   curClass nonBridgeRubySelectorsInto: set hiddenInto: hidden protection: protInt
	               env: envId 
	 ] .
     curClass := curClass rubySuperclass: envId .
  ] .
  ^ set 

%


set class Object
category: '*maglev-runtime'
method:
_classForRubyClassVar
  ^ self virtualClass _classForRubyClassVar

%


set class Object
category: '*maglev-runtime'
method:
_methodDefTargetClass: envId
  | cls | 
  cls := self .
  self isBehavior ifFalse:[ cls := self virtualClass ]. 
  cls isRubySingletonClass ifTrue:[ ^ cls ].
  [ cls isRubyVirtual ] whileTrue:[
     cls := cls rubySuperclass: envId
  ].
  ^ cls 

%


set class Object
category: '*maglev-runtime'
method:
_nameForMethodMissing
"A ruby primitive , part of fix for Trac 752"

 ^ self virtualClass rubyFullName: 1"__callerEnvId"

%


set class Object
category: '*maglev-runtime'
method:
_rubyBindingCtx
  "Private,  only for use in building TOPLEVEL_BINDING "
  ^ self _bindingContext: 0

%


set class Object
category: '*maglev-runtime'
method:
_rubyClassVarDefinedQ: aSymbol mref: modulRef 
  "called from generated code"
  ^ modulRef _rubyClassVarDefinedQ: aSymbol env: 1"__callerEnvId"

%


set class Object
category: '*maglev-runtime'
method:
_rubyClassVarGet: aSymbol  mref: modulRef 
  "called from generated code"
  ^ modulRef _classForRubyClassVar _rubyClassVarGet: aSymbol env: 1"__callerEnvId"

%


set class Object
category: '*maglev-runtime'
method:
_rubyClassVarGetOrNil: aSymbol  mref: modulRef 
  "called from generated code"
  ^ modulRef _rubyClassVarGetOrNil: aSymbol env: 1"__callerEnvId"

%


set class Object
category: '*maglev-runtime'
method:
_rubyInspect
  "a ruby primitive"
  | str names ts envId |
  envId := 1"__callerEnvId" .
  str := '#<' copy .
  str addAll: (self class rubyFullName: envId)  .
  ts := GsProcess _recursionGuardSet .
  (ts _addIfAbsent: self) ifFalse:[  "already in ts"
    str addAll: '...>' .  
   ^ str 
  ].
  [ | s |
    s := str .
    s addAll: ':0x'.
    s addAll: self asOop hex.
    names := self  rubyInstvarNames . 
    1 to: names size do:[:n | | nam |
      s add: $  .
      s addAll: (nam := names at: n)  ; add: $=  .
      s addAll: ((self rubyInstvarAt: nam asSymbol ) @ruby1:inspect ). 
    ].
    s add: $> . 
  ] ensure:[
    ts remove: self
  ].
  ^ str

%


set class Object
category: '*maglev-runtime'
method:
_rubyTo: anInteger

^ Range from: self to: anInteger 

%


set class Object
category: '*maglev-runtime'
method:
_rubyTo_: anInteger

^ Range from: self limit: anInteger 

%


set class Object
category: '*maglev-runtime'
method:
_singletonClass: envId 
  | cls |
  self class == Module ifTrue:[   "a  class<<self   within a  module "
    cls := self moduleMethodsModuleOrNil ifNil:[ 
       cls := self _rubyModuleIncludeSelfEnv: envId 
    ].
  ] ifFalse:[  
    self isBehavior ifTrue:[ 
       cls := self virtualClass . "extending metaclass"
    ] ifFalse:[ cls := self _singletonClassFor: envId ].
  ].
  ^ cls 

%


set class Object
category: '*maglev-runtime'
method:
_singletonClassFor: envId
   | cls |
   (cls := self virtualClass)  isRubySingletonClass ifFalse:[
      self addRubySingletonClass: envId .
      cls := self virtualClass .
      cls isRubySingletonClass ifFalse:[ self error:'_singletonClassFor:, creation failed' ].
   ].
   ^ cls

%

