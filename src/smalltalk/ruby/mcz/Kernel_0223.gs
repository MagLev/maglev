
set class Kernel
category: '*maglev-runtime'
method:
atExit: aBlock
  aBlock _isExecBlock ifFalse:[ ArgumentTypeError signal:'argument must be a block'].
  RubyCompiler addExitHandler: aBlock .
  ^ aBlock

%


set class Kernel
category: '*maglev-runtime'
method:
includeRubyModule: aModule
  "a ruby primitive.
   used by  include   at  ruby main program level, not within a class or module"
  self class == Object ifFalse:[ ArgumentTypeError signal:'invalid receiver for include'].
  ^ Object addRubyVirtualSuperclass: aModule forMeta: false env: 1"__callerEnvId"  

%


set class Kernel
category: '*maglev-runtime'
method:
resolveSmalltalkGlobal: aName
  "intented for bootstrap only"
  | assoc |
  assoc := System myUserProfile resolveSymbol: (aName asSymbol) .
  assoc ifNil:[ self error:'Smalltalk global ' , aName , ' not found' ].
  ^ assoc _value

%


set class Kernel
category: '*maglev-runtime'
method:
rubyGlobalVariables
  "a ruby primitive"
  | ns arr |
  ns := Object transientNameSpace: 1"__callerEnvId"  .
  arr := { } .
  ns keysAndValuesDo:[ :aKey :aVal |  
    (aKey at: 1) ==  $$  ifTrue:[ arr add: (String withAll: aKey)].
  ].
  "  #'$?'  not currently included  , it is a runtime dynamic value"
  ^ arr 

%


set class Kernel
category: '*maglev-runtime'
method:
rubyKernelAutoload: aName file: aFile
  "a ruby primitive.
  Install an RubyAutoloadAssociation in the top level name space"
  ^ (Object nameSpace: 1"__callerEnvId" ) 
       rubyAutoload: aName asSymbol file: aFile 

%


set class Kernel
category: '*maglev-runtime'
method:
rubyKernelAutoloadFileFor:  aSymOrString
  "a ruby primitive.
  Return the name of the file registered for autoloading of aSymOrString.
  see also Module>>rubyAutoloadFileFor:"
  | assoc ns |
  ns := Object nameSpace: 1"__callerEnvId" .
  assoc := ns resolveConstant: aSymOrString asSymbol .
  assoc ifNotNil: [
    (assoc isKindOf: RubyAutoloadAssociation) ifTrue: [ ^ assoc fileName ] .
  ] .
  ^ nil .

%


set class Kernel
category: '*maglev-runtime'
method:
smalltalkUserGlobalsAt: aName put: aValue
  "intended for bootstrap only"
  | assoc |
  RubyCompilerState current installingPrims ifFalse:[
	self error:'storing into UserGlobals not allowed outside of bootstrap'
  ].
  UserGlobals at: aName put: aValue .

%


set class Kernel
category: '*maglev-runtime'
method:
traceGlobalVarAssign: aSymbol block: aBlock
  "a ruby primitive"
  | tmps dict envId |
  envId := 1"__callerEnvId" .
  (aSymbol at:1) == $$ ifFalse:[ ArgumentError signal:'name must begin with ''$'' ' ].
  tmps := SessionTemps current .
  dict := tmps at: #RUBY_traceGlobalVars otherwise: nil .
  aBlock ifNil:[  "untrace_var" | tns |
      tns := Object transientNameSpaceForStore: envId .
      (tns includesKey: aSymbol) ifFalse:[ NameError signal: 'undefined global variable ' , aSymbol ].
      dict ifNil:[ ^ { } ].
      ^ dict removeKey: aSymbol otherwise: { }  .
  ] ifNotNil:[ | trc |
     dict ifNil:[
       tmps at:#RUBY_traceGlobalVars put: ( dict := IdentityKeyValueDictionary new )
     ].
     trc := dict at: aSymbol otherwise: nil.
     trc ifNil:[  dict at: aSymbol put: ( trc := { } ) ].
     trc add: aBlock.
     Module _incrementCachedConstantsSerialNum .
     ^ nil
  ]

%


set class Kernel
category: '*maglev-runtime'
method:
waitpid: pidInt flags: flagsInt
  "Calls waitpid() .
   Updates Ruby $? and returns an Array  { pid . status },
   else returns a SmallInteger errno."
| parr pid rawStatus |
parr := System _waitpid: pidInt flags: flagsInt .
parr _isArray ifTrue:[
  pid := parr at: 1 .
  rawStatus := parr at: 2 .
  pid == pidInt ifTrue:[ | arr |
    arr := { rawStatus .  (rawStatus bitAnd: 16rFF)"childStatus" .
           (pid == pidInt)"completedBool" . nil "errMsg". 0 "errno" } .
    GsProcess _current _rubyThreadDataAt: 2 "GC_RubyGsProcessClientData_childProcStatus"
              put:  (RubyProcessStatus with: arr ) .
  ].
].
^ parr  

%


set class Kernel
category: '*maglev-runtime'
method:
_eval: aString binding: aBinding with: vcGlobalsSelf fileName: aFileName lineNumber: anInt 
    "A ruby primitive.
     Evaluate aString.  vcGlobals is an Array of size 3"
  | defStk lexSelfStk envId cld aClass |
  envId := 1"__callerEnvId" .
  cld := GsProcess _current _clientData .
  aClass := aBinding methodDefTarget .

  (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: aClass .
  cld at: 7 put: aClass " _rubyThreadDataAt: 7 put: " .

  (lexSelfStk := RubyCompilerState current lexicalSelfStack ) push: nil .
  ^ [
       RubyCompiler new evaluateString: aString binding: aBinding with: vcGlobalsSelf 
           fileName: aFileName lineNumber: anInt  env: envId
    ] ensure:[
       defStk pop: aClass . 
       lexSelfStk pop: nil .
    ].

%


set class Kernel
category: '*maglev-runtime'
method:
_forkvExec: commandStr
  "A ruby primitive.
   Result is an Array  { rawStatus . childStatus . resultString , errMsg, errno } "
| arr resultStr |
arr := System _performOnServer: commandStr .
resultStr := arr at: 3 .
resultStr ifNil:[  | rawStatus childStatus errno errMsg |
  rawStatus := arr at: 1 .
  childStatus := arr at: 2 .
  errno := arr at: 5 .
  errMsg := arr at: 4 .
  commandStr _error: #hostErrPerform args:{ errMsg . errno . rawStatus . childStatus }.
  ^ nil
].
GsProcess _current _rubyThreadDataAt: 2 "OC_RubyGsProcessClientData_childProcStatus+1"
            put:  (RubyProcessStatus with: arr ) .
^ arr

%


set class Kernel
category: '*maglev-runtime'
method:
_rubyLoop1: aBlock
  "a ruby primitive, conforming to Object>>_rubyEach1: "
    aBlock ifNil:[ CannotReturn signal:'no block given']. 
    [ 
      [
        [ true ] whileTrue:[    aBlock @ruby1:value ].     
      ] onException: RubyStopIterationError do: [:ex |   "added for 1.8.7"
        ^ self "Ruby StopIteration caught, terminate iteraton"
      ]
    ] onException: RubyBreakException do: [:ex | | args |
      args := ex gsArguments .
      (args at: 1)  ifTrue:[  "Ruby break, terminate enumeration"
        ^ args at: 2
      ] ifFalse:[
        ex retry "Ruby retry - restart the enumeration"
      ]
    ] .
    ^ self

%


set class Kernel
category: '*maglev-runtime'
method:
_rubyLoop2: aBlock
  "a ruby primitive, conforming to Object>>_rubyEach1: "
    aBlock ifNil:[ CannotReturn signal:'no block given'].
    [
      [
        [ true ] whileTrue:[    aBlock @ruby1:value ].
      ] onException: RubyStopIterationError do: [:ex |   "added for 1.8.7"
        ^ self "Ruby StopIteration caught, terminate iteraton"
      ]
    ] onException: RubyBreakException do: [:ex | | args |
      args := ex gsArguments .
      (args at: 1)  ifTrue:[  "Ruby break, terminate enumeration"
        ^ args at: 2
      ] ifFalse:[
        ex retry "Ruby retry - restart the enumeration"
      ]
    ] .
    ^ self

%


set class Kernel
category: '*maglev-runtime'
method:
_system: commandStr
  "A ruby primitive.
   Result is an Array  { rawStatus . childStatus . childCompletedBool  . 
            errMsg. errno } "
| arr done |
arr := System __system: commandStr .
GsProcess _current _rubyThreadDataAt: 2 "GC_RubyGsProcessClientData_childProcStatus"
            put:  (RubyProcessStatus with: arr ) .
^ arr

%

