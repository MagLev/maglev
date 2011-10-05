
set class RubyContext class
category: '*maglev-runtime'
method:
bootWarnings
  ^ SessionTemps current at:#RUBY_BootWarnings otherwise: false

%


set class RubyContext class
category: '*maglev-runtime'
method:
bootWarnings: aBool
  SessionTemps current at:#RUBY_BootWarnings put: (aBool ifTrue:[ true] ifFalse:[false])

%


set class RubyContext class
category: '*maglev-runtime'
method:
commitTransaction
  System commitTransaction ifFalse:[ | cnfDict arr ex |
	 cnfDict := System transactionConflicts .
	 arr := { } .
	 cnfDict associationsDo:[:assoc | arr add: assoc ].
	 (ex := Error new) messageText: 'commit failed' ; args: arr .
	 ex signal .
	 ^ false 
  ].
  ^ true

%


set class RubyContext class
category: '*maglev-runtime'
method:
copyPath: anArray replacing: original with: replacement
	"Return a copy of anArray, that shares nothing (both the array and the elements are copied).
	 Convert any leading occurrence of original with replacement.  This is intended for converting
	 between $MAGLEV_HOME and its expansion for load path."
	|path originalEnd|
	originalEnd := original size + 1.
	path := anArray copy .
	1 to: path size do:[:n ||each|
		each := path at: n .
		(each at: 1 equals: original)
			ifTrue:[ each := replacement, (each copyFrom: originalEnd to: each size) ] 
			ifFalse: [ each := each copy ] .
		path at: n put: each .
	 ].
	^ path .

  

%


set class RubyContext class
category: '*maglev-runtime'
method:
createSmalltalkFFIWrappers
    | year directory classes envId |
    envId := 1 .
    RubyContext load .
    year := DateAndTime now year printString.
    directory := '$MAGLEV_HOME/lib/ruby/site_ruby/1.8/smalltalk/'.
    (GsFile isServerDirectory: directory) == true ifFalse:[ | dirOk |
        dirOk := GsFile createServerDirectory: directory .
        dirOk ifNil:[ Error signal:'failed to create directory' ].
    ].
    classes := Module _wrappedSmalltalkClasses .
    1 to: classes size by: 2 do:[:n | | eachClass stName file isRubyCls inDcSeg |
            eachClass := classes at: n .
            stName := eachClass name .
            isRubyCls := (eachClass nameSpace: envId ) ~~ nil .
            inDcSeg := eachClass objectSecurityPolicy == DataCuratorObjectSecurityPolicy .
            file := GsFile openWriteOnServer: directory , eachClass name , '.rb'.
            [  GsFile gciLogServer: '-- writing: ', file name .
        file nextPutAll:
'#--
# ' , stName, '.rb
#
# Copyright (c) ' , year , ' GemStone Systems, Inc.  All rights reserved.
#
#++

# = Overview
#
# This file defines the methods available in the Smalltalk class ' , stName , '.
# It is not intended for general use in applications, but for use in developing tools
# and persistence libraries to be used by applications.
'       ; cr .

        inDcSeg ifFalse:[
          file nextPutAll: 
'# ' , stName , ' should not be extended in Ruby (it is in SystemObjectSecurityPolicy) ' ; cr; cr.
        ].
        eachClass allSuperClassesDo: [:eachSuperClass|
            file nextPutAll: 'require ''smalltalk/', eachSuperClass name, '''' ; cr
        ] .
        file cr .

        file nextPutAll:'module Smalltalk' ; cr ;
             nextPutAll:'  ', stName , ' = __resolve_smalltalk_global(:' , stName , ')' ; cr .

         isRubyCls  ifTrue:[ | rName |
            file nextPutAll:'end' ; cr ;
                 nextPutAll:'class ' , (rName := eachClass rubyFullName: envId )  ; cr ;
                 nextPutAll:'    # ', rName , ' is identically Smalltalk::', stName ; cr .
        ] ifFalse:[
            file nextPutAll:'  class ' , stName ; cr .
        ].
        eachClass class selectors asSortedCollection do: [:eachSelector |
            | string |
            string := eachSelector copyReplaceChar: $: with: $_  .
            file
               nextPutAll: '    class_primitive_nobridge ''_st_';
               nextPutAll: string;
               nextPutAll: ''', ''';
               nextPutAll: eachSelector;
               nextPutAll: '''';
               cr.
        ].
        eachClass selectors asSortedCollection do: [:eachSelector |
            | string |
            string := eachSelector copyReplaceChar: $: with: $_  .
            file
              nextPutAll: '    primitive_nobridge ''_st_';
              nextPutAll: string;
              nextPutAll: ''', ''';
              nextPutAll: eachSelector;
              nextPutAll: '''';
              cr.
        ].
        isRubyCls ifFalse:[ file nextPutAll:'  end' ; cr ].
        file nextPutAll: 'end'; cr .
    ] ensure: [
        file close.
    ].
].

%


set class RubyContext class
category: '*maglev-runtime'
method:
default
  | val  |
  val := SessionTemps current at: #DefaultRubyContext otherwise: nil .
  val ifNil:[ 
     self error:'RubyContext default   used prior to   RubyContext load'.
  ].
  ^ val

%


set class RubyContext class
category: '*maglev-runtime'
method:
defaultOrNil
  ^ SessionTemps current at: #DefaultRubyContext otherwise: nil .

%


set class RubyContext class
category: '*maglev-runtime'
method:
ensurePrimsLoaded
	"Check if there is already a ruby context with prims loaded installed,
	 and if not, install one and commit the transaction."
	Saved ifNil: [ RubyContext load ] .

%


set class RubyContext class
category: '*maglev-runtime'
method:
installTopBinding: aBinding
  "called from generated code,  returns the argument" 
  (Object transientNameSpaceForStore: 1"__callerEnvId") 
       at: #TOPLEVEL_BINDING transientRuntimePut: aBinding .
  ^ aBinding

%


set class RubyContext class
category: '*maglev-runtime'
method:
load 
  ^ self load: #() 

%


set class RubyContext class
category: '*maglev-runtime'
method:
load: optionsArray
  | ctx |
  ctx := self load: optionsArray env: 1 .
  ^ ctx

%


set class RubyContext class
category: '*maglev-runtime'
method:
load: optionsArray  env: envId
  "Returns the current default context after initializing or loading it."
  | ctx sessTmps |
  ctx := (sessTmps := SessionTemps current) at: #DefaultRubyContext  otherwise: nil.
  ctx ifNil:[
    (ctx := Saved) ifNil:[
       Boolean useRubyClasses: true bootStrap: true .
       RubyCompiler initializeParser . 
       ctx := self new .
       "Saved := ctx . " "save early for debugging bootstrap load only"
       sessTmps installRubyContext: ctx .
       ctx initTopNameSpace: envId reload: false .
       ctx setMagLevOptions: optionsArray ;
         requirePrimitives: envId .
       Saved := ctx .
       self commitTransaction .
       ctx initTransient: envId .
    ] ifNotNil:[
       Boolean useRubyClasses: true bootStrap: false .
       sessTmps installRubyContext: ctx .
       ctx setMagLevOptions: optionsArray .
       RubyCompiler initializeParser .
       ctx initTransient: envId .
    ].
  ] ifNotNil:[
    Boolean useRubyClasses: true bootStrap: false .
  ].
  (true virtualClass == TrueClass and:[ false virtualClass == FalseClass]) ifFalse:[
    self error:'Boolean(C)>>useRubyClasses: failed failed for TrueClass/FalseClass'
  ].
  (true class == Boolean and:[ false class == Boolean]) ifFalse:[
    self error:'Boolean(C)>>useRubyClasses: failed for Boolean'
  ].

  System signalAlmostOutOfMemoryThreshold: 110 . "110% is within about 10% of OOM"
  "deleted ctx initTransient: 2 for env 2 "

  ^ ctx

%


set class RubyContext class
category: '*maglev-runtime'
method:
loadFileNamed: aString env: envId
  "used in smalltalk scripts run from topaz"
   ^ self default loadFileNamed: aString env: envId

%


set class RubyContext class
category: '*maglev-runtime'
method:
nextConfFileLine: aFile
  "Return next line not beginning with #, or nil if eof,
   contents of line after # are excluded.  "
   | line |
   [ true ] whileTrue:[
     line := aFile nextLine .
     line ifNil:[ ^ nil ].
     line := line trimWhiteSpace . 
     line size > 0 ifTrue:[
       (line at: 1) ~~ $# ifTrue:[
	      ^ (line copyUpTo: $# ) trimWhiteSpace
	   ].
     ].
   ].

  

%


set class RubyContext class
category: '*maglev-runtime'
method:
persistableInstances
  "true means  newly created classes have non-peristable_instances_bit == 0"
  ^ RubyCompilerState current persistableInstances

%


set class RubyContext class
category: '*maglev-runtime'
method:
persistableInstances: aBoolean
  "true means  newly created classes have non-peristable_instances_bit == 0"
  RubyCompilerState current persistableInstances: aBoolean .
  ^ aBoolean 

%


set class RubyContext class
category: '*maglev-runtime'
method:
persistenceMode
  "true means  inside of a Maglev.persistent  block"
  ^ RubyCompilerState current persistenceMode

%


set class RubyContext class
category: '*maglev-runtime'
method:
persistenceMode: aBoolean
  "a Ruby primitive. 
   true means  inside of a Maglev.persistent  block.  
   Returns the previous state of the persistenceMode .  "
  | prev envId |
  prev := RubyCompilerState current persistenceMode: aBoolean .
  (prev == true  and:[ aBoolean ~~ true ]) ifTrue:[ | path |
     envId := 1"__callerEnvId" .
     path := RubyContext transientLoadPathCopy: envId .
    (Object persistentNameSpace: envId) rubyGlobalVar: #'$:' put: path . 
  ].

%


set class RubyContext class
category: '*maglev-runtime'
method:
reload: optionsArray  env: envId
  | ctx sessTmps |
  "resets to empty the persistent env 1 method dicts of 
   all classes for which bootstrap code is loaded.
   
   Does not change persistent LOADED_FEATURES "
  ctx := (sessTmps := SessionTemps current) at: #DefaultRubyContext  otherwise: nil.
  ctx ifNil:[
    (ctx := Saved) ifNil:[ Error signal:'primitives not yet loaded'].
    Boolean useRubyClasses: true bootStrap: true .
    sessTmps installRubyContext: ctx .
    ctx deleteMethods: envId ;
       initTopNameSpace: envId reload: true ;
       setMagLevOptions: optionsArray ; 
       reloadPrimitives: envId . 
    self commitTransaction .
    Boolean useRubyClasses: true bootStrap: false .
    ctx initTransient: envId .
  ] ifNotNil:[
    Error signal:'duplicate reload:env:'
  ].

%


set class RubyContext class
category: '*maglev-runtime'
method:
reset
  "resets environment 1"
  | found |
  found := Saved ~~ nil .
  " self reset: 2   with Melbourne parser, NO LONGER USED"
  self reset: 1 .
  Saved := nil .
  RubyCompiler parser: nil .
  SessionTemps current removeKey: #DefaultRubyContext ifAbsent: [] .
  ^ found

%


set class RubyContext class
category: '*maglev-runtime'
method:
reset: envId
  "To be used only to force subsequent reload of ruby bootstrap code.
   Returns true if a previous committed context was cleared"
  | prev found | 
  found := false .
  (prev := Saved) ifNotNil:[ 
    prev rubyContextReset: envId . "clear global name dicts in classes"
    found := true .
  ].
  ^ found

%


set class RubyContext class
category: '*maglev-runtime'
method:
runFileNamed: aString env: envId
  "used in smalltalk scripts run from topaz"
   ^ self default runFileNamed: aString env: envId

%


set class RubyContext class
category: '*maglev-runtime'
method:
transientLoadPathCopy: envId
	"Copy the transient load path to the persistent load path, ensuring that the array
	 and all elements are copies (nothing shared).  Convert any leading prefix equal to
	 $MAGLEV_HOME to '$MAGLEV_HOME."
	|path mHome|
	mHome := RubyEnv _getenv: 'MAGLEV_HOME' .
	path := (Object transientNameSpace: envId) rubyGlobalVar: #'$:' .
	^ RubyContext copyPath: path replacing: mHome with: '$MAGLEV_HOME' .

%


set class RubyContext class
category: '*maglev-runtime'
method:
_clearPersistentLoadedFeatures
  "a ruby primitive"
   self default _clearPersistentLoadedFeatures: 1"__callerEnvId"    

%


set class RubyContext class
category: '*maglev-runtime'
method:
_loadMspec
  ^ self _loadMspec: #() env: 0

%


set class RubyContext class
category: '*maglev-runtime'
method:
_loadMspec: options
  ^ self _loadMspec: options env: 0

%


set class RubyContext class
category: '*maglev-runtime'
method:
_loadMspec: options env: envId
  | ctx |
    ctx := RubyContext load: options . "traceLoad logSexp"
    ctx env: envId persistentDo:[
      ctx _loadMspec: envId .
     (Object transientNameSpace: envId) at: #DEBUG_SPEC runtimePut:  true .
     System commitTransaction ifFalse:[ nil error:'commit fail' ].
    ].
    ^ ctx .

%


set class RubyContext class
category: '*maglev-runtime'
method:
_runBenchmarks: logPath  repeat: repeatCount timeout: timeOut 
   "logPath is path to directory where stdout and report file for each benchmark will be written,
     timeOut is in seconds "
| saveDir skips saveStdout |
self error:'OBSOLETE, needs envId rework'.
saveDir := RubyDirectory _getwd .
[ | hm bmNum lf confFile status  ctx |
  hm := RubyEnv _getenv:'MAGLEV_HOME' .
  status := RubyDirectory _chdir: hm .
  status == 0 ifFalse:[ nil error:' chdir failed' ].
  ctx := RubyContext load .
  ctx persistentDo:[
    RubyContext requireFileNamed: 'benchmark/utils/bench.rb' env: 1 ;
         commitTransaction.
  ].
  bmNum := 1 .
  lf := Character lf .
  confFile := GsFile openReadOnServer: '$MAGLEV_HOME/benchmark/benchmark.conf' .
  saveStdout := Object transientNameSpace at: #STDOUT .
  [ true ] whileTrue:[
    | aLine fullPath bmResult stdoutFileName reportFileName |
    aLine := self nextConfFileLine: confFile  .
    aLine ifNil:[      ^ bmNum "hit eof" ].
    aLine := aLine trimWhiteSpace .
    aLine last == lf ifTrue:[ aLine size: (aLine size - 1) ].
    fullPath := hm , '/benchmark/' , aLine .
    GsFile gciLogServer:'--- start ', bmNum asString , ' :' , aLine .
    ProcessorScheduler _allProcessesTerminate9 . "terminate leftover GsProcesses "
    ctx abortResetTransient .
    stdoutFileName := logPath , 'benchStdout', bmNum asString , '.out' .
    reportFileName := logPath , 'bmreport', bmNum asString,'.txt' .
    GsFile redirectRubyStdout: ( GsFile openWriteOnServer: stdoutFileName ).
    bmResult := RubyContext runRuby: fullPath
        with: repeatCount asString , ' ' , timeOut asString , ' '  , reportFileName .
    GsFile redirectRubyStdout: saveStdout .
    GsFile gciLogServer:'--- end ' , aLine .
    bmNum := bmNum + 1 .
  ]
] ensure:[
  RubyDirectory _chdir: saveDir .
  saveStdout ifNotNil:[ GsFile redirectRubyStdout: saveStdout ] .
].

%


set class RubyContext class
category: '*maglev-runtime'
method:
_runPassingSpecs
  ^ self _runPassingSpecs: 'passingspecs.conf' verbose: false debugErrors: true 
        options: #() env: 1 

%


set class RubyContext class
category: '*maglev-runtime'
method:
_runPassingSpecs: confFileName verbose: verboseBool debugErrors: debugBool options: optsArr env: envId
  "for use from a topaz -l process, to run all passing specs,    stopping to debug any error.
   Returns the number of specs run "
| count ctx lf confFile saveDir |
saveDir := RubyDirectory _getwd .
[
  ctx := RubyContext _loadMspec: optsArr env: envId  .
  (RubyEnv _getenv:'USER') = 'otisa' ifTrue:[ | tmpDir status |
     tmpDir := (RubyEnv _getenv: 'GEMSTONE') , '/data/tmp' .
     (GsFile isServerDirectory: tmpDir) == true ifFalse:[
       status := RubyDirectory _mkdir: tmpDir permissions: 8r740 .
       status == 0 ifFalse:[ self error:' cannot create' , tmpDir ].
     ].
          "ruby_selector_suffix dependent"
     (RubyEnv _current) with: 'RUBYSPECTMP' with: tmpDir perform:#'[]=#2__' env:1
  ].
  count := 1 .
  lf := Character lf .
  confFile := GsFile openReadOnServer: '$MAGLEV_HOME/../svn/tests/rubytst/', confFileName .
  [ true ] whileTrue:[
    | aLine fullPath result pcu |
    aLine := self nextConfFileLine: confFile .
    aLine == nil ifTrue:[
       ^ count "DONE, hit eof"
    ].
    pcu := System _tempObjSpacePercentUsed .
    pcu > 80 ifTrue:[
      GsFile gciLogServer:'   ', pcu asString, '% temporary object memory used'
    ].
    GsFile gciLogServer:'--- start ', count asString , ' :' , aLine .
    ProcessorScheduler _allProcessesTerminate9 . "terminate leftover GsProcesses "
    ctx abortResetTransient .
    (Object transientNameSpaceForStore: envId) at: #DEBUG_SPEC runtimePut: debugBool  ;
                 at: #DEBUG_SPEC_VERBOSE runtimePut: verboseBool . 
    [  result := ctx _runSpec: aLine env: envId .
       GsFile gciLogServer:' <done>' .
    ] onException: AbstractException do:[:ex |
      GsFile gciLogServer:' <failed with error, ' , ex asString , '>' .
      (debugBool or:[ count == 1]) ifTrue:[ nil pause ].
    ].
    count := count + 1 .
  ].
] ensure:[
  RubyDirectory _chdir: saveDir
].

%


set class RubyContext class
category: '*maglev-runtime'
method:
_runPassingSpecs: confFileName verbose: verboseBool options: optsArr env: envId
  ^ self _runPassingSpecs: confFileName verbose: verboseBool debugErrors: true 
            options: optsArr env: envId

%


set class RubyContext class
category: '*maglev-runtime'
method:
_runVmUnit 
"Run the vmunit tests from a topaz -l , assumming RubyContext load already done.
 uses currently configured MRI parser host and port ."

  ^ self _runVmUnit: nil options: #() 

%


set class RubyContext class
category: '*maglev-runtime'
method:
_runVmUnit: parserHost options: optsArray
  ^ self _runVmUnit: parserHost options: optsArray  env: 1

%


set class RubyContext class
category: '*maglev-runtime'
method:
_runVmUnit: parserHost options: optsArray  env: envId
"Run the vmunit tests from a topaz -l , assumming RubyContext load already done.
skips the tests listed in  skips array   below ."

| saveDir ctx skips saveStdout |
skips := { } .
ctx := RubyContext load: optsArray env: envId .
saveDir := RubyDirectory _getwd .
GsFile gciLogServer:'--- pwd= ', RubyDirectory _getwd .
[
  | hm count lf confFile logF status verbos |
  verbos := ( SessionTemps current at: #MagRpDEBUG otherwise: 0) > 1 .
  hm := RubyEnv _getenv:'MAGLEV_HOME' .
  status := RubyDirectory _chdir: hm .
  status == 0 ifFalse:[ self error:' chdir failed' ].
  count := 1 .
  lf := Character lf .
  confFile := GsFile openReadOnServer: hm , '/src/test/vmunit.conf' .
  logF := GsFile openWriteOnServer: saveDir , '/vmunit.log' .
  saveStdout := (Object transientNameSpace: envId) at: #STDOUT .
  [ true ] whileTrue:[
    | aLine fullPath bmResult |
    [ | skip |
      aLine := self nextConfFileLine: confFile .
      aLine == nil ifTrue:[
      logF close .
      ^ count "hit eof"
      ].
      skip := ( skips detect:[:ea| aLine at:1 equals: ea ] ifNone:[ nil ]) ~~ nil .
      skip ifTrue:[  GsFile gciLogServer:'--- SKIPPING ' , aLine ].
      skip. 
    ] whileTrue .
    aLine last == lf ifTrue:[ aLine size: (aLine size - 1) ].
    fullPath := 'src/test/' , aLine .
    ProcessorScheduler _allProcessesTerminate9 . "terminate leftover GsProcesses "
    ctx abortResetTransient .
  " parserHost ifNotNil:[ | port |
      port := RubyCompiler parser port .
      RubyCompiler parser: (RubyParseTreeClient host: parserHost port: port ) .
    ].
  "
    GsFile gciLogServer:'--- begin ' , aLine .
    verbos ifFalse:[ GsFile redirectRubyStdout: logF env: envId ].
    bmResult :=  ctx runFileNamed: fullPath env: envId .
    verbos ifFalse:[ GsFile redirectRubyStdout: saveStdout env: envId ].
    count := count + 1 .
  ].
] ensure:[
  saveStdout ifNotNil:[ GsFile redirectRubyStdout: saveStdout env: envId ].
  RubyDirectory _chdir: saveDir
].

%


set class RubyContext class
category: '*maglev-runtime'
method:
_saved
  ^ Saved

%


set class RubyContext
category: '*maglev-runtime'
method:
abortResetTransient
  "Aborts transaction.  Resets transient state for env 1"
  | saveTrVals envId |
  envId := 1 .
  saveTrVals := self transientAssocsForReinit: envId .
  Module clearVmTransientState: envId . "clears all classes loaded in VM "
  Object _clearLookupCaches: envId ."invalidate all method lookup and send-site caches"
  System beginTransaction .  

  SessionTemps current removeKey: #RubyMainSelf ifAbsent:[] .
  RubyCompilerState clearTransientState .
  self reInitTransient: saveTrVals env: envId .

%


set class RubyContext
category: '*maglev-runtime'
method:
addPrePrimMethodsTo: envId
  "add to specified env,
   methods defined in env 1 in file pre_prim_methods.gs  "

{ { Object . 	#singleton_method_added: } . 
   { Behavior . #method_added:  } . 
   { UndefinedObject . 	#__rubyOr:  } . 
   { Boolean . 	#class  } . 
   { Boolean . 	#__rubyAnd:  } . 
   { Boolean . 	#__rubyXor:  } . 
   { Boolean . 	#__rubyOr: } } do:[ :pair | | meth cls sel |
      cls := pair at: 1 .
      sel := pair at: 2 .
      meth :=  cls compiledMethodAt: sel environmentId: 1 .
      cls addRubySelector: sel method: meth env: envId .  
    ]

%


set class RubyContext
category: 'as yet unclassified'
method:
commitTransaction
  ^ self class commitTransaction

%


set class RubyContext
category: '*maglev-runtime'
method:
copyRubyCHeaderFiles
 | dest src srcF destF |
 dest := '$MAGLEV_HOME/lib/ruby/1.8/include/ruby.h' .
 src :=  '$MAGLEV_HOME/../svn/src/ruby.h' .
 (srcF := GsFile openReadOnServer: src ) ifNotNil:[
   "we have a private build with svn checkout"
   GsFile removeServerFile: dest . "ignore errors"
   (destF := GsFile openWriteOnServer: dest) ifNil:[ self error:'create of ruby.h failed'].
   destF nextPutAll:'// Generated file, do not edit. master copy in svn
'.
   (destF nextPutAll: srcF contents ) ifNil:[ self error:'write to ruby.h failed'].
   (destF close ) ifNil:[ self error:'close of ruby.h failed.'].
   srcF close .
 ].

%


set class RubyContext
category: '*maglev-runtime'
method:
defaultLoadPath
 | stdPath maglevHome |
  stdPath := { }  .
  maglevHome := System gemEnvironmentVariable: 'MAGLEV_HOME' .
  ^ maglevHome ifNotNil:[
     { (maglevHome, '/lib/ruby/site_ruby/1.8' ) . 
        (maglevHome, '/lib/ruby/site_ruby') . 
        (maglevHome, '/lib/ruby/1.8/') . 
         '.'  }
   ] ifNil:[ { } ].

%


set class RubyContext
category: '*maglev-runtime'
method:
deleteMethods: envId
  "used when forcing reloading of Ruby bootstrap code"
  (modifiedStClasses atOrNil: envId ) ifNotNil:[ :set |
    set do:[:aCls |
      aCls ~~ Object ifTrue:[
        aCls deleteMethods: envId .
        aCls virtualClass deleteMethods: envId .
      ]
    ].
  ].
  (rubyPrimMethods at: envId) removeAll 

%


set class RubyContext
category: '*maglev-runtime'
method:
deleteSmalltalkWrapperFiles
  | dir names  |
  dir := '$MAGLEV_HOME/lib/ruby/site_ruby/1.8/smalltalk' .
  (GsFile isServerDirectory: dir) == true ifTrue:[
    names := GsFile contentsOfDirectory: dir onClient: false .
    names do:[:fn | 
	  (GsFile isServerDirectory: fn ) == false ifTrue:[  "skip . , ..  entries" 
	    (GsFile removeServerFile: fn ) ifNil:[
	       Error signal: 'failed to remove ...',  (fn last: 30) , GsFile lastErrorString 
	    ].
      ].
    ].
    (GsFile removeServerDirectory: dir ) ifNil:[
      Error signal: 'failed to remove .../smalltalk ' , GsFile lastErrorString
    ].
  ].

%


set class RubyContext
category: '*maglev-runtime'
method:
env: envId persistentDo: aBlock 
  "returns result of the one arg block aBlock"
| cst savePm |
cst := RubyCompilerState initialize: envId .
savePm := cst persistenceMode .
^ [ cst persistenceMode: true .
    aBlock value 
  ] ensure:[
    cst persistenceMode: savePm .
  ]

%


set class RubyContext
category: '*maglev-runtime'
method:
evalDashEStrings: anArray
   |tns oldFile |
   oldFile := (tns := Object transientNameSpaceForStore: 1) at: #'__FILE__' otherwise: nil .
   [ |evalString comp|
       tns at: #'__FILE__' compilePut: '-e' .

       evalString := anArray inject: '' into: [:acc :el | acc, Character lf, el ] .
       RubyCompilerState initialize .
       comp := RubyCompiler new .
       comp evaluateString: evalString with: { nil . nil } withSelf: Object 
             binding: nil fileName: '-e' lineNumber: 1  env:1 .
       (SessionTemps current at: #MAGLEV_commitFlag otherwise: false) ifTrue: [ 
           self commitTransaction 
       ] .
   ] onException: Error do: [:e|
     tns at: #'__FILE__' compilePut: oldFile .
     e outer
   ] .
   tns at: #'__FILE__' compilePut: oldFile .
   ^ self .

%


set class RubyContext
category: '*maglev-runtime'
method:
fileNamed: aString env: envId
  ^ RubyFile filename: aString env: envId

%


set class RubyContext
category: '*maglev-runtime'
method:
fileNamed: aString env: envId do: aBlock
    "changed to store currentFile in a session temp, not in instVar,
     to avoid write-write conflict on receiver."
    ^ self withFile: (self fileNamed: aString env: envId) do: aBlock .

%


set class RubyContext
category: 'as yet unclassified'
method:
initModifiedStClasses: envId 
  | arr set dict |
  (arr := modifiedStClasses ) ifNil:[
	 arr := Array new: envId .
	 modifiedStClasses := arr .
  ] ifNotNil:[
     arr size < envId ifTrue:[ arr size: envId ].
  ].
  (set := IdentitySet new ) add: Object .
  arr at: envId put: set .
  "----------------"
  (arr := rubyPrimMethods) ifNil:[
	  arr := Array new: envId .
	  rubyPrimMethods := arr .
  ] ifNotNil:[
     arr size < envId ifTrue:[ arr size: envId ].
  ].
  dict := IdentityKeyValueDictionary new .  "keys are GsNMethods, values are Symbols or Array of Symbols"
  arr at: envId put: dict  .

%


set class RubyContext
category: '*maglev-runtime'
method:
initTopNameSpace: envId reload: reloadBool
  "used for bootstrap load of primitives only"
  | stdPath maglevHome tns |
  reloadBool ifFalse:[ self initModifiedStClasses: envId ].

  stdPath := self defaultLoadPath . 
  maglevHome := System gemEnvironmentVariable: 'MAGLEV_HOME' .
  maglevHome ifNil:[ self error:'MAGLEV_HOME not defined' ].
  stdPath insert: { (maglevHome, '/src/') } at: 1 .

  (RubyCompilerState initialize: envId) persistenceMode: true .
  RubyGlobalVarNode initialize: envId .
  reloadBool ifTrue:[ 
    "Object deleteMethods: 1 .  DONE IN reloadprims topaz script"
    tns := Object transientNameSpaceForStore: envId .
    (tns resolveConstant: #RUBY) _valueNoAction == self ifFalse:[
       Error signal: 'Object::RUBY not identical to RubyContext '
    ].
  ] ifFalse:[ 
    tns := RubyNameSpace initTopScope: envId  .
    tns at: #RUBY runtimePut: self .
  ].
  tns rubyGlobalVar: #'$:'  put:  stdPath ;
      rubyAlias: #'$LOAD_PATH' from:  #'$:' ;
      rubyAlias: #'$-I'  from:  #'$:' .

  reloadBool ifFalse:[
    tns rubyGlobalVar: #'$"' put: { } ;
         rubyAlias: #'$LOADED_FEATURES' from: #'$"' .
    (Object persistentNameSpace: envId) 
        rubyGlobalVar: #'$"' put: { } ;
        rubyAlias: #'$LOADED_FEATURES' from: #'$"' .
  ].
  self installPrimitiveBootstrap: envId .
  ^ self

%


set class RubyContext
category: '*maglev-runtime'
method:
initTransient: envId
  "executed at session startup, when instance created for bootstrap 
     or when instance loaded from committed state"
" | thrEnv |
  thrEnv := self __threadRubyEnvId  .
  [ 
"
    | tns f |
    "self __threadRubyEnvId: envId." "So RubyEnv and RubyHash instances work"
    RubyCompilerState initialize: envId .
    tns := Object transientNameSpaceForStore: envId .
    "assume maglev-ruby script always using topaz -l ; GsFile server/client are same"
    tns  at: #STDIN  transientRuntimePut: (f := GsFile _stdinServer ) ;
         rubyGlobalVar: #'$stdin'  put: f ;
         at: #STDOUT transientRuntimePut: (f := GsFile _stdoutServer ) .
    (tns rubyGlobalVar: #'$stdout' put: f ) "for 1.9 only  setReadOnly"  .
    tns  rubyGlobalVar: #'$>'      put: f ;
         at: #STDERR transientRuntimePut: (f := GsFile _stderrServer );
         rubyGlobalVar: #'$stderr' put: f .
    (tns rubyGlobalVar: #'$$'      put: System _gemProcessId ) setReadOnly .
    (tns rubyGlobalVar: #'$SAFE' put:  0 ) setReadOnly .
   (tns associationAt:#'$SAFE') immediateInvariant .   
"above list should match code in transientAssocsForReinit"
     self _initTransient: envId .
" ] ensure:[
    self __threadRubyEnvId: thrEnv
  ]
"

%


set class RubyContext
category: '*maglev-runtime'
method:
installConstant: aName name: stSymbol
  "a Ruby primitive used in bootstrap"
  | stAssoc aKey val envId assoc pns |
  envId := 1"__callerEnvId" .
  stAssoc := System myUserProfile resolveSymbol: ( stSymbol asSymbol) .
  stAssoc ifNil:[ self error:'Smalltalk global ' , stSymbol , ' not found' ].
  aKey := aName asSymbol .
  (aKey at:1) == $$ ifTrue:[ 
      self error: 'installConstant: not to be used for a global variable' 
  ].
  val := stAssoc _value .
  pns := Object persistentNameSpace: envId .
  (assoc := pns resolveConstant: aKey) ifNotNil:[
    assoc _valueNoAction == val ifFalse:[
      Error signal:'Object::', aKey , ' already exists with different identity'
    ]
  ] ifNil:[
    pns at: aKey runtimePutInvariant:  val .
  ].
  RubyNameSpace traceGlobals >= 1 ifTrue:[
    GsFile gciLogServer: 'installGlobal: ', aName , ' : ' , stSymbol .
  ].

%


set class RubyContext
category: '*maglev-runtime'
method:
installPrimitiveBootstrap: envId
      "ruby_selector_suffix dependent"
   { { Behavior . #primitive      . #installPrimitive:selector: }.
     { Behavior . #primitive_nobridge .  #installPrimitiveNobridge:selector:  }.
     { Behavior . #primitive      . #installPrimitive:  }.
     { Behavior . #primitive_nobridge .  #installPrimitiveNobridge:  }.
     { Behavior . #class_primitive    .   #installClassPrimitive:selector: }.
     { Behavior . #class_primitive_nobridge . #installClassPrimitiveNobridge:selector:  }.
     { Behavior . #class_primitive    .   #installClassPrimitive:  }.
     { Behavior . #class_primitive_nobridge  . #installClassPrimitiveNobridge:  }.
     { Behavior . #primitive_env      .     #installPrimitiveEnv:sel:suffix:  }.
     { Behavior . #primitive_nobridge_env . #installPrimitiveNobridgeEnv:sel:suffix:  }.
     { Behavior . #class_primitive_nobridge_env . #installClassPrimitiveNobridgeEnv:sel:suffix:  }.
     { Behavior . #method_added  . #_method_added: } .
     { Object   . #class  . #class } .
     { Object   . #singleton_method_added  . #_singleton_method_added: } .
   } do:[ :elem | | cls prefix stSel desc rubySel |
       cls := elem at: 1 . prefix := elem at: 2 .  stSel := elem at: 3 .
       rubySel := prefix _asSymbolWithRubySuffix: (stSel numArgs bitShift: 2) .
       cls addRubySelector: rubySel  env: envId 
          smalltalkMethod: (cls compiledMethodAt: stSel)
    ]. 

%


set class RubyContext
category: '*maglev-runtime'
method:
loadBootFileNamed: aString env: envId
  "load a file persistently, in bootstrap mode, 
   which disallows dynamic instVars 
   and does constant evaluation at compile time, if possible "
  | cst savePm savePath |
  cst := RubyCompilerState initialize: envId .
  savePm := cst persistenceMode .
  ^ [ cst installingPrims: true ; persistenceMode: true .
      self _addLibs: (RubyEnv _getenv:'MAGLEV_HOME') , '/src' env: envId .
      self loadFileNamed: aString env: envId
    ] ensure:[
      cst installingPrims: false ; persistenceMode: savePm .
      RubyFile loadPath: envId put: self defaultLoadPath .
    ]

%


set class RubyContext
category: '*maglev-runtime'
method:
loadFileNamed: aString
  "a ruby primitive"
     ^ self loadFileNamed: aString env: 1"__callerEnvId" 

%


set class RubyContext
category: '*maglev-runtime'
method:
loadFileNamed: aString env: envId
    | ary file |
    ary := RubyFile findRubyFileFor: aString isRequire: false env: envId .
    file := ary at: 1 . 
    (file == nil or:[ file exists not]) ifTrue:[
        RubyLoadError signal: 'no such file to load -- ', aString 
    ] .
    ^ file loadIntoEnv: envId . 

%


set class RubyContext
category: '*maglev-runtime'
method:
loadPersistentFile: aString env: envId
  "load a file persistently, but without bootstrap flags"
  | cst savePm savePath |
  cst := RubyCompilerState initialize: envId .
  savePm := cst persistenceMode .
  savePath := (RubyFile loadPath: envId) copy .
^ [ cst persistenceMode: true .
    self _addLibs: (RubyEnv _getenv:'MAGLEV_HOME') , '/src' env: envId . 
    self loadFileNamed: aString env: envId
  ] ensure:[
    cst persistenceMode: savePm .
    RubyFile loadPath: envId put: savePath .
  ]

%


set class RubyContext
category: '*maglev-runtime'
method:
persistentLoadedFeatures: envId

  ^ (Object persistentNameSpace: envId) rubyGlobalVar: #'$"' 

%


set class RubyContext
category: '*maglev-runtime'
method:
persistentLoadPathCopy: envId
  | path mHome|
  "Copy the persistent version of the ruby load path to the transient state.
   Expand $MAGLEV_HOME for each element of the path.  Ensure both the array
   and the contents of the array are copied so nothing shared between transient
   and persistent copies."
  mHome := RubyEnv _getenv:'MAGLEV_HOME' .
  path := (Object persistentNameSpace: envId) rubyGlobalVar: #'$:' .
  ^ RubyContext copyPath: path replacing: '$MAGLEV_HOME' with: mHome .

%


set class RubyContext
category: '*maglev-runtime'
method:
reInitTransient: anArray env: envId
  | tns |
  RubyCompilerState initialize: envId .
  tns := Object transientNameSpaceForStore: envId .
  1 to: anArray size do:[ :n |  
	  tns addTransientAssociation: (anArray at: n)
  ].

  self _initTransient: envId .

%


set class RubyContext
category: '*maglev-runtime'
method:
reloadPrimitives: envId
  ^ self  _requirePrimitives_traceGlobals: 0 traceLocals: 0 env: envId reload: true

%


set class RubyContext
category: '*maglev-runtime'
method:
requireFileNamed: aString
  "a ruby primitive"
     ^ self  requireFileNamed: aString env: 1"__callerEnvId" 

%


set class RubyContext
category: '*maglev-runtime'
method:
requireFileNamed: aString env: envId
   "Find file named by aString (using ruby load path, and appending .rb as necessary).
    If the file has not been loaded, then load it and return true.  
    Adds the file to the transient loaded features.
    If persistenceMode==true, adds the file to the persistent loaded features.
  "
  | pm file tfeatures pfeatures fnam cst rtModuStk prevLexPath ary |
  ary := RubyFile findRubyFileFor: aString isRequire: true env: envId .
  file := ary at: 1 .
  tfeatures := self transientLoadedFeatures: envId .
  (self _isLoaded: (ary at: 2 ) with: tfeatures) ifTrue: [ 
      ^ false . "Already loaded, so skip"
  ].
  file ifNil: [ RubyLoadError signal: 'no such file to load -- ', aString ].
  cst := RubyCompilerState current .
  pm := cst persistenceMode .
  tfeatures add: (fnam := file featureName) .  "loadedFeatures is an Array"
  pm ifTrue:[ (pfeatures := self persistentLoadedFeatures:envId ) add: fnam ].
  rtModuStk := cst rtModuleStack .
  prevLexPath := rtModuStk copy .
  [
    rtModuStk size: 0 ; add: Object .
    [
      self withFile: file do: [:f|  f loadIntoEnv: envId ] .
    ] onSynchronous: AbstractException do:[:ex |
      tfeatures removeLast .
      pm ifTrue:[ pfeatures removeLast ].
      ex pass 
    ].
  ] ensure:[
    rtModuStk size: 0 ; addAll: prevLexPath . 
  ].
  ^ true .

%


set class RubyContext
category: '*maglev-runtime'
method:
requirePrimitives: envId
  ^ self  _requirePrimitives_traceGlobals: 0 traceLocals: 0 env: envId reload: false .

%


set class RubyContext
category: '*maglev-runtime'
method:
rubyContextReset: envId
  "used when forcing reloading of Ruby bootstrap code"
  | set |
  set :=  modifiedStClasses atOrNil: envId  .
  set ifNotNil:[ 
     set do:[:aCls |
       aCls rubyContextReset: envId .
      aCls virtualClass rubyContextReset: envId .
    ].
  ].
  modifiedStClasses at: envId  put: nil .
  rubyPrimMethods at: envId put: nil .

%


set class RubyContext
category: '*maglev-runtime'
method:
runFileNamed: aString env: envId
  | tns |
  tns := Object transientNameSpaceForStore: envId .
  aString ifNotNil:[ tns rubyGlobalVar: #'$0' put: aString ].
  ^ self loadFileNamed: aString env: envId .

%


set class RubyContext
category: '*maglev-runtime'
method:
runFileNamed: aName withRubyArgs: rubyArgs withScriptArgs: scriptArgs
	^ self runFileNamed: aName 
		withRubyArgs: rubyArgs
		withScriptArgs: scriptArgs
		env: 1

%


set class RubyContext
category: '*maglev-runtime'
method:
runFileNamed: aName withRubyArgs: rubyArgs withScriptArgs: scriptArgs env: envId

  "This is called by maglev-ruby shell script to run a ruby file, aName.
   rubyArgs is an array of parameters to maglev (e.g., '-I' 'a/path' '-r')
   scriptArgs is an array of parameters passed to the script in ARGV.
   Setup the ARGV array, $0 and run the script. 
   Do NOT use loadFileNamed etc,,as they will try to find aString on the load path."
        
   | args commitFlag retValue |
   commitFlag := (SessionTemps current at: #MAGLEV_commitFlag otherwise: false) .
   args := RubyArgs forScript: aName withRubyArgs: rubyArgs withScriptArgs: scriptArgs .
   args processArgsInto: self env: envId .
        
   "Run either the file or the -e '...' scriptlets, but not both"
   retValue := args scriptlets ifEmpty:[
      (aName ~~ nil and: [ aName size ~~ 0 ]) ifTrue:[ |file|
        "Create the file, but do not look up file on $LOAD_PATH"
        file := RubyFile withGivenPath: aName fullPath: (RubyFile absolutePathFor: aName) .
        file ifNil: [ RubyLoadError signal: 'no such file to load -- ', aName ] .
        "Do NOT call loadFile or requireFile, as they look on the load path."
         retValue := self withFile: file do: [:f| f loadIntoEnv: envId ] ]
   ] ifNotEmpty: [ retValue := self evalDashEStrings: (args scriptlets) ] .
   commitFlag ifTrue: [ self commitTransaction ].
   ^ retValue .

%


set class RubyContext
category: 'as yet unclassified'
method:
selectorForPrimMethod: meth envId: envId rcvrClass: rcvrClass
  "return the ruby selector for the given smalltalk method,
     if that method is installed as a ruby primitive, otherwise return nil"
| dict list aSel siz count multClasses firstCls |
dict := rubyPrimMethods at: envId .
list := dict at: meth otherwise: nil .
list ifNil:[ ^ nil ].
(siz := list size) == 2 ifTrue:[ ^ list at: 2"the rubySelector"].
count := 0 .
firstCls := list at: 1 .
1 to: siz by: 2 do:[:n | |aCls |
  (rcvrClass _rubySubclassOf:(aCls := list at: n) env: envId) ifTrue:[
    count := count + 1 .
    aSel := list at: n + 1 .
  ].
  aCls == firstCls ifFalse:[ multClasses := true ].
].
count == 1 ifTrue:[ ^ aSel ].
multClasses ifNil:[ ^ list at: 2 "the first entry" ].
^ nil.  "cannot determine specific selector"

%


set class RubyContext
category: '*maglev-runtime'
method:
setMagLevOptions: argsArray
  "Set options to the vm passed by the maglev-ruby script.

   TODO: I suppose we could store the blocks in a dictionary, and then
   just invoke them for each option passed on the command line..."
| tmps captStack |
tmps := SessionTemps current .
captStack := true .
1 to: argsArray size do:[:n | | ea |
  ea := argsArray at: n .
  ea == #persistent ifTrue:[ tmps at: #MAGLEV_persistentFlag put: true ] ifFalse:[
  ea == #commit ifTrue:[ tmps at: #MAGLEV_commitFlag put: true ;
                              at: #MAGLEV_persistentFlag put: true ] ifFalse:[
  ea == #traceLoad ifTrue:[ tmps at: #MAGLEV_RubyFile_traceLoad put: true ] ifFalse:[
  ea == #logSexp ifTrue:[ tmps at: #MAGLEV_logSexp put: true ] ifFalse:[
  ea == #traceGlobals ifTrue:[ RubyNameSpace traceGlobals: 2 ] ifFalse:[
  ea == #noCaptureStack ifTrue:[ captStack := false ]          
  ]]]]]
] .
System gemConfigurationAt:#GemExceptionSignalCapturesStack put: captStack

%


set class RubyContext
category: 'as yet unclassified'
method:
trackRubyClass: aClass env: envId 

   ( modifiedStClasses at: envId ) add: aClass 
	

%


set class RubyContext
category: '*maglev-runtime'
method:
trackRubyPrimitive: stMeth inClass: aClass rubySel: rubySel env: envId  
  "add to the rubyPrimMethods dictionary for this envId ,
   for use during code profiling."

  | dict list |
  dict := rubyPrimMethods at: envId .
  list := dict at: stMeth otherwise: nil .
  list ifNil:[
	 dict at: stMeth put: { aClass . rubySel }.
  ] ifNotNil:[ 
    list add: aClass ; add: rubySel .    
  ].

%


set class RubyContext
category: '*maglev-runtime'
method:
transientAssocsForReinit: envId
  | arr tns |
  tns := Object transientNameSpaceForStore: envId .
  arr := { } .
  #( #STDIN  #'$stdin'  #STDOUT #'$stdout' #'$>' 
     #STDERR  #'$stderr'   #'$$'  #'$SAFE' 
   ) do:[ :sym |
     arr add: (tns associationAt: sym ) 
   ].
   ^ arr 

%


set class RubyContext
category: '*maglev-runtime'
method:
transientLoadedFeatures: envId

  ^ (Object transientNameSpace: envId) rubyGlobalVar: #'$"' 

%


set class RubyContext
category: '*maglev-runtime'
method:
withFile: rubyFile do: aBlock
	"Run the block with rubyFile as the current file."
	^ rubyFile  exists ifTrue: [aBlock value: rubyFile ].
    

%


set class RubyContext
category: '*maglev-runtime'
method:
_addLibs: libString env: envId 
    "Add libraries specified by libString to $:.  The libs may be a single library or
    several libraries separated by ':'."
    | newLib rns |
    newLib := libString findTokens: ':' .
    rns := Object transientNameSpace: envId .
         "GsFile gciLogServer: '-- Adding: ', libString printString, ' to $:' . "
    (rns associationAt: #'$:') globalVarValue insertAll: newLib at: 1 .

%


set class RubyContext
category: '*maglev-runtime'
method:
_clearPersistentLoadedFeatures: envId

  ^ (Object persistentNameSpace: envId) rubyGlobalVar: #'$"' put: { } 

%


set class RubyContext
category: '*maglev-runtime'
method:
_fixSymbolSuperclass: envId 

   "at end of bootstrap,
      make Symbol have Object as its superclass in Ruby "
   Symbol transientRubySuperclass: envId put: Object ;
          persistentRubySuperclass: envId put: Object .

%


set class RubyContext
category: '*maglev-runtime'
method:
_initFcntlConstants: envId 
  "executed during VM startup
   to define OS dependent constants in module Fcntl "
  | arr  tns fcntl  assoc |
  assoc := (Object transientNameSpace: envId) resolveConstant: #Fcntl .
  assoc ifNotNil:[ 
	 fcntl := assoc _valueNoAction .  
    arr := RubySocket _socketOsConstants: 1 .
    tns := fcntl transientNameSpaceForStore: envId .
    1 to: arr size by: 2 do:[:k | | nam val |
      nam := (arr at: k) asSymbol .
      val := (arr at: k + 1) .
      tns at: nam transientRuntimePut: val . 
    ].
    SessionTemps current at:#RUBY_Fcntl putNoStub: fcntl "protect in-memory copy of Fcntl from GC"
  ].  "ifNil:  do nothing at start of bootstrap "

%


set class RubyContext
category: '*maglev-runtime'
method:
_initProcessConstants: envId 
  "executed during VM startup
   to define OS dependent constants in module Fcntl "
  | arr tns procCls  assoc |
  assoc := (Object transientNameSpace: envId) resolveConstant: #Process .
  assoc ifNotNil:[ 
     procCls := assoc _valueNoAction .  
    arr := RubySocket _socketOsConstants: 4 .
    tns := procCls transientNameSpaceForStore: envId .
    1 to: arr size by: 2 do:[:k | | nam val |
      nam := (arr at: k) asSymbol .
      val := (arr at: k + 1) .
      tns at: nam transientRuntimePut: val . 
    ].
    SessionTemps current at:#RUBY_Process putNoStub: procCls "protect in-memory copy of Process from GC"
  ].  "ifNil:  do nothing at start of bootstrap "

%


set class RubyContext
category: '*maglev-runtime'
method:
_initTransient: envId
  "executed at session startup, and by smalltalk main programs which reinitialize
    transient state when running vmunit or rubyspec tests."
  | tns path argv  argf assoc |
  tns := Object transientNameSpaceForStore: envId .
  (assoc := tns rubyGlobalVar: #'$"' put:  (self persistentLoadedFeatures: envId) copy ) 
      setReadOnly .
  tns _rubyAlias: #'$LOADED_FEATURES' fromAssoc: assoc .

  (assoc := tns rubyGlobalVar: #'$:' put:  (path := self persistentLoadPathCopy: envId) ) 
    setReadOnly . 
  tns _rubyAlias: #'$LOAD_PATH' fromAssoc:  assoc ;
      _rubyAlias: #'$-I'        fromAssoc:  assoc .

  tns at: #MAGLEV_MARSHAL_CLASS_CACHE transientRuntimePut: RubyIdentityHash new ;
      at: #ARGV   transientRuntimePut: ( argv := { } ) ;
      at: #ARGF   transientRuntimePut: (argf := RubyArgf with: argv) ;
      at: #ENV       transientRuntimePut: (RubyEnv _currentEnvAssoc _value) .
      "RUBY_PLATFORM, PLATFORM from transient_const_set  in post_prims/Object.rb "
  (tns rubyGlobalVar: #'$*' put: argv) setReadOnly .
  (tns rubyGlobalVar: #'$<' put: argf) setReadOnly .
  (tns rubyGlobalVar: #'$-a' put: nil) setReadOnly .  "maglev-ruby does not implement -a"
  (tns rubyGlobalVar: #'$-l' put: nil) setReadOnly .  "maglev-ruby does not implement -l"
  (tns rubyGlobalVar: #'$-p' put: nil) setReadOnly .  "maglev-ruby does not implement -p"

  tns addTransientAssociation:( RubyDynGlobalVarAssociation newWithKey: #'$FILENAME'
                                            with:[ argf @ruby1:filename ] ).
  RubyGlobalVarNode sessionInitialize .
  "MAGLEV_SESSION_TEMPS not used anymore"
  "TOPLEVEL_BINDING  installed by generated code , see  #installTopBinding:  "
  envId == 1 ifTrue:[
    RubySocket _initTransientSocketConstants: envId . 
    self _initFcntlConstants: envId .
    self _initProcessConstants: envId .
    RubyThrowException _initSignalConstants: envId .
  ] ifFalse:[
    envId == 2 ifTrue:[ "RubyParser does not use Socket or Fcntl"]
              ifFalse:[ self error:'Socket,Fcntl init needs work for per-envId key in SessionTemps'].
  ].

%


set class RubyContext
category: '*maglev-runtime'
method:
_isLoaded: givenPath with: features 
    "Searches the $LOADED_FEATURES global to see if givenPath, or
    givenPath + .rb, is on it."
    |  x |
    (features detect: [ :f| f = givenPath ] ifNone: [ nil ]) ifNotNil: [ ^ true ] .
	
    x := givenPath, '.rb' .
    (features detect: [ :f| f = givenPath ] ifNone: [ ^ false ]) ifNotNil: [ ^ true ] .
    ^ false
	

%


set class RubyContext
category: '*maglev-runtime'
method:
_loadMspec: envId
  "load the mspec.rb from submodule of Gemstone internal git repository
   for debugging a spec file from  topaz -l .
   Returns receiver. "
  | saveDir |
  saveDir := RubyDirectory _getwd .
  [  | hm saveDir status rns |
     hm := RubyEnv _getenv:'MAGLEV_HOME' .
     RubyNameSpace traceGlobals: 0 .  
     status := RubyDirectory _chdir: hm, '/spec/mspec/lib' .
     status == 0 ifFalse:[ self error:' chdir failed' ]. 
     rns := Object transientNameSpaceForStore: envId .
     rns rubyGlobalVar: #'$0' put: 'mspec.rb' .
     self requireFileNamed: 'mspec.rb' env: envId.
     
     rns at: #DEBUG_SPEC runtimePut:  false  ;  
         at: #DEBUG_SPEC_VERBOSE runtimePut: false .
  ] ensure:[
     RubyDirectory _chdir: saveDir
  ].

%


set class RubyContext
category: '*maglev-runtime'
method:
_requirePrimitives_traceGlobals: gInt traceLocals: locInt env: envId reload: reloadBool 
   | res tns cst opns hm pfeatures |
   cst := RubyCompilerState current .
   cst persistenceMode: true .
   tns := Object transientNameSpaceForStore: envId .
   tns rubyGlobalVar: #'$MaglevInBootstrap' put: true . "allow ruby code to test $MaglevInBootstrap for debug"
   reloadBool ifTrue:[
     opns := Object persistentNameSpace: envId .
     pfeatures := (opns rubyGlobalVar: #'$"') copy .
   ] ifFalse:[
     pfeatures := { } .
   ].
   [  cst installingPrims: true ; reloadingPrims: reloadBool .
      envId == 1 ifTrue:[ 
        self deleteSmalltalkWrapperFiles ; copyRubyCHeaderFiles .
      ].
      res := self fileNamed: 'kernel/kernel.rb' env: envId do:[ :file |
      [ RubyParserM initializeParser . 
        RubyNameSpace  traceGlobals: gInt .
        RubyStaticScope traceLocals: locInt . 
        self class commitTransaction . "save class var changes"
        file loadIntoEnv: envId .
        self class commitTransaction . "save prims code"  
      ] ensure:[
        RubyNameSpace  traceGlobals: 0 .
        RubyStaticScope traceLocals: 0 .
      ].
     ].
   ] ensure:[
     cst installingPrims: false 
   ].
   self requireFileNamed: 'kernel/post_prims.rb' env: envId . "will abort due to new MainSelf"
     "kernel.rb and post_prims.rb use require to load the files.  This leaves $LOADED_FEATURES
      with all of the bootstrap files listed.  Now reset LOADED_FEATURES back 
      to previous state. "
   ( tns associationAt: #'$"' ) globalVarValue: pfeatures  .
   opns ifNil:[ opns := Object persistentNameSpace: envId ].
   ( opns associationAt: #'$"' ) globalVarValue: pfeatures .
   #( #TRUE #FALSE #NIL) do:[:sym | (opns associationAt: sym) immediateInvariant ].
   self _fixSymbolSuperclass: envId .
   hm := System gemEnvironmentVariable: 'MAGLEV_HOME' .
   reloadBool ifFalse:[
     opns rubyGlobalVar: #'$:' put:  (RubyContext copyPath: (self defaultLoadPath) replacing: hm with: '$MAGLEV_HOME').
   ].
   tns rubyGlobalVar: #'$:' put:  self defaultLoadPath .  "separate copy for tns"
   tns rubyGlobalVar: #'$MaglevInBootstrap' put: nil .
   cst persistenceMode: false . 
   ^ res

%


set class RubyContext
category: '*maglev-runtime'
method:
_runSpec: pathArg env: envId
 
 "run an individual spec file from  topaz -l  .
  aPath is relative to  git/spec/rubyspec/1.8 in a Gemstone internal checkout.
  assumes RubyContext(c)>>_loadMspec already done"

  | saveDir aPath |
  envId == 1 ifFalse:[ ArgumentError signal:' invalid envId'].
  aPath := pathArg trimWhiteSpace .
  [ saveDir := RubyDirectory _getwd .
    [  | hm fullName specCls status args|
       hm := RubyEnv _getenv:'MAGLEV_HOME' .
       status := RubyDirectory _chdir: hm, '/spec/mspec/lib' .
       status == 0 ifFalse:[ self error:' chdir failed' ].
       fullName :=  hm , '/spec/rubyspec/' , aPath  . 
       specCls := Object rubyConstAt: #MSpec env: envId .  
        args := RubyArgs forScript: fullName withRubyArgs: { } withScriptArgs: { } .
        args processArgsInto: self env: envId .
      ^ specCls @ruby1:runonespec: fullName  .
   ] onException: AbstractException do:[:ex |
      self pause .
   ]
 ] ensure:[
    RubyDirectory _chdir: saveDir
 ].

%

