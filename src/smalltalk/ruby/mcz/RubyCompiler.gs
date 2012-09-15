
set class RubyCompiler
category: 'as yet unclassified'
classmethod:
addExitHandler: aBlock
  
  (SessionTemps current at:#RubyExitHandlers) add: aBlock

%


set class RubyCompiler
category: 'as yet unclassified'
classmethod:
altParser: aParser
  "The alternative parser is a second MRI parse server which is 
    only  used to print the sexp stream from a v3 ParseTree gem
   for comparision to the v2.2.0 sexp stream during debugging, 
     if activated by  ( RubyParseTreeClient logSexp: true)   .  
   We are not using the v3.0 for any code generation yet."

	AltParser := aParser .

%


set class RubyCompiler
category: 'as yet unclassified'
classmethod:
basicLog: aString
	| f |
	self error:'should not be here'. 
	f := GsFile openAppendOnServer: '/opt/gemstone/log/ruby.log'.
	f cr; nextPutAll: aString.
	f close

%


set class RubyCompiler
category: 'as yet unclassified'
classmethod:
beSilent
	Verbose := nil

%


set class RubyCompiler
category: 'as yet unclassified'
classmethod:
beVerbose
	Verbose := true

%


set class RubyCompiler
category: 'as yet unclassified'
classmethod:
blockEvalSelector
	^ #__compileEval

%


set class RubyCompiler
category: '*maglev-runtime'
classmethod:
handleRubyThrow: anException sysExitBlock: sysExitBlk main: mainThrBool 
  | namErr sigNum throwExBlk |
  throwExBlk := [ :ex|  | resigEx | 
     mainThrBool ifTrue:[  
        (resigEx := NameError new) messageText:'no catch found for throw';
                                        name: ex name .
     ] ifFalse:[
        (resigEx := ThreadError new) messageText:'no catch found for throw ,', ex name.
     ].
     ex resignalAs: resigEx
   ].
  (sigNum := anException signalNumber) ifNotNil:[ | envId tns dict blk |
    envId := 1 "assume env 1" .
    tns := RubySignal transientNameSpaceForStore: envId .
    dict := tns at:#TrappedSignals . 
    (blk := dict at: sigNum ) ifNotNil:[ | handledOk |
      sigNum == (tns at: #SigTERM) ifTrue:[
        [ | exc |
          (Delay forMilliseconds: 5000 ) highPriorityWait . 
           GsFile gciLogServer:'SIGTERM , 5 second delay expired' .
           Error signalFatalSigTerm .
        ] forkAt: 40"inline highestPriority" with: #() env: 1 .
        ProcessorScheduler scheduler yield .
      ].
      [
        blk @ruby1:__signal_callback .
        handledOk := true
      ] onException: { RubyThrowException . RubySystemExit } 
        do: {  throwExBlk . sysExitBlk }.
      handledOk ifNotNil:[
        anException resume  "resume execution where signal was received"
      ].
      anException outer
    ] ifNil:[ 
      Break new 
         details: 'no Ruby handler for signal ', sigNum asString ;
         signalNotTrappable .
      anException resume .  "in case execution continued from debugger"
    ]
  ] ifNil:[ 
    throwExBlk value: anException 
  ].

%


set class RubyCompiler
category: '*maglev-ast'
classmethod:
initializeParser 
  RestrictedClasses ifNil:[ |set |
    (set := IdentitySet new)
      add: ExecBlock ;  add: GsNMethod ; add: VariableContext;
      add: SmallInteger . 
    RestrictedClasses := set .
    (set := IdentitySet new) 
       add: #'+' ; add: #'-' ; add: #'*' ; add:#'>=' ; add:#'<=' ; add:#'bitAnd:'; add:#'<'.
    SmallIntSpecialSelectors := set
  ].

%


set class RubyCompiler
category: 'as yet unclassified'
classmethod:
isVerbose
	^ Verbose ifNil: [false]

%


set class RubyCompiler
category: 'as yet unclassified'
classmethod:
log: aString
	Verbose ifNotNil: [self basicLog: aString]

%


set class RubyCompiler
category: 'as yet unclassified'
classmethod:
parser 
  ^ Parser

%


set class RubyCompiler
category: 'as yet unclassified'
classmethod:
parser: aParser
  
	Parser := aParser . 
	AltParser := nil .

%


set class RubyCompiler
category: 'as yet unclassified'
classmethod:
parseTreePort
	|env|
	env := System gemEnvironmentVariable: 'PARSETREE_PORT'.
	^ env ifNil: [2001] ifNotNil: [env asNumber]

%


set class RubyCompiler
category: '*maglev-runtime'
classmethod:
reimplementationAllowed: aSymbol for: aClass cst: aRubyCompilerState
      "ruby_selector_suffix dependent"
  | firstCh  |
  (RestrictedClasses includes: aClass) ifTrue:[
     aClass == SmallInteger ifTrue:[ |  sym  |
       sym := aSymbol prefixIfRubySelector  asSymbol .
       ( SmallIntSpecialSelectors includes: sym ) ifTrue:[ 
           ^ false "cannot reimplement  certain selectors like Fixnum#+   "
        ].
     ] ifFalse:[ "ExecBlock, GsNMethod or VariableContext, no extensions outside bootstrap"
       aRubyCompilerState installingPrims ifFalse:[ ^ false ].
     ].
  ].
  aSymbol size >= 7 ifTrue:[
    (firstCh := aSymbol at: 1) == $b ifTrue:[
       (aSymbol at:1 equals:'block_given?')  ifTrue:[
          aRubyCompilerState installingPrims ifFalse:[ ^ false ].
       ].
    ].
    (firstCh == $_ and:[ (aSymbol at: 2) == $_ ]) ifTrue:[
      ( ((aSymbol at: 1 equals: '__send__') and:[ aRubyCompilerState installingPrims not ] ) 
        or:[ (aSymbol last == $& and:[  aSymbol at: 1 equals: '__each#' ])
            "__each#0_& is prim handler for RubyBreakException installed in Object.rb"
        or:[ aSymbol at: 1 equals: '__perform#' ] ]
      ) ifTrue:[
         ^ false
       ].
    ].
  ].
  ^ true

%


set class RubyCompiler
category: '*maglev-runtime'
classmethod:
runAtExitBlocks: envId
  | tmps arr |
  arr := (tmps := SessionTemps current) at:#RubyExitHandlers otherwise: nil .
  arr ifNotNil:[  | sysExitBlk sz blk prevSiz count n |
    n := 0 .
    count := 0 .
    prevSiz := arr size .
    [ blk := arr _rubyDeleteAt: -1 . blk ~~ nil ] whileTrue:[
      n := n + 1 .
      [ [ [  
            blk @ruby1:value 
           ] onException:  RubyThrowException do: [ :ex|  
               self handleRubyThrow: ex sysExitBlock: sysExitBlk main: true . 
           ].
        ] onException: RubyBreakException do:[:ex |
           ex resignalAs: (CannotReturn new messageText:'break not within block nor while/for loop')
        ]
      ] onException: { RubySystemExit . AbstractException } do: 
       { 
          [:ex | ex outer  "defer to top level handlers for SystemExit " 
          ] .
          [ :ex |  | msg | 
           msg := [  'error , ' , ex asString .
           ]  onException: AbstractException do:[:exb | 
              ex return: 'error getting exception string' 
           ].
           GsFile gciLogClient:  'during at_exit handler ' , n asString , ': ' , msg . 
           (tmps at: #Maglev_ruby_debugFlag otherwise: false) ifTrue:[  
             self pause  "for debugging at_exit problems"
           ].
           ex return "return from onException:do: to try next exit handler"
          ] 
        } .
      sz := arr size .
      sz >= prevSiz ifTrue:[
        count := count + 1 .
        count > 10000 ifTrue:[ 
          Exception new details:'infinite loop in atexit processing' ;
             signalNotTrappable 
        ].
      ] ifFalse:[
        prevSiz := sz .
        count := 0 .
      ].
    ].
  ].
  ProcessorScheduler scheduler _killOtherRubyThreads

%


set class RubyCompiler
category: '*maglev-runtime'
classmethod:
withRubyHandlers: envId main: mainThrBool do: aBlock 
  | sysExitBlk |
  sysExitBlk := [ :ex|  | status nx mainThr currThr runAtExits runAtExitBlk |    
    mainThr := (SessionTemps current at:#RubyMainThread otherwise:nil).
    status := ex status  .
    runAtExits := ex runAtExitHandlers .
    runAtExitBlk := [ 
      [ RubyCompiler runAtExitBlocks: envId 
      ] on: RubySystemExit do: [ :sx |
        status := sx status . 
        sx return  "abandon further execution of at_exit blocks"
      ]
    ] .
    currThr := GsProcess _current .
    (mainThr ~~ currThr and:[ mainThr ~~ nil and:[ mainThr alive]]) ifTrue:[ | stat |
      stat := mainThr rubyStatus .
      (stat = 'sleep' or:[ stat = 'run']) ifTrue:[
        mainThr priority: Processor highestPriority . 
        (nx := RubySystemExit new) status: ex  status  ; args: { ex status } .
        mainThr signalException: nx .
        Processor yield.
      ].
      runAtExits ifTrue:[ runAtExitBlk value ].
      AbstractException signal: 'Kernel.exit invoked from non-main thread, and main thread not alive' .
    ] ifFalse:[
      runAtExits ifTrue:[ runAtExitBlk value ]
    ].
    status = 0 ifTrue:[  ex return: true].
    (nx := RubySystemExit new) status: status  ; args: { status } .
    nx signal "return error to topaz"
  ].
 ^ [ 
      [ | currThr nx res | 
        res := aBlock value .
        (SessionTemps current at:#RubyMainThread otherwise:nil) == GsProcess _current 
		ifTrue:[ (RubySystemExit new status: 0) signal ].
        res
      ] onException: { RubyThrowException . RubySystemExit } 
        do: { [ :ex|  self handleRubyThrow: ex sysExitBlock: sysExitBlk main: mainThrBool ]  . 
               sysExitBlk }.
   ] onException: AbstractException do:[:ex || stmps |
     stmps := SessionTemps current.
     (ex isKindOf: RubyBreakException) ifTrue:[
       "Note,  onException:{ exA . exB} do:{ }  form of handler 
        not recognized by signalRubyBreak bytecode."
       ex resignalAs: (CannotReturn new messageText:'break not within block nor while/for loop')
     ].
     ((stmps at:#RubyExitOnException otherwise: false) or: [(GsProcess _current threadDataAt: #RubyExitOnException) == true]) ifTrue:[ 
       (stmps  at:#Maglev_ruby_debugFlag otherwise: false) ifTrue:[
          GsFile gciLogServer:'an Exception would cause exit when abort_on_exception is true'.
          self pause .  "stop for debugging the application"
       ].
       ex resignalAs:( RubySystemExit new status: 0) .
       AbstractException signal:'should not be here after abort_on_exception activated'.
     ] ifFalse:[
       ex outer
     ]
   ]

%


set class RubyCompiler
category: '*maglev-ast'
method:
bridgeForPrimAllowed: rubySel 
      "ruby_selector_suffix dependent"
   | firstChar |
   firstChar :=  rubySel at: 1 .
   firstChar == $s ifTrue:[
     (rubySel at:1 equals:'send') ifTrue:[
      rubySel == #'send#1*&' ifFalse:[ ^ false  "no bridge for    def send ; end  " ] ].
   ].
   firstChar == $_ ifTrue:[
     (rubySel at:1 equals: '__send__')  ifTrue:[
      rubySel == #'__send__#1*&' ifFalse:[ ^ false  "no bridge for    def send ; end  " ] ].
   ].
   ^ true

%


set class RubyCompiler
category: '*maglev-runtime'
method:
compiledMethodForIR: irNode
 | cm vbs errs text | 
 cm :=  GsNMethod generateFromIR: irNode.
 (cm isKindOf: Array) ifTrue: [
    (vbs := Verbose) ifNotNil:[    self basicLog: cm printString; log: 'Compilation error' ].
    (cm at: 1) class == GsNMethod ifTrue:[
        vbs ifNotNil:[ self basicLog: 'Compile warnings' ].
           ^ cm at: 1.
    ].
    errs := cm at: 2 .
    text := String new .
    errs do:[ :descr | | errNum arg |
       text add: 'compilerError ' ; add: (errNum := descr at: 1) asString ; add: ': ' .
       (arg := descr atOrNil: 3) ifNotNil:[ text add: arg asString ; add: ', ' ].
       (arg := descr atOrNil: 4) ifNotNil:[ text add: arg asString ; add: ', ' ].
       text add: (((Globals at: #GemStoneError) at: #English) atOrNil: errNum ) asString .
     ].
    CompileError new 
       args: { errs . nil "no source". 'no category'. Object. System myUserProfile symbolList.
                'no selector' } ; 
       messageText: text ;
       signal
 ].
 ^ cm

%


set class RubyCompiler
category: '*maglev-runtime'
method:
compileEvalMethod: selectorArg inClass: aClass rubyMethod: aNode env: envId 
  ^ self compileMethod: selectorArg inClass: aClass rubyMethod: aNode env: envId 
		 	isEval: true

%


set class RubyCompiler
category: '*maglev-runtime'
method:
compileFileNamed: fullPath loadName: aName env: envId
        "The fullPath is the file to compile.  aName is the name passed to require or load.
         The parser uses aName to fill out __FILE__"
  | prevSelf |
^ [ | ast  res cst compStack defStk defCls |
    cst := RubyCompilerState initialize: envId .
    prevSelf  := GsProcess initRubyMainThread: installingPrims env: envId .
    ast := self parseFileNamed: fullPath loadName: aName .
    prevSelf ifNil:[ ast setMainProgram ].
    (compStack := cst compilerStack) push: self .
    [ | cm cld |
      cm := self compileEvalMethod: #__compileFile inClass: Object 
            rubyMethod: ast  env: envId .
      prevSelf ifNil:[
         self class withRubyHandlers: envId main: true do: [
           | topSelf |
           topSelf := SessionTemps current at: #RubyMainSelf .
           cld := GsProcess _current clientData .
           (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: (defCls := topSelf class) .
           cld at: 7 put: defCls " _rubyThreadDataAt: 7 put: " .
           res := topSelf performMethod: cm  .
        ]
      ] ifNotNil:[  "recursed to load another file"
        cld := GsProcess _current clientData .
        (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: (defCls := prevSelf class) .
        cld at: 7 put: defCls " _rubyThreadDataAt: 7 put: " .
        res := prevSelf  performMethod:  cm  .
      ].
    ] ensure:[
       defStk ifNotNil:[ defStk pop: defCls ].
       compStack pop: self .
       prevSelf ifNil:[ SessionTemps current at: #RubyMainSelf put: nil ].
    ].
    res
  ] onException: AbstractException do:[:ex |
    prevSelf ifNil:[
      [ | lf msg |
        lf := Character lf .
        (SessionTemps current at:#Maglev_ruby_debugFlag otherwise: false) ifTrue:[
           msg := 'error , ' , ex asString,  ',
             during ' , fullPath .
         ] ifFalse:[ |tns level|
           "Only print stack trace if warning level set and > 1"
           msg := nil .
           tns := Object transientNameSpaceForStore: 1 .
           level := tns rubyGlobalVar: #'$-W' .
           level ifNil: [ level := 0 ] .
           (level > 1) ifTrue: [
              (msg := ex @ruby1:inspect ) add: lf .
             (ex @ruby1:backtrace ) do:[:line | msg add: line; add: lf ].
           ] .
        ].
        msg ifNotNil: [ GsFile gciLogClient: msg ].
      ] onException: AbstractException do:[:exb |
          exb return: 'error during , ' , fullPath
      ].
    ].
    ex outer
  ].

%


set class RubyCompiler
category: '*maglev-runtime'
method:
compileIn: aClass rubyMethod: aNode
  "called from generated code"
   | cls selPrefix envId |
   cls := aClass ifNil:[ Object "for irb"  ].  
   envId := 1"__callerEnvId" . 
   selPrefix := self compileSelector: nil inClass: cls 
                rubyMethod: aNode env: envId  .
   selPrefix ifNotNil:[
     "ruby_selector_suffix dependent , use perform to bypass protection"
     aClass with: selPrefix perform: #'method_added#1__' env: 1 .
   ].
   ^ nil  "per ruby specs, a  'def'  returns nil"

%


set class RubyCompiler
category: '*maglev-runtime'
method:
compileMethod: selectorArg inClass: aClass rubyMethod: aNode env: envId isEval: evalBool
  "Result is a GsNMethod , result not added to any method dict"
    | ir cm cst compStack  aSymbol fil pos |
    "self startLog. "
    currentClass := aClass .
    currClassOrModule ifNil:[ currClassOrModule := aClass ].
    leaves := IdentityKeyValueDictionary new.
    compStack := (cst := RubyCompilerState initialize: envId) compilerStack. 
    compStack push: self .
    [ " GsFile gciLogServer:'compiling ' , aSymbol , ' in ' , aClass name  "
      ir := aNode irMethodNode: envId forClass: aClass .   
      evalBool ifTrue:[ ir setRubyEval ].
      (aSymbol := selectorArg) ifNil:[ aSymbol := aNode methodSelector ]. 
    ] ensure: [
       compStack pop: self
    ].
    ir class: currentClass .
   
    ir selector: aSymbol . "after position installed, for better error message"
    cm := self compiledMethodForIR: ir .
    ^ cm 

%


set class RubyCompiler
category: '*maglev-runtime'
method:
compileSelector: selectorArg inClass: aClass rubyMethod: aNode    env: envId 
  "Result is selector prefix of method added, or nil if remplementation disallowed"
    | ir cm cst compStack isMethDef aSymbol fil pos selPrefix prot |
    currentClass := aClass .
    currClassOrModule ifNil:[ currClassOrModule := aClass ].
    leaves := IdentityKeyValueDictionary new.
    compStack := (cst := RubyCompilerState initialize: envId) compilerStack. 
    compStack push: self .
    [ ir := aNode irMethodNode: envId forClass: aClass .
      (aSymbol := selectorArg) ifNil:[ aSymbol := aNode methodSelector ]. 
      (self reimplementationAllowed: aSymbol node: aNode cst: cst ) ifFalse:[ 
         ^ nil 
      ].
    ] ensure: [ 
       compStack pop: self
    ].
    isMethDef := aNode isMethodDefinition .
    ir class: aClass  .
    "fileName and source should already exist in aNode from walk phase"
    isMethDef ifTrue:[  ir addMethodProtection: ( prot := aClass rubyMethodProtection: envId ) ].
    ir selector: aSymbol . "after position installed, for better error message"
    cm := self compiledMethodForIR: ir .
    isMethDef ifTrue:[  
      selPrefix := self installBridgeMethodsFor: aSymbol in: aClass  
            argsDescr: aNode argsNode argsDescrInt optArgs: cm rubyOptArgsBits  
            protection: cm rubyMethodProtection primKind: 0 env: envId 
    ] ifFalse: [
     selPrefix := aSymbol rubySelectorPrefixSymbol
    ].
    aClass addRubySelector: aSymbol method: cm env: envId  .
    ( aClass allModuleMethodsEnabled: envId) ifTrue:[ aClass addModuleMethod: aSymbol env: envId ].
    needsSuperEach ifTrue:[ | rseSel rseSrc rseMth |
      envId == 1 ifFalse:[ self error:'__superEach only supported in env 1 ' ].
      rseSel := #__superEach: .
      (aClass baseCompiledMethodAt: rseSel environmentId: envId) ifNil:[
         rseSrc := (Object baseCompiledMethodAt: rseSel environmentId: 0 ) sourceString.
         "compile copied smalltalk code in env 0, install in envId method dicts" 
         rseMth := aClass _primitiveCompileMethod: rseSrc 
             symbolList: (System myUserProfile symbolList) category: nil 
            oldLitVars: nil intoMethodDict: false intoCategories: nil intoPragmas: nil environmentId: 0 .
         rseMth _isArray ifTrue:[ self error:'compilation error during __superEach ' ].
         aClass addRubySelector: rseSel method: rseMth env: envId .
      ].
    ].
    ^ selPrefix  "selector prefix returned for use in method_added calls" 

%


set class RubyCompiler
category: '*maglev-runtime'
method:
compileSingletonFor: anObject rubyMethod: aNode
  "called from generated code.
   two use cases, 1)  inside of a class;...end  we are at a def self.foo;...end
                    2)  def anObj.foo;...end "
  | cls selPrefix singletonCls  envId |
  envId := 1"__callerEnvId" .
  anObject class == Module ifTrue:[
    cls := anObject moduleMethodsModuleOrNil ifNil:[  anObject _rubyModuleIncludeSelfEnv: envId 
].
  ] ifFalse:[
    anObject isBehavior ifTrue:[
      cls := anObject virtualClass "compiling a class method"
    ] ifFalse:[
      anObject _isNumber ifTrue:[
        ArgumentTypeError signal:'singleton method not allowed on a Numeric'
      ].
      cls := anObject _singletonClassFor: envId .
    ].
  ].
  singletonCls := cls .
  selPrefix := self compileSelector: nil inClass: cls rubyMethod: aNode
                      env: envId .
  selPrefix ifNotNil:[
    installingPrims ifFalse:[
     "ruby_selector_suffix dependent , use perform to bypass protection"
      singletonCls ifNotNil:[
         anObject with: selPrefix perform: #'singleton_method_added#1__' env: 1 .
      ] ifNil:[
         cls with: selPrefix perform: #'method_added#1__' env: 1 .
      ].
    ].
  ].

%


set class RubyCompiler
category: '*maglev-runtime'
method:
compileString: rubySource loadName: aName 
    "A ruby primitive.
    Used by  lib/ruby/site_ruby/1.8/maglev/ruby_compiler.rb  .
    Compile and run ruby source in sourceString. aName is the name passed to require or load.
    The parser uses aName to fill out __FILE__"
 | envId |
 envId := 1"__callerEnvId" .
^ [ | ast res cmState compStack prevSelf cm trace tmps warn |
    cmState := RubyCompilerState initialize: envId .
    (prevSelf := SessionTemps current at: #RubyMainSelf otherwise: nil) ifNil:[
        RubyLoadError signal:'compileString: cannot be used for the Ruby main program'
     ].
    trace := SessionTemps current at: #MagRpDEBUG otherwise: 0 .
    warn := SessionTemps current at:#MAGLEV_parseWarn otherwise: false .
    ast := RubyParserM rpParseString: rubySource path: aName loadName: aName 
                yTrace: trace - 1 warnings: warn .
    (compStack := cmState compilerStack) push: self .
    [  cm := self compileEvalMethod: #__compileFile  inClass: Object rubyMethod: ast  env: envId .
       res := prevSelf  performMethod:  cm  .
    ] ensure:[
       compStack pop: self .
       prevSelf ifNil:[ SessionTemps current at: #RubyMainSelf put: nil ].
    ].
    res
  ] onException: AbstractException do:[:ex |  | msg |
     msg := [   'error , ' , ex asString,  ',
          during ' , aName .
    ] onException: AbstractException do:[:exb |
       exb return: 'error during , ' , aName
    ].
    GsFile gciLogClient: msg .
    ex pass
  ].

%


set class RubyCompiler
category: 'as yet unclassified'
method:
currentClass
	^ currentClass

%


set class RubyCompiler
category: '*maglev-runtime'
method:
defineClassNamed: aSymbol rubyMethod: aNode inScope: aParent fixedIvs: ivList
   ^ self defineClassNamed: aSymbol rubyMethod: aNode inScope: aParent 
      superclass: Object env: 1"__callerEnvId" fixedIvs: ivList

%


set class RubyCompiler
category: '*maglev-runtime'
method:
defineClassNamed: aSymbol rubyMethod: aNode inScope: parentArg   
    superclass: superCls  env: envId fixedIvs: ivList
      "ruby_selector_suffix dependent, use of perform"
  |  aCls res parNs parVal clsTns inBoot aParent  |
  parNs := (aParent := parentArg) nameSpaceOrNil: envId  . 
  parNs ifNil:[
     aParent isBehavior ifFalse:[ aParent := aParent _singletonClassFor: envId ].
     parNs := aParent nameSpace: envId .
  ] ifNotNil:[
     aParent isBehavior ifFalse:[ aParent := parNs myClass ].
  ].
  parNs ifNotNil:[  "a singleton class may have no name space"
    parVal := parNs classOrNameSpaceAt: aSymbol inModule: aParent .
    parVal isBehavior ifTrue:[ aCls := parVal ]
                 ifFalse:[ parVal isNameSpace ifTrue:[ clsTns := parVal transientCopy ]].
  ].        
  inBoot := installingPrims .       
  aCls ifNil:[   | cst  |
    cst := RubyCompilerState current .
    aCls :=  superCls newRubySubclass: aSymbol instancesPersistent: cst persistableInstances
        fixedIvs: ivList .
    parNs ifNil:[ parNs := aParent initNameSpacesForExtend: envId ]
          ifNotNil:[ parNs := parNs transientCopy ].
    self initSubclass: aCls name: aSymbol parentNs: parNs tns: clsTns cState: cst env: envId. 
    aCls _incrementCachedConstantsSerialNum .
    parNs at: aSymbol runtimePut: aCls .
    inBoot ifFalse:[
      aCls immediateInvariant.
      "Can't use @ruby1, must use perform to bypass meth protection"
      superCls with: aCls perform:#'inherited#1__' env: 1 .
           "See also kernel/bootstrap/Class.rb for Class.new(superCls)"
      res := aNode ifNotNil:[ self extendClass: aCls rubyMethod: aNode env: envId ]
                   ifNil:[ aCls ].
    ] ifTrue:[   
      " send of #inherited not done in bootstrap"
      "leave class modifiable, for fixedInstvar to be defined automatically based
       on uses in method bodies."
      res := aNode ifNotNil:[ self extendClass: aCls rubyMethod: aNode env: envId ]
                   ifNil:[ aCls ].
      aCls immediateInvariant.
    ].
  ] ifNotNil:[ | existingSuper |
    ivList size ~~ 0 ifTrue:[ 
       ArgumentError signal:'__fixed_instvars usable only in first opening of a class'
    ].
    superCls == Object ifFalse:[  "deleted inBoot term"
      (existingSuper := aCls superClass "non-virt superclass") == superCls ifFalse:[
           ArgumentTypeError signal: 'superclass mismatch for ', aSymbol . 
      ].
    ].
    inBoot ifTrue:[ 
      (aCls transientNameSpace: envId) ifNil:[
        aCls deleteMethods: envId .  "remove all persistent methods"
        aCls virtualClass deleteMethods: envId .
      ].
    ].
    clsTns := aCls initNameSpacesForExtend: envId .  "creates a tns"
    clsTns _parent ifNil:[ aCls ~~ Object ifTrue:[ clsTns parent: parNs "extending a Smalltalk cass"]].
    inBoot ifTrue:[
      (aCls transientNameSpaceForStore: envId) _name: aSymbol .  "override smalltalk name"
    ].    
    res := aNode ifNotNil:[ self extendClass: aCls rubyMethod: aNode env: envId ]
        ifNil:[ aCls ].
  ].
  ^ res

%


set class RubyCompiler
category: '*maglev-runtime'
method:
defineClassNamed: aSymbol rubyMethod: aNode inScope: aParent superclass: superCls
    fixedIvs: ivList

  "called from generated code"
   ^ self defineClassNamed: aSymbol rubyMethod: aNode inScope: aParent
      superclass: superCls env: 1"__callerEnvId" fixedIvs: ivList

%


set class RubyCompiler
category: '*maglev-runtime'
method:
defineModuleNamed: aSymbol rubyMethod: aNode inScope: parentArg 
  "called from generated code"
^ self defineModuleNamed: aSymbol rubyMethod: aNode inScope: parentArg 
    env: 1"__callerEnvId"

%


set class RubyCompiler
category: '*maglev-runtime'
method:
defineModuleNamed: aSymbol rubyMethod: aNode inScope: parentArg env: envId
  | aModu parNs parVal moduTns aParent |
  parNs := (aParent := parentArg) nameSpaceOrNil: envId  . 
  parNs ifNil:[
     aParent isBehavior ifFalse:[ aParent := aParent _singletonClassFor: envId ].
     parNs := aParent nameSpace: envId .
  ] ifNotNil:[
     aParent isBehavior ifFalse:[ aParent := parNs myClass ].
  ].
  parNs ifNotNil:[  "a singleton class may have no name space"
     parVal := parNs classOrNameSpaceAt: aSymbol inModule: aParent .
     parVal isBehavior ifTrue:[ aModu := parVal ]
                 ifFalse:[ parVal isNameSpace ifTrue:[ moduTns := parVal transientCopy ]].
  ].
  aModu ifNil:[ 
     aModu := Module _newModule .
     aModu name: aSymbol .
     parNs ifNil:[ parNs := aParent initNameSpacesForExtend: envId ]
           ifNotNil:[ parNs := parNs transientCopy ].
     aModu init_parentNs: parNs tns: moduTns env: envId .
     aModu _incrementCachedConstantsSerialNum .
     parNs at: aSymbol runtimePut: aModu .
  ] ifNotNil:[ | cst |
    aModu is_aModule ifFalse:[ ArgumentTypeError signal:'arg to module keyword exists but is not a Module'].
    RubyCompilerState current installingPrims ifTrue:[  "reloading bootstrap code"
      (aModu transientNameSpace: envId) ifNil:[   
        aModu deleteMethods: envId .  "remove all persistent methods"
        aModu moduleMethodsModule deleteMethods: envId .
        aModu transientNameSpaceForStore: envId . "create a tns"
      ].
    ]
  ].
  aNode ifNotNil:[
    self extendModule: aModu rubyMethod: aNode env: envId .
  ].
  ^ aModu

%


set class RubyCompiler
category: '*maglev-runtime'
method:
evalLineBias
  ^ 1  "just a synthesized 'begin' at start of the eval"

%


set class RubyCompiler
category: '*maglev-runtime'
method:
evalMethodSource: aString with: vcGlobalsArr binding: aBinding
  "given a String and a RubyBinding, build the complete source string   for an eval "
   | methSrc lf |
   lf := Character lf .
  "$~, $_  initialized to default nil"
   (methSrc :=  'begin' copy ) add: lf ;
          add: aString ; add: lf ;
          add: 'ensure'; add: lf ;
          add: '  Thread.__evVcGput($~, $_)' ; add: lf ;
          add: 'end' ; add: lf .
   ^ methSrc

%


set class RubyCompiler
category: '*maglev-runtime'
method:
evaluateString: aString binding: aBinding with: vcGlobalsArgs fileName: aFileName 
    lineNumber: aLineNumber  env: envId
  |  evalArgsStk evalArgs |
  "aBinding is a  RubyBinding and takes precedence over self in vcGlobalsSelf"
  ^ [ | bnd |
      bnd := aBinding .
      (evalArgsStk := GsProcess _current _rubyThreadDataAt: 6 ) 
         push: (evalArgs := { bnd . vcGlobalsArgs atOrNil: 4"block_arg" }) .
      self evaluateString: aString  with: vcGlobalsArgs 
          withSelf: bnd  selfObj binding: bnd 
          fileName: aFileName lineNumber: aLineNumber  env: envId 
    ] ensure:[
       evalArgsStk pop: evalArgs 
    ]

%


set class RubyCompiler
category: '*maglev-runtime'
method:
evaluateString: aString with: vcGlobalsArr withSelf: theSelf binding: aBinding 
  fileName: aFileName lineNumber: aLineNumber env: envId

  |   cst fileStk compStk aFile res methSrc  callerSel  evalSel
    fname forModuleEval lexPath prevLexPath rtModuStk evalScope"trac829" |
  envId < 1 ifTrue:[ self error: 'evaluateString: invalid envId' ].
  evalSel := self class blockEvalSelector.
      "ruby_selector_suffix dependent"
  callerSel := #'__evalCaller#2__' . "must agree with implem in bootstrap/Object.rb"
     "we will compile aString as main program level  ruby code so we don't get
      parse errors about dynamic constant defn or class def inside method (ticket 276)"
  methSrc := self evalMethodSource: aString with: vcGlobalsArr binding: aBinding .
  cst := RubyCompilerState initialize: envId .
  aBinding ifNotNil: [
    lexPath := aBinding lexicalPath ifNil:[ RubyLexicalPath withAll: { Object } ].
    (forModuleEval := aBinding forModuleEval) ifTrue:[ lexPath add: theSelf ].
    (evalScope := RubyEvalScope _basicNew) binding: aBinding . "trac 829"
  ] ifNil: [
    forModuleEval := false.  "lexPath, evalScope left as nil"
  ] .

  fname := aFileName ifNil:[ '(eval)' ].
  aFile := RubyEvalFile new source: methSrc  .
  aFile fileName: fname .
  (fileStk := cst fileStack) push: aFile .
  (compStk := cst compilerStack) push: self .
  rtModuStk := cst rtModuleStack .
  prevLexPath := rtModuStk copy .
  lexPath ifNotNil:[ rtModuStk size: 0 ; addAll: lexPath  ].
  [  |  cm prevSelf rubyRootNode |
        rubyRootNode :=  RubyParserM rpParseString: methSrc scope: evalScope
             lineBias: ( aLineNumber - self evalLineBias "extra lines for vcGlobals...")
         evalScope: evalScope fileName: fname .
        rubyRootNode fileName: fname  source: methSrc .
         "irMethodNode will be sent to  ast  later resulting in a zero arg method"
        cm := self compileEvalMethod: evalSel inClass: theSelf class
		  rubyMethod: rubyRootNode env: envId .
        vcGlobalsArr at:3 put: theSelf  .   "append/overwrite rcvr of the eval"
        prevSelf := GsProcess initRubyMainThread: false env: envId .
        prevSelf ifNil:[
          self class withRubyHandlers: envId main: true do: [
            res:= theSelf with: vcGlobalsArr with: cm perform: callerSel env: envId .
          ]
        ] ifNotNil:[
          res:= theSelf with: vcGlobalsArr with: cm perform: callerSel env: envId
        ]
  ] ensure:[
    compStk pop: self .
    fileStk pop: aFile .
    rtModuStk size: 0 ; addAll: prevLexPath .
  ].
  ^ res

%


set class RubyCompiler
category: '*maglev-runtime'
method:
extend: anObject rubyMethod: aNode blk: blkArg rtModulePath: rtModules
  "called from generated code, used for RubySClassNode "
  | envId cld defStk rtModuStk prevModules cls | 
  envId := 1"__callerEnvId" . 
  cls := anObject _singletonClass: envId .
  cld := GsProcess _current clientData .
  (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: cls .
  cld at: 7 put: cls " _rubyThreadDataAt: 7 put: " .
  rtModuStk := cld at: 5  "_rubyThreadDataAt: 5, rtModuleStack" .
  prevModules := rtModuStk copy .
  rtModuStk size: 0 ; addAll: rtModules; add: cls .
  ^ [  | cm mArray |
      cm := self compileMethod: #'__compileClass' inClass: cls class 
              rubyMethod: aNode env: envId isEval: false .
      cls with: blkArg performMethod: cm  "pass blkArg for Trac 808"
    ] ensure:[
      defStk pop: cls .
      rtModuStk size: 0 ; addAll: prevModules .
      cls rubyMethodProtection: 0 env: envId . "reset default to public"
    ]

%


set class RubyCompiler
category: '*maglev-runtime'
method:
extendClass: aClass rubyMethod: aNode env: envId
 | cld defStk rtModuStk |
  cld := GsProcess _current clientData .
  (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: aClass .
  cld at: 7 put: aClass " _rubyThreadDataAt: 7 put: " .
  rtModuStk := cld at: 5  "_rubyThreadDataAt: 5, rtModuleStack" .
  rtModuStk push: aClass .
  ^ [ | cm |
      cm := self compileMethod: #'__compileClass' inClass: aClass class rubyMethod: aNode
         env: envId isEval: false .
      aClass performMethod: cm .
    ] ensure:[
      aClass rubyMethodProtection: 0 env: envId . "reset default prot to public" 
      defStk pop: aClass .
      rtModuStk pop: aClass .
    ].

%


set class RubyCompiler
category: '*maglev-runtime'
method:
extendModule: aModule rubyMethod: aNode env: envId
  | cld defStk rtModuStk |
  cld := GsProcess _current clientData .
  (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: aModule .
  cld at: 7 put: aModule " _rubyThreadDataAt: 7 put: " .
  rtModuStk := cld at: 5  "_rubyThreadDataAt: 5, rtModuleStack" .
  rtModuStk push: aModule .
  ^ [ | cm | 
     cm := [ currClassOrModule := aModule .
       self compileMethod: #'__compileClass' inClass: aModule class 
                rubyMethod: aNode env: envId isEval: false .
     ] ensure:[
        currClassOrModule := nil .
     ].
     [ 
       aModule performMethod: cm .
     ] ensure:[
       aModule rubyMethodProtection: 0 env: envId; "reset default prot to public"
            disableModuleMethodsAll: envId  .
     ]. 
  ] ensure:[
    rtModuStk pop: aModule .
    defStk pop: aModule .
  ]

%


set class RubyCompiler
category: 'as yet unclassified'
method:
indexOfInstVar: aSymbol with: aNode
   "aSymbol is an instVar name without leading $@ "
	|  index  cls names |
	names := (cls := currentClass) _instVarNames .
	index := names ifNil:[ 0] ifNotNil: [ names indexOfIdentical: aSymbol ] .
	index == 0 ifTrue:[
		cls isModifiable_oops_notModule ifTrue:[
				cls addRubyInstVar: aSymbol .
				index := currentClass _instVarNames indexOfIdentical: aSymbol ]
			ifFalse:[ index := -1 . "a dynamic instVar" ]
	] ifFalse:[  | privateSz |
	  ((privateSz := cls rubyPrivateInstSize) > 0 and:[ index <= privateSz 
	           and:[  installingPrims not]]) ifTrue:[
		  aNode signalParseError: ' in class ',  cls name , ' instVar ' , aSymbol , ' is private to Smalltalk'
	   ].
	].
	^ index

%


set class RubyCompiler
category: '*maglev-compiling'
method:
initialize
  installingPrims := RubyCompilerState current installingPrims .
  useRubyParser := 2 .
  needsSuperEach := false .

%


set class RubyCompiler
category: '*maglev-runtime'
method:
initSubclass: cls name: aSymbol parentNs: parTns tns: tnsArg cState: cst env: envId
  | pm cx tns metaCls  |
  pm := cst persistenceMode .
  cls _setRubyModulePersistentBit: pm .
  " meta _setInstancesPersistentBit:   done by   newRubySubclass:... "
  
  (tns := tnsArg) ifNotNil:[
	 cls installTransientNameSpace: tns persistentMode: pm env: envId .
	 (pm and:[ cst installingPrims ]) ifTrue:[
		 tns copyAssociationsToPns "for constants created at compile time"
	 ].
  ] ifNil:[
    tns := cls initNameSpacesForExtend: envId . 
  ].
  tns parent: parTns "maybe nil" .

  tns := (metaCls := cls virtualClass) initNameSpacesForExtend: envId .
  tns parent: (parTns myClass virtualClass transientNameSpaceForStore: envId) .

%


set class RubyCompiler
category: '*maglev-runtime'
method:
installBridgeMethodsFor: argSymbol  in: aClass argsDescr: argsDescrInt optArgs: optArgsDescr
                    protection: protInt primKind: kindArg env: envId
       "During install of primitives,  
       a bridge method will not override any existing ruby method .
       This is to allow coding variants of Ruby methods by hand 
       for things like Object >> send .   
       optArgsDescr is a SmallInteger containing 1 bits for args with default value.
       optArgsDescr is zero if installing bridge methods to a primitive.
       Returns ruby selector prefix of argSymbol  "   
  | inPrims primKnd |
  inPrims :=  installingPrims .
  inPrims ifTrue:[ 
    (self bridgeForPrimAllowed: argSymbol)  ifFalse:[   ^ self "do nothing" ] .
    primKnd :=  kindArg + 4 .
  ] ifFalse:[
     primKnd := kindArg . 
  ].
  ^ RubyBridge installBridgesFor: argSymbol in: aClass argsDescr: argsDescrInt 
           optArgs: optArgsDescr protection: protInt  primKind: primKnd 
           env: envId

%


set class RubyCompiler
category: 'as yet unclassified'
method:
instVarExists: stName with: aNode 
  "return true if there is already a GsComVarLeaf for the specified instVar,
   or if aName specifies an instVar with a fixed offset" 
   |  leaf |
   leaf := leaves at: stName otherwise: nil .
   ^ leaf ifNotNil:[ true]  ifNil:[  (self indexOfInstVar: stName with: aNode) >= 0 ]

%


set class RubyCompiler
category: 'as yet unclassified'
method:
leafForInstVar: stName rubyName: rubyName with: aRubyNode
  | sym |
  sym := stName ifNil:[ rubyName ].
	^ leaves at: sym  ifAbsentPut:      "dictionary keyed on smalltalk style names"
		[ | ivOfs  |
		  ivOfs := self indexOfInstVar: sym with: aRubyNode .
		  ivOfs == -1 ifTrue:[
		    installingPrims ifTrue:[
			  aRubyNode signalParseError:'dynamic instVar ', sym ,
			     '  not allowed in bootstrap code ' 
			].
		  ].
		  GsComVarLeaf new
			instanceVariable: sym   
			ivOffset:  ivOfs
		]

%


set class RubyCompiler
category: 'as yet unclassified'
method:
log: aString
	self class log: aString

%


set class RubyCompiler
category: '*maglev-runtime'
method:
parseFileNamed: fullPath loadName: aName
  | ast trace warn  tmps|
  tmps := SessionTemps current .
  trace := tmps at: #MagRpDEBUG otherwise: 0 .
  warn := tmps at:#MAGLEV_parseWarn otherwise: false .
"  useRubyParser == 2 ifTrue:["

   trace ~~ 0 ifTrue:[ GsFile gciLogServer:'Mel parse ' , aName ].  
   ast := RubyParserM rpParseFile: fullPath  loadName: aName yTrace: trace - 1 
        warnings: warn .
 
"
    ] ifFalse:[
  useRubyParser == 0 ifTrue:[
    trace ~~ 0 ifTrue:[ GsFile gciLogServer:'MRI parse ' , aName ].
    (alt := A_ltParser) ifNotNil:[
       alt printSexpForFileNamed: fullPath loadName: aName
    ].
    ast := (parser := self parser) parseFileNamed: fullPath loadName: aName .
    ast fileName: fullPath source:  ( parser catFileNamed: fullPath) .
    ] ifFalse:[
  useRubyParser == 1 ifTrue:[ 
    trace ~~ 0 ifTrue:[ GsFile gciLogServer:'Rp parse ' , aName ].  
    ast := R_ubySexpParser rpParseFile: fullPath loadName: aName
  ] ifFalse:[
    self error:'Ruby parser not specified'
  ]]].
"
  ^ ast  "result is after walkWithScopes has been done"

%


set class RubyCompiler
category: 'as yet unclassified'
method:
parser
  ^ Parser

%


set class RubyCompiler
category: '*maglev-runtime'
method:
reimplementationAllowed: aSymbol node: aNode cst: aRubyCompilerState
      "ruby_selector_suffix dependent"
  (self class reimplementationAllowed: aSymbol for: currentClass 
                  cst: aRubyCompilerState ) ifFalse:[ | msg |
     msg := 'reimplementation of  ' , aSymbol , '  not allowed, near ' ,
                    aNode sourcePositionAsString .
     aRubyCompilerState installingPrims ifTrue:[
       Error signal: msg .
     ].
     (SessionTemps current at: #MAGLEV_parseWarn otherwise: false) ifTrue: [
       GsFile gciLogServer: 'WARNING ' , msg .
       "(aSymbol at:1 equals:'__send') ifTrue:[ self pause ]."
     ].
     ^ false
  ].
  ^ true

%


set class RubyCompiler
category: 'as yet unclassified'
method:
setNeedsSuperEach
  needsSuperEach := true

%


set class RubyCompiler
category: 'as yet unclassified'
method:
sourceForIR: irNode
	|strm|
	strm := IndentingStream newPrinting.
	irNode printFormattedOn: strm.
	^ strm contents.
	

%


set class RubyCompiler
category: 'as yet unclassified'
method:
sourceString
   | str s f |
   (str := sourceString) ifNotNil:[   ^ str ].
    s := (f := RubyCompilerState current fileStack topOrNil)  ifNotNil:[ f source].
    ^ s ifNotNil:[  s, '
###' ].
		

%


set class RubyCompiler
category: 'as yet unclassified'
method:
startLog
	self log: '/---'

%


set class RubyCompiler
category: 'as yet unclassified'
method:
stopLog
	self log: '---/'

%


set class RubyCompiler
category: 'as yet unclassified'
method:
useRubyParser: aBool
  useRubyParser := aBool

%

