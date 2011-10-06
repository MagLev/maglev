
set class GsProcess
category: '*maglev-runtime'
classmethod:
anonymousSelectors

  ^ #(__compileFile __compileEval)

%


set class GsProcess
category: '*maglev-runtime'
classmethod:
backtraceToLevel: aLevel
| backtrace aFrame meth level |        
backtrace := Array new .
level := 1 .
[ level <= aLevel _and:[ (aFrame := self _frameContentsAt: level + 1) ~~ nil] ] whileTrue:[
    level := level + 1.
    meth := aFrame at: 1 . "a GsNMethod"
    meth == nil ifFalse:[ | farr env |
      env := meth environmentId .
       farr := { 
         meth _descrForStack . 
          (meth _lineNumberForStep: (meth _previousStepPointForIp: (aFrame at: 2)))  + meth _lineNumberBias  . 
         env . 
         meth homeMethod _rubyName 
       }.
       env ~~ 0 ifTrue:[ | fileLine |
          farr add: meth isRubyBridgeMethod .
         fileLine := meth _fileAndLine .
         fileLine ifNotNil:[ farr addAll: fileLine ].
       ].
       backtrace add: farr .
    ].
].
^ backtrace

%


set class GsProcess
category: '*maglev-runtime'
classmethod:
currentMethDefTarget
  ^ self   _current _rubyThreadDataAt: 7 

%


set class GsProcess
category: '*maglev-runtime'
classmethod:
initRubyMainThread: inPrims env: envId
  "returns the previous main program self , or nil if this is the
   first invocation in a ruby session."

| prevSelf tmps oMain |
prevSelf := (tmps := SessionTemps current) at: #RubyMainSelf otherwise: nil.
prevSelf ifNil:[
  tmps at:#RubyMainSelf put: (oMain := Object new) ;
     at:#RubyMainThread put: self _current ;
     at:#RubyDefaultThreadGroup put: RubyThreadGroup new ;
     at:#RubyExitHandlers put: Array new .
  inPrims ifFalse:[ 
     RubyContext @ruby1:customize_top_self: oMain .
     System _sessionStateAt: 19 put: RubyIdentityHash new . "dict of RubyFinalizer's " 
  ] .
  tmps at:#RubyMainInitialized put: true .  "for use in debugging AST to IR code"
  System abortTransaction . "abort changes to commited AST from IR generation of top_self"
].
^ prevSelf

%


set class GsProcess
category: '*maglev-runtime'
classmethod:
methodDefnMetaTarget
  "called from generated code"  
  ^ ( self _current _rubyThreadDataAt: 3 ) topValue theMetaClass

%


set class GsProcess
category: '*maglev-runtime'
classmethod:
methodDefnTarget
  "called from generated code and Object>>_bindingContext: "  
  ^ ( self _current _rubyThreadDataAt: 3 ) topOrNil 

%


set class GsProcess
category: '*maglev-runtime'
classmethod:
rubyExitOnException: aBoolean
 "Returns previous value of the abort_on_exception state.
  If arg not nil, installs arg as new value."
 | prev tmps |
 tmps := SessionTemps current .
 prev := tmps at:#RubyExitOnException otherwise: false .
 aBoolean ifNotNil:[
   tmps at:#RubyExitOnException put: aBoolean
 ].
 ^ prev

%


set class GsProcess
category: '*maglev-runtime'
classmethod:
_rubyEvalBinding

  ^ (self _current _rubyThreadDataAt: 6"evalArgsStk") topValue at: 1"aBinding"

%


set class GsProcess
category: '*maglev-runtime'
classmethod:
_rubyEvalBlockArg

  ^ (self _current _rubyThreadDataAt: 6"evalArgsStk") topOrNil 
       ifNotNil:[ :evArgs | evArgs at: 2"block_arg" ].

%


set class GsProcess
category: '*maglev-runtime'
classmethod:
_rubyEvalHomeMethod
  "returns the name of the home method, per __method__"
^ self _rubyEvalBinding homeMethod _rubyName

%


set class GsProcess
category: '*maglev-runtime'
classmethod:
_rubyEvalVc

  ^ ((self _current _rubyThreadDataAt: 6"evalArgsStk") topValue at: 1"aBinding") context

%


set class GsProcess
category: '*maglev-runtime'
classmethod:
_rubyEvalVcPutTilde: tArg underscore: uArg

  tArg _storeRubyVcGlobal: 16r70 .
  uArg _storeRubyVcGlobal: 16r71 .

%


set class GsProcess
category: '*maglev-runtime'
classmethod:
_topazExceptionName: anException
  
 ^ [ | ecls |
      (ecls := anException class) _isExceptionClass ifFalse:[ ^ nil ].
      (SessionTemps current at:#RubyMainSelf otherwise: nil) ifNotNil:[
         "Ruby main program active, attempt Ruby execution to get name"
         [ ^ ecls @ruby1:name ] onException: AbstractException do:[:ex | ex return ].
       ].
       ecls name
    ] onException: AbstractException do:[:ex |
      ex return: '<error during _topazExceptionName:>'
    ].

%


set class GsProcess
category: '*maglev-Accessing'
method:
args
	^ args

%


set class GsProcess
category: '*maglev-Accessing'
method:
arStack

	^ arStack

%


set class GsProcess
category: '*maglev-Accessing'
method:
block
	^ block

%


set class GsProcess
category: '*maglev-Debugging Support'
method:
convertState: stateClass andStack: stackClass to: newStateClass and: newStackClass
	| stackSet |
	self _clientData ifNil: [^ self].
	stackSet := IdentityDictionary new.
	
	"migrate any compiler state"
	(self _clientData select: [:ea | ea class == stateClass]) asArray do: [:state || newState |
		newState := newStateClass new.
		stateClass instVarNames withIndexDo: [:name :idx || var |
			var := state instVarAt: name.
			var class == newStackClass
				ifTrue: [newState instVarNamed: name put: (stackSet at: var ifAbsentPut: [(newStackClass new: var size)
																							addAll: var;
																							yourself])]
				ifFalse: [newState instVarNamed: name put: var]].
		self _clientData at: (self _clientData indexOf: state) put: newState].
	
	"migrate any compiler stacks"
	(self _clientData select: [:ea | ea class == stackClass]) asArray do: [:stack |
		self _clientData
			at: (self _clientData indexOf: stack)
			put: (stackSet at: stack ifAbsentPut: [(newStackClass new: stack size)
													addAll: stack;
													yourself])].


%


set class GsProcess
category: '*maglev-Debugging Support'
method:
convertToPersistableState
	
	self
		convertState: RubyCompilerState andStack: RubyCompilerStack
		to: RubyPersistableCompilerState and: RubyPersistableCompilerStack

%


set class GsProcess
category: '*maglev-Debugging Support'
method:
convertToRunnableState

	self
		convertState: RubyPersistableCompilerState andStack: RubyPersistableCompilerStack
		to: RubyCompilerState and: RubyCompilerStack

%


set class GsProcess
category: '*maglev-Debugging Support'
method:
rubyPersistentStateClasses

    ^ #(RubyPersistableCompilerState RubyPersistableCompilerStack)

%


set class GsProcess
category: '*maglev-Debugging Support'
method:
rubyTransientStateClasses

    ^ #(RubyCompilerState RubyCompilerStack)

%


set class GsProcess
category: '*maglev-runtime'
method:
_currentMethDefMetaTarget
  "For debugging with topaz"
  ^ (self  _rubyThreadDataAt: 7 ) theMetaClass

%


set class GsProcess
category: '*maglev-runtime'
method:
_currentMethDefTarget
  "For debugging with topaz"
  ^ self _rubyThreadDataAt: 7 

%


set class GsProcess
category: '*maglev-runtime'
method:
_defTargets
 "For debugging with topaz"
^ { (self _rubyThreadDataAt: 3 ) .
    (self _rubyThreadDataAt: 7)  }

%


set class GsProcess
category: '*maglev-override-Debugging Support'
method:
_localTrimStackToLevel: aLevel

"Deletes stack entries from 1 to (aLevel - 1) inclusive, thus making aLevel the
 new top-of-stack(TOS).  At new TOS, looks in the implementation class of the
 method at TOS, using the selector of the method at TOS.  If that class's method
 dictionary contains a different GsNMethod, the GsNMethod currently in the method
 dictionary is installed as the method at the TOS.  The saved instruction
 pointer for TOS is set to the entry of the method at TOS.

 Limitations:
   If the receiver was executing in native code, and the stack
   is trimmed, any resumption of execution will be in interpreted mode.

   If the new top-of-stack is an anonymous method, it is not possible to
   determine whether that method has been recompiled, and the method at new top
   of stack will not be changed.  Debuggers should use the
   GsNMethod | _recompileWithSource method to implement recompilation from a
   debugger pane.  _recompileWithSource: raises an error on attempts to
   recompile an anonymous method.

   Raises an error if the new top-of-stack would represent the entry to an
   ExecBlock and the home method of the block has been recompiled.

   Raises an error if the new top-of-stack would represent a return 
   across an FFI , Ruby C extension, or UserAction call.

 Has no effect if aLevel is out of range or aLevel < 2.

 Debuggers must not cache or directly manipulate VariableContexts 
 when examining or altering stacks.

 Provides the ability for a debugger to restart execution after recompiling
 a method that is active on the stack of the receiver."

 | fpIdx aMeth oldHome oldmClass newTosIdx oldDepth newDepth newTosFpOfs 
   prevFpIdx envId |
  self _nativeStack ifTrue:[
    self _convertToPortableStack .
  ].
  "check for argument aLevel out of range"
  oldDepth := self localStackDepth .
  (aLevel < 2 or:[ aLevel > oldDepth ]) ifTrue:[ ^ self ].
  newDepth := oldDepth - (aLevel - 1) .

  1 to: aLevel do:[:n| 
    prevFpIdx := fpIdx .
    fpIdx := self _localFrameOffsetAt: n  . 
    aMeth := self _fetchMethod:( arStack at:( fpIdx + FP_codePtr_OFS)) .
    aMeth ifNil:[ self _halt:'Cannot trim stack across a reenter marker'].
  ].
  
  "check to see whether new TOS method has been recompiled, and if so, 
   install the new method."
  oldHome := aMeth homeMethod .
  oldmClass := oldHome inClass .
  envId := (oldHome respondsTo: #environmentId) ifTrue: [oldHome environmentId] ifFalse: [0].
  (oldmClass isNil or: [self class anonymousSelectors includes: oldHome selector]) ifFalse: [
    "not an anonymous method"
    | newMeth oldCptr |
    newMeth := oldmClass compiledMethodAt: oldHome selector environmentId: envId.
    newMeth == aMeth ifFalse:[
      aMeth == oldHome ifFalse:[
        self _halt:'Cannot trim stack to a block in recompiled method'.
      ].
      "install recompiled method"
      oldCptr := arStack at: (fpIdx + FP_codePtr_OFS) . 
      oldCptr class == GsNMethod ifTrue:[
        arStack at: (fpIdx + FP_codePtr_OFS) put: newMeth .
      ] ifFalse:[
        self _halt:'should not be here'
      ].
    ].
  ].
  newTosIdx := fpIdx + FP_codePtr_OFS .
  "push savedIP which is byte offset to first instr in the method"
  newTosIdx := newTosIdx - 1 .
  arStack at: newTosIdx put: OC_GSNMETHOD_FIRST_INSTR_OFFSET .
  "don't need  _tosIsIpNil in arStack object"

  newTosFpOfs := arStack at: prevFpIdx .
  topFpOffset := newTosFpOfs .  "adjust offset to Top FP"

  "delete the unwanted stack frames.  
     Since FP linkages in arStack are relative to   arStack size    
     there is no other fixup needed. "
  arStack removeFrom:1 to: newTosIdx - 1 .

  "adjust the depth of the receiver."
  uaCount_depth := ((uaCount_depth bitShift: -32)  bitShift:32)  
         bitOr: (newDepth bitAnd: 16rFFFFFFFF) .
  
  dbgFpsCache := nil . "clear frame offsets cache"

%


set class GsProcess
category: '*maglev-runtime'
method:
_methodDefnTarget
  "for debugging only"
  ^ (  self _rubyThreadDataAt: 3 ) topOrNil

%


set class GsProcess
category: '*maglev-runtime'
method:
_rubyInspect
  | str  |
  str := '#<' copy .  
  str addAll: self class name ;
     addAll: ':0x' ; addAll: self asOop hex ; add: $  ;
     addAll: self rubyStatus asString  ;
     add: $> .
  ^ str

%


set class GsProcess
category: '*maglev-runtime'
method:
_startRuby: envId
      "ruby_selector_suffix dependent"

 | blockCopy argsCopy |
 blockCopy := block.
 argsCopy := args.
 RubyCompilerState initializeForNewThread: envId .
 ^ [
     RubyCompiler withRubyHandlers: envId main: false do: [
      envId == 1 ifTrue:[ blockCopy with: argsCopy perform:#'call#0*_' env: 1 ]
         ifFalse:[ Exception signal:'envId > 1 not supported' . nil ]
     ]
   ] onException: {  TerminateProcess . AbstractException } 
      do: { [:ex |
            GsProcess _current _handleTerminateProcess: ex .
            ex return: ex  "return exception as thread result"
          ] .
          [:ex |  ex return: ex  "return exception as thread result" ]
        }

%

