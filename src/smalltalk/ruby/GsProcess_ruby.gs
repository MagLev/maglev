!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: GsProcess_ruby.gs 26876 2011-09-23 16:26:12Z otisa $
!
!  additional methods  for GsProcess to support Ruby 
!=========================================================================

set class GsProcess
! adding additional methods , don't 'removeallmethods'

category: 'Ruby support'

!Allow subclasses of GsProcess for Ruby
expectvalue true
run
GsProcess allowSubclasses .
^ GsProcess subclassesDisallowed == false
%


method:
rubyPrivateSize
^ 20 "inline  GsProcess instSize"
%

classmethod: 
rubyPrivateInstSize
^ GsProcess instSize 
%

classmethod:
_rubyThreadDataAt: anOffset

 ^ self _current _rubyThreadDataAt: anOffset
%

method:
_rubyThreadDataAt: anOffset
 " Returns the Ruby thread-local data specified by anOffset, or nil.
  anOffset 	data
    1           result of RubyCompilerState current
    2           Ruby global $? , last child process status (a SmallInteger)
    3           method def target stack
    4           IdentitySet for ruby RecursionGuard
    5           The runtime ModuleStack  , rtModuleStack
    6           evalArgs stack 
    7           current method definition target (not a stack)
   Offsets symbolically named see *RubyGsProcessClientData* in opalcls.ht, bom.c
"
 ^ clientData at: anOffset
%

method:
_newRubyThreadData
  ^ clientData ifNil:[
      clientData := Array new: OC_RubyGsProcessClientDataSize .
    ]
%

classmethod:
_recursionGuardSet
 "use _newRubyThreadData for lazy init, to handle   topaz inspect ..."

 ^ (( self _current clientData ifNil:[ self _current _newRubyThreadData ])
      at: GC_RubyGsProcessClientData_recursionGuardSet) ifNil:[
    self _current clientData at: 4 put: IdentitySet new 
 ]
%

classmethod:
_rubyThreadDataAt: anOffset put: aValue

 "Returns receiver.  Store aValue into Ruby thread-local data.
  anOffset defined by  comments in _rubyThreadDataAt: "

 ^ self _current _rubyThreadDataAt: anOffset put: aValue
%

method:
_rubyThreadDataAt: anOffset put: aValue
  clientData at: anOffset put: aValue
%

method:
rubyPrivateSize
  ^ GsProcess instSize
%
classmethod:
rubyPrivateInstSize
  ^ GsProcess instSize
%

! initRubyMainThread moved to .mcz

classmethod:
allProcesses

  ^ self _scheduler allProcesses asArray
%

classmethod:
rubyMain
  "The RubyMainThread is initialized in class RubyCompiler.  "

  ^ SessionTemps current at: #RubyMainThread otherwise: nil
%  

classmethod:
pass

  "Returns receiver."
  self _scheduler yield
%

classmethod:
stop
  		"TODO set Thread.critical to false"
  self _current suspend .
%  

method:
threadDataAt: aName
  | dict |
  aName _isSymbol ifTrue:[
    (dict := environment) ifNil:[ ^ nil ].
    ^ dict at: aName otherwise: nil .
  ] ifFalse:[
    aName _isOneByteString ifFalse:[
      ArgumentError signal:'argument is not a String or Symbol'.
    ].
    (dict := environment) ifNil:[ ^ nil ].
    ^ dict at: aName asSymbol otherwise: nil .
  ]
%

method:
threadDataAt: aName put: aValue
  "Returns aValue"
  | dict |
  aName _isOneByteString ifFalse:[
    ArgumentError signal:'argument is not a String or Symbol'.
  ].
  (dict := environment) ifNil:[ 
     dict := IdentityKeyValueDictionary new .
     environment := dict 
  ].
  dict at: aName asSymbol put: aValue .
  ^ aValue
%

method:
keys
 | dict |
 (dict := environment) ifNil:[ ^ { } ].
 ^ dict keysAsArray 
%

method:
includesKey: aName
  | dict |
  aName _isOneByteString ifFalse:[
    ArgumentError signal:'argument is not a String or Symbol'.
  ].
  (dict := environment) ifNil:[ ^ false ].
  ^ (dict at: aName asSymbol otherwise: nil) ~~ nil 
%


classmethod:
exit
  | sched nextProc curr |
  sched := self _scheduler .
  curr := self _current .
  [
    curr _terminationStarted ifFalse:[ 
      curr _setModeinfoBit: ModeInfo_terminationStarted .
      curr _executeEnsureBlocks
    ]. 
  ] onException: AbstractException do:[ :ex |
    "ignore exception"
  ].
  curr _finishTermination . 
  nextProc := sched _findReadyProcessForExit: curr .
  nextProc ifNil:[  "no other processes to run"
    RubySystemExit signal:'Thread.exit called' 
  ] ifNotNil:[
    sched _switchFrom: nil to: nextProc .
  ].
  self _uncontinuableError . "should not reach here"
%  

method:
exit
 | curr |
 curr := self _current .
 (self == curr or:[ self == GsProcess rubyMain]) ifTrue:[ ^ GsProcess exit ].

 self _terminationStarted ifTrue:[ ^ self ].
 
 self terminate .
%

method:
alive

^ self _isTerminated not
%
 
method:
rubyGroup
  "Return the process group(a RubyThreadGroup ) the receiver is in.
   Assign receiver to the default group if it has no group."

  | g |
  (g := group) ifNil:[ 
    g := RubyThreadGroup default .
    group := g
   ].
  ^ g
%

method:
_start
  "Called from C to start a new process. 
   NOTE, exception handling differs in extent0.dbf and extent0.ruby.dbf .
   This version of _start from extent0.ruby.dbf .

   This method will never return. Once the receiver completes it should
   find another process to run. 
   This method is preloaded during VM startup.  You must logout/login
   to have any changes to this method take effect."
  | res envId curr |
  (modeInfo bitAnd: ModeInfo_continuationMask) ~~ 0 ifTrue:[
    self error:'cannot start an instance that is a continuation'
  ].
  block ifNotNil:[
    res :=
    (envId := msgEnvironmentId) == 0 ifTrue:[ 
      [ 
        block valueWithArguments: args 
      ] onException: { RubyThrowException . RubyBreakException . RubySystemExit }
        do: [ :ex | 
               "unexpected Ruby exception in a Smalltalk GsProcess."
               ex copy signalNotTrappable  .  " ... to GCI"
               ex resume .  "for GciContinue"
       ].
    ] ifFalse:[ 
      self _setModeinfoBit: ModeInfo_isRubyThread .
      self _startRuby: envId
    ].
  ].
  "if this process resumed a continuation, then current process may
   no longer be self at this point."
  curr := self _terminateCurrentWithResult: res . "terminate currently running process"
  joiners ifNotNil:[ 
    curr == self ifTrue:[ self _signalJoiners ]
               ifFalse:[ self error:'cannot _signalJoiners after resuming continuation'].
  ].
  self _scheduler _runNextProcess .
%


method:
joinValue
  self _join: 1000000000 .
  ^ blockResult .
%

method:
_join: limitSeconds
  | cls res |
  res := self join: limitSeconds .
  res == nil ifTrue:[ ^ nil "timed out" ].
  res := blockResult .
  (cls := res class) _isExceptionClass ifTrue:[ 
    cls == CannotReturn ifTrue:[ 
      ThreadError signal:'return cannot be used to exit a Thread'
    ].
    res copy signal 
  ].
  ^ self
%

method:
rubyPriority: anInt

anInt _isSmallInteger ifFalse:[
    ArgumentError signal:'expected a Fixnum' .
].
self priority: anInt
%

method:
rubyStatus

  self == self _current ifTrue:[ ^ 'run' copy ].
  self _isTerminated ifTrue:[
    blockResult == _remoteNil ifTrue:[ ^ nil ].
    ^ false
  ].
  ^ 'sleep' copy
%

method:
rubyStopped
  ^ (self _current) ~~ self
%

classmethod
rubyBasicNew

^ self rubyBasicNew_stBaseClass: GsProcess
%


method:
rubyStart: argsArray  block: aBlock
  "a ruby primitive"
  | envId sched currProc |
  envId := 1"__callerEnvId" .
  (currProc := self _current) == self ifTrue:[
    InternalError new 
      details:'GsProcess>>rubyStart, inconsistent current process' ;
      signalNotTrappable
  ].
  aBlock ifNil:[ ThreadError signal:'no block given' ].
  self _init: aBlock args: argsArray env: envId 
              stackSerial: currProc  _stackSerialNum 
              forked:  ModeInfo_forked .
  (sched := self _scheduler) _scheduleNewProcess: self .
  sched yield .
  ^ self
%

method:
rubyRun
  self rubyResume .
  self _scheduler yield
%

method:
rubyResume
  | sched |
  (modeInfo bitAnd: ModeInfo_anyTermination) ~~ 0 ifTrue:[
    ThreadError signal:'cannot resume a terminated thread'
  ].
  sched := self _scheduler.
  sched _delayUnschedule: waitingOn .
  waitingOn := nil .
  joiners ifNotNil:[:j | j do: [:aDelay | sched _delayUnschedule: aDelay ]].
  joiners := nil .
  ^ self _activate
%

method:
rubyPriority
  ^ self priority - 15 .
%

method:
rubyPriority: anInt
  anInt _isSmallInteger ifFalse:[
    ArgumentError signal:'expected a Fixnum'
  ].
  self priority: anInt + 15
%
method:
clearIoSelectResult
  | res |
  res := ioSelectResult .
  ioSelectResult := nil .
  ^ res
%

category: 'Private Ruby Scheduling'
method:
_reapEvents
  waitingOn := nil . 
  self _scheduler _scheduleProcess: self
% 

method:
_setTerminated
  "for use in RubySystemExit processing only"

  self _setModeinfoBit: ModeInfo_terminated
%
method:
_checkIfDebuggable
  "Check to make sure the receiver is debuggable. Currently this
   means that it is in the debug or ready states.
   If it is not debuggable then raise an error."

  | status |

  status := self _statusString.
  (status = 'ready' or:[status = 'debug'  
      or:[ status = 'active' or:[ status = 'suspended']]]) ifFalse: [
    ImproperOperation new _number: 2376 ; reason: #rtErrGsProcessNotDebuggable;
        object: self ; signal
  ].
%

