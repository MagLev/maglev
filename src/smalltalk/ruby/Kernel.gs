!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
! File: Kernel.gs
!   
!  Smalltalk methods for Module Kernel
!=========================================================================

!--------------------------
set class Kernel

run
 "remove all instance methods from a Module"
 Kernel persistentMethodDictForEnv: 0 put: GsMethodDictionary new .
 Kernel _categoriesForEnv: 0 put: GsMethodDictionary new .
 true
%

! DO NOT removeallclassmethods  , remember Kernel is an instance of Module
!  there should be NO smalltalk classmethods for Kernel.


!---------------------------------------
category: 'Ruby support'

! Kernel>>_system: moved to .mcz

method:
sprintf: formatString with: argsArray

"A ruby primitive.
 Ruby  printf per Pickaxe page 529..532"
<primitive: 767>
formatString _validateClass: String .
argsArray _validateClass: Array  .
self _primitiveFailed: #sprintf:with: args: { formatString . argsArray }
%

method:
catch1: aSymbol do: aBlock
  "catch:do: for env 1"
| sym | 
(sym := aSymbol) _isSymbol ifFalse:[
  aSymbol _isRubyString ifTrue:[ sym := aSymbol asSymbol ]
               ifFalse:[ ArgumentTypeError signal:'expected a Symbol or String'].
].
aBlock _isExecBlock ifFalse:[ 
  (aBlock ifNil:[ CannotReturn] ifNotNil:[ ArgumentTypeError]) signal:'expected a block'.
].
^ aBlock rescue1: RubyThrowException do:[ :ex | 
    ex name == sym ifTrue:[ 
      ex return: ex _value 
    ].
    ex pass 
  ]  
%
method:
catch2: aSymbol do: aBlock
  "catch:do: for env 2"
| sym | 
(sym := aSymbol) _isSymbol ifFalse:[
  aSymbol _isRubyString ifTrue:[ sym := aSymbol asSymbol ]
               ifFalse:[ ArgumentTypeError signal:'expected a Symbol or String'].
].
aBlock _isExecBlock ifFalse:[ 
  (aBlock ifNil:[ CannotReturn] ifNotNil:[ ArgumentTypeError]) signal:'expected a block'.
].
^ aBlock rescue2: RubyThrowException do:[ :ex | 
    ex name == sym ifTrue:[ 
      ex return: ex _value
    ].
    ex pass 
  ]  
%

method:
throw: aSymbol
| sym |
(sym := aSymbol) _isSymbol ifFalse:[
  aSymbol _isRubyString ifTrue:[ sym := aSymbol asSymbol ]
               ifFalse:[ ArgumentTypeError signal:'expected a Symbol or String'].
].
RubyThrowException new name: sym ; signal
%

method:
throw: aSymbol with: aValue
| sym |
(sym := aSymbol) _isSymbol ifFalse:[
  aSymbol _isRubyString ifTrue:[ sym := aSymbol asSymbol ]
               ifFalse:[ ArgumentTypeError signal:'expected a Symbol or String'].
].
RubyThrowException new name: sym value: aValue ; signal
%

method:
_lastDnuProtection

"Returns a SmallInteger 0..2 the method protection associated with 
 most recent ruby MNU, and clears that state in the VM."
<primitive: 787>

self _primitiveFailed: #_lastDnuProtection
%
method:
_waitForMilliseconds: millisecondCount
  "Suspends the active process for millisecondCount milliseconds.
   Returns number of milliseconds slept.
   millisecondCount==0 is equivalent to yield .
   "
  | sched proc nowMs interval |
  sched := GsProcess _scheduler .
  nowMs := sched _now .
  interval := millisecondCount .
  interval <= 10 ifTrue:[
    interval == 0 ifTrue:[
      " don't change priority if just yielding"
      sched _waitForMilliseconds: interval  .
      ^ sched _now - nowMs .
    ].
    interval <= 0 ifTrue:[
      ArgumentError signal:'sleep time must be positive'
    ].
    "don't sleep less than probable clock resolution, to avoid
    failing to yield."
    interval := 10 . 
  ].
  proc := sched activeProcess .
  sched _waitForMilliseconds: interval  .
  ^ sched _now - nowMs .
%

method:
_highPriorityWaitForMilliseconds: millisecondCount
  "Set the active process's priority high and then
   suspends the active process for millisecondCount milliseconds.
   Returns number of milliseconds slept.
   millisecondCount==0 is equivalent to yield .
   "
  | sched oldPrio proc nowMs interval |
  sched := GsProcess _scheduler .
  nowMs := sched _now .
  interval := millisecondCount .
  interval <= 10 ifTrue:[
    interval == 0 ifTrue:[
      " don't change priority if just yielding"
      sched _waitForMilliseconds: interval  .
      ^ sched _now - nowMs .
    ].
    interval <= 0 ifTrue:[
      ArgumentError signal:'sleep time must be positive'
    ].
    "don't sleep less than probable clock resolution, to avoid
    failing to yield."
    interval := 10 . 
  ].
  "raise priority so timeout will interrupt other thread running hot"
  proc := sched activeProcess .
  oldPrio := proc _raisePriority .
  sched _waitForMilliseconds: interval  .
  proc priority: oldPrio .
  ^ sched _now - nowMs .
%
method: 
selectRead: readIos write: writeIos error: errIos timeout: timeoutArr

 <primitive: 845> 
  "primitive adds the readIos,writeIos,errIos
   to the scheduler's poll info, and always fails.
   If any of the IOs is a GsFile, that IO will
   have been immediately added to the ioSelectResult.
   Each of readIos, writeIos, errIos must be nil or an Array of IOs.
   timeoutArr must be a one-element array containing nil, or a
   SmallInteger milliseconds to wait .
   Returns an Array of 3 Arrays, containing IOs ready for read,write
   or with errors.
"
 | proc sched res msToWait |
 sched := GsProcess _scheduler .
 proc := sched activeProcess .
 (res := proc clearIoSelectResult) ifNotNil:[ ^ res ].
 msToWait := timeoutArr at: 1 .  "Ruby code does arg kind checking"
 msToWait ifNil:[
   sched _waitForIoSelect  .  "wait forever until a socket is ready"
 ] ifNotNil:[
   sched _waitForIoSelect: msToWait
 ].
 "scheduler poll primitive 192 has filled in in proc.ioSelectResult"
 ^ proc clearIoSelectResult
%

method:
execv: commandString opcode: opcodeInt envVars: envArray fdDups: fdDupsArray args: arrayOfStrings

"A ruby primitive , used by Ruby Kernel.exec , Kernel.spawn

 opcode == 0 , exec , using execve() which
  terminates the current session to execute specified command.  
  This primitive will return
  or generate Smalltalk errors only in the event of malloc failures
  or invalid argument kinds.  Otherwise the primitive terminates
  Smalltalk/Ruby execution, disconnects from the stone process,
  and calls execv().  If execv() returns, error information is printed
  to stdout and this process exits with non-zero status. 

 opcode == 1 , spawn  using posix_spawn and return pid of the child
    or a negated errno value. (Not available on HPUX)

 envArray contains environment variables information, to modify
 the environment before execv() is called.
  envArray[0] is a boolean saying whether to call clearenv() (linux only)
  envArray[1..n] are name/value or name/nil  pairs to be used
  as args to setenv() or unsetenv() .

  fdDupsArray is an array of file descriptor pairs to be used to call dup2()
  before execv() is called.   See Ruby documentation of Kernel.exec for
  more details.
"
<primitive: 890>
self _primitiveFailed: #execv:opcode:envVars:fdDups:args: 
     args: { commandString . opcodeInt . envArray . fdDupsArray . arrayOfStrings }

%

