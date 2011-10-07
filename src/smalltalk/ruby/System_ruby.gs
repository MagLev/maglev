!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!=========================================================================

set class System

!  additional methods  for System to support Ruby 

category: 'Ruby Support'
classmethod:
enableContinuations

"Enables Smalltalk (i.e. Seaside style) continuations. 
 Continuations are enabled by default at the start of a new session."

^ self _zeroArgPrim: 115
%

classmethod:
disableContinuations

"Disables Smalltalk (i.e. Seaside style) continuations."

^ self _zeroArgPrim: 116
%

classmethod:
_processInfo: opcode with: intOne with: intTwo
"opcode 0  getuid  
        1  geteuid
        2  getgid
	3  getegid 
        4  setuid(uid)
        5  setegid(egid)
        6  seteuid(euid)
        7  setgid(gid)
	8  getpid
	9  getpgrp
       10  getppid
       11  getpgid(pid)
       12  kill(signal, pid)   (pid < 0 , pid==getpid() not supported)
 method result is a SmallInteger, >=0 for success 
  or the negated value of C errno "
<primitive: 764>
opcode _validateClass: SmallInteger .
intOne ifNotNil:[ intOne _validateClass: SmallInteger] .
intTwo ifNotNil:[ intTwo _validateClass: SmallInteger] .
self _primitiveFailed: #_processInfo:with:with:
% 

classmethod:
_gemProcessId

"Return result of calling getpid()"

^ self _processInfo: 8 with: nil with: nil
%

classmethod:
_sessionTempsAt: aSymbol

  ^ SessionTemps current at: aSymbol otherwise: nil
%

classmethod:
_sessionTempsAt: aSymbol put: aValue

  SessionTemps current at: aSymbol  put: aValue .
  ^ aValue
%
classmethod:
__system: commandStr

"Invoke section 3 system() .
 Returns an Array { rawStatus . childStatus . childCompletedBool . 
			errMsg. errno } "

<primitive: 904>
self _primitiveFailed: #__system:
%

classmethod:
_waitpid: pidInt flags: flagsInt
  "Calls waitpid() .  
   Returns { pid . status } , or a SmallInteger errno "
<primitive: 889>

self _primitiveFailed: #_waitpid:flags: args: {  pidInt . flagsInt }
%
