!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
! File: RubySignal.gs
!   
!  Smalltalk methods for Module Signal
!=========================================================================

!--------------------------
set class RubySignal

run
 "remove all instance methods from a Module"
 { RubySignal } do:[:cls |
   cls persistentMethodDictForEnv: 0 put: GsMethodDictionary new .
   cls _categoriesForEnv: 0 put: GsMethodDictionary new .
 ].
 true
%

! DO NOT removeallclassmethods  
!  there should be NO smalltalk classmethods for RubySignal.


!---------------------------------------
category: 'Ruby support'
method:
_trapSignal: sigNumber ignore: ignoreArg block: aBlock

"sigNumber must be a SmallInteger, 
 ignoreArg is one of
    nil  install specified block
    1    install  SIG_IGN handler
    2    revert to the VM default for specified signal
 aBlock must be nil, an ExecBlock or RubyProc .
 Installs specified C signal handler in VM .
 Caller responsible for Adjusting contents of the
 Ruby IdentityHash named TrappedSignals to match the args.

 Returns previously installed aBlock or nil .
"
<primitive: 872>

self _primitiveFailed: #_trapSignal:ignore:block: 
     args: { sigNumber . ignoreArg . aBlock }
%
