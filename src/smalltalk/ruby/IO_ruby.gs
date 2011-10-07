!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: IO_ruby.gs 26114 2011-07-08 16:18:18Z stever $
!
!  additional methods  for IO to support Ruby 
!=========================================================================
set class IO

! adding additional methods , don't 'removeallmethods'

category: 'Ruby support'

method:
binmode

"Set the receiver to binary mode.  Has no effect on Unix."

^ self
%

method:
sync

  "GsFile and GsSocket do not buffer output in object memory"
  ^ true
%
 
method:
setSync: aBoolean

  "has no effect, 
   GsFile and GsSocket do not buffer output in object memory"

^ self
% 

method:
stat

"Returns an instance of GsFileStat describing the receiver."

^ GsFile fstat: fileDescriptor isLstat: false
%
method:
fcntl: op with: arg

"supported operations:  F_GETFD, F_GETFL, F_SETFL, FD_CLOEXEC.
  arguments must use the OS-dependent constants returned by
     RubySocket _socketOsConstants: 1
  F_GETFD returns value of   fileDescriptor instVar .
  Returns -1 if the arg (an Array) contains a return value,
    -2 if operation not supported, 
     0 if successful set operation,
    > 0 failed and is an errno returned.
"
<primitive: 848>
op _validateClass: SmallInteger .
arg _validateClass: Array .
self _primitiveFailed: #fcntl:with: args: { op . arg }
%
