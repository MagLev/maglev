!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: TCPServer.gs 22300 2009-09-22 20:04:28Z otisa $
!=========================================================================

set class UNIXServer

removeallmethods 
removeallclassmethods 

method:
speciesForAccept

  ^ UNIXSocket
%
method:
bind: aPath

  | res |
  res := self _twoArgPrim: 13 with: aPath with: nil .
  res == self ifFalse:[ self signalSocketError:'bind failed' ].
  ^ self
%

