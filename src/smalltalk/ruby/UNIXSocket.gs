!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!=========================================================================

set class UNIXSocket
removeallmethods
removeallclassmethods

category: 'Ruby support'

classmethod:
new
  | s |
  (s := self _basicNew) _zeroArgPrim: 27  "UNIX Stream socket" ;
     rubyBlocking: true .
  ^ s
%

classmethod:
pair 
  | fds a b | 
  fds := self _zeroArgClassPrim: 29 .  "socketpair"
  fds ifNil:[ self signalSocketError: 'socketpair failed' ].
  a := self _basicNew .
  a _twoArgPrim: 14 with: (fds at: 1) with: nil ;  "_setFd:"
     rubyBlocking: true .
  b := self _basicNew .
  b _twoArgPrim: 14 with: (fds at: 2) with: nil ;  "_setFd:"
      rubyBlocking: true .
  ^ { a . b }
%

method:
address
  | res |
  res := self _zeroArgPrim: 30 .  "SocketAddress AF_UNIX"
  res ifNil:[
    ^ self signalSocketError:'address failed'
  ].
  ^ res
%

method:
_peerPath
  "returns a String or nil. Sockets obtained from socketpair
   will return nil. "
  ^ self _zeroArgPrim: 31 . "SocketPeerAddress AF_UNIX"
%

method:
connect: aPath

"Connect the receiver to the UNIXServer socket identified by aPath.
 Returns true if the connection succeeded and false if not."

| status |
status := self _twoArgPrim: 16 with: aPath with: nil.
status == self ifTrue:[ ^ true ].
(status == false) ifTrue: [
  "connect call has blocked, wait for it to complete"
  self _waitForWriteReady .
  ^ self peerAddress ~~ nil .
].
^ false
%

