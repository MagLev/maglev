!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!=========================================================================

set class UDPSocket
removeallmethods
removeallclassmethods

category: 'Ruby support'

classmethod:
new
  ^ self _basicNew _zeroArgPrim: 26  "UDP socket" 
%

method:
bind: aHostName port: aPort

  | host portNum |
  aHostName = '' 
    ifTrue:[ host := nil  "INADDR_ANY" ]
    ifFalse:[ aHostName = '<broadcast>' 
       ifTrue:[ host := -1 ]
       ifFalse:[ host := GsSocket getHostAddressByName: aHostName .
		 host ifNil:[ ^ self signalSocketError:'getHostAddressByName failed']]].

  portNum := self bindTo: aPort toAddress: host .
  portNum ifNil:[ self signalSocketError:'bind failed' ].
  ^ self
%

method:
connectTo: portNumber on: aHostName

  | host ok |
  aHostName = ''
    ifTrue:[ host := nil  "INADDR_ANY" ]
    ifFalse:[ aHostName = '<broadcast>'
       ifTrue:[ host := -1 ]
       ifFalse:[ host := aHostName ]].
  ok := super connectTo: portNumber on: host .
  ok ifFalse:[ self signalSocketError:'connect failed' ].
  ^ self
%


! _send:...port: separate from GsSocket because of isRubyBlocking logic
method:
_send: aString flags: flagsInt host: hostName port: portArray

  | portNum idx strSize portStr |
  portNum := portArray atOrNil: 1 .
  portNum _isSmallInteger ifTrue:[ 
    portStr := portNum asString .
  ] ifFalse:[
    portNum _isOneByteString ifFalse:[ ArgumentTypeError signal:'port must be a String or Fixnum'].
    portStr := portNum .
  ].
  hostName _isOneByteString ifFalse:[ ArgumentTypeError signal:'hostname must be a String'].
  flagsInt == 0 ifFalse:[ ArgumentError signal:'non-zero flags arg not supported yet'].
  aString _isOneByteString ifFalse:[ ArgumentTypeError signal:'first arg(data to send) must be a String'].
  strSize := aString size .
  idx := 1 .
  [ true ] whileTrue:[ | result |
    [ result := self _send: aString startingAt: idx to: hostName port: portStr .
      result == true 
    ] whileTrue . "loop to handle EINTR"
    result _isSmallInteger ifTrue:[
      idx := idx + result .
      idx > strSize ifTrue:[ ^ strSize "done" ]
          ifFalse:[ result == 0 ifTrue:[ SocketError signal:'sendto infinite loop']].
    ] ifFalse:[
      result == false ifTrue:[  "non-blocking socket would have blocked."
        isRubyBlocking ifTrue:[
          result := self _waitForReadReady
        ] ifFalse:[
          SocketErrorEAGAIN signal:'no data available on non-blocking socket'
        ].
      ] ifFalse:[
        SocketError signal:'sendto failed, ', result asString
      ].
   ] 
 ]
%  

method:
_send: aString startingAt: anOffset to: hostName port: portNum

"Returns a SmallInteger number of bytes written, 
  or false to indicate EWOULDBLOCK,
  or true to indicate EINTR, 
  or a String error message."
 
<primitive: 883>
self _primitiveFailed: #_send:to:port:
     args: { aString . anOffset . hostName . portNum }
%


