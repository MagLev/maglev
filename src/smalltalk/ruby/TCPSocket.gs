!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: TCPSocket.gs 26203 2011-07-20 17:20:49Z otisa $
!=========================================================================

set class TCPSocket

removeallmethods 
removeallclassmethods 

category: 'Ruby support'

classmethod:
new: aHostName port: portName

  "blocking connection attempt to portName on aHostName .
   The scheduler will allow other threads  to run of the connect blocks.
   The result will be a non-blocking Smalltalk socket, with
   isRubyBlocking==true , or nil if connection failed.
   portName maybe the name of a service such as 'http', or can be  a port number."
 
  | sock |
  sock := self new .
  sock rubyConnectTo: portName on: aHostName . "uses getaddrinfo"
  sock rubyBlocking: true .
  ^ sock .
%  
method:
rubyConnectTo: portName on: hostName
  "portName may also be a number"
  | status |
  status := self connectTo: portName on: hostName . "uses getaddrinfo"
  status ifFalse:[ | portNum hostAddr errno |
    errno := self lastErrorCode .
    portNum := self class resolvePort: portName .
    portNum ifNil:[ ^ self signalSocketError:'port name not found' ].
    hostAddr := self class getHostAddressByName: hostName .
    hostAddr ifNil:[
      ^ self signalSocketError:'getHostAddressByName failed'.
    ].
    ^ errno 
  ].
  ^ self
%

classmethod:
open: aHostName 
  "connection attempt to random port on aHostName .
   The scheduler will allow other threads  to run of the connect blocks.
   The result will be a non-blocking Smalltalk socket, with
   isRubyBlocking==true . "

  | portNum sock |
  sock := self new .
  portNum := sock bindTo: nil . "get a random port"
  portNum ifNil:[ self signalSocketError:'bind failed' ].
  ^ sock rubyConnectTo: portNum on: aHostName   "now use getaddrinfo"
%

classmethod:
resolvePort: portName

  "portName can be nil to specify a random port (in which case result is nil),  
   or can the name of a service such as 'http', or can be  a port number.

   Returns nil or a port number."

  | portNum |
  portName ~~ nil ifTrue:[
    (portName _isOneByteString) ifTrue:[
      portNum := self getServicePortByName: portName .
      portNum == nil ifTrue:[
        "portName is string, but not a service name; try as int"
         [ portNum := portName asInteger ] onSynchronous: AbstractException do:[ :ex|
             ^ self signalSocketError:'port not found' 
         ]
      ]
    ] ifFalse:[
       portName _isSmallInteger ifTrue: [ portNum := portName ]
    ].
    "At this point, portNum is either a number, or nil"
    (portNum _isSmallInteger and:[ portNum <= 0 or:[ portNum >= 65535]]) ifTrue:[
      ^ self signalSocketError:'invalid portName argument' 
    ].
  ].
  ^ portNum
%
