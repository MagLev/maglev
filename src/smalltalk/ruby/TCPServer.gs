!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: TCPServer.gs 25988 2011-06-14 17:55:54Z stever $
!=========================================================================

set class TCPServer

removeallmethods 
removeallclassmethods 

category: 'Ruby support'

classmethod:
new: aHostName port: portName

  "Return a new blocking listening socket bound to specified interface and port.
   portName maybe the name of a service such as 'http', or can be  a port number."

  | sock queueLength portNum hostAddr status |
  queueLength := 10 . "assumed"

  portNum := self resolvePort: portName .
  hostAddr := self getHostAddressByName: aHostName .
  hostAddr ifNil:[
    ^ self signalSocketError:'getHostAddressByName failed'.
  ].
  sock := self new .

  status := sock bindTo: portNum toAddress: hostAddr .
  portNum ifNotNil:[
    status == portNum ifFalse:[ ^ self signalSocketError:'bind failed' ].
  ] ifNil:[
    status ifNil:[ ^ self signalSocketError:'bind to random port failed'].
  ].
  status := sock makeListener: queueLength .
  status ifNil:[ 
    ^ self signalSocketError:'unable to initiate listening on socket'
  ].
  sock rubyBlocking: true .
  ^ sock .
%

method:
speciesForAccept

  ^ TCPSocket
%
