!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
!  file  RubySocket.gs ,  Smalltalk implementation of methods from
!    Ruby  BasicSocket and   Ruby  Socket .
!
!  Smalltalk RubySocket is a subclass of GsSocket .
!  Ruby Socket will be identically Smalltalk RubySocket .
!=========================================================================

set class RubySocket

! remove disallowed smalltalk behavior
removeallmethods
removeallclassmethods

category: 'Ruby support'

classmethod:
_basicNew
  ^ self rubyNewCFinalizer_stBaseClass: RubySocket
%

classmethod:
_newSocketForFd: anInteger
  ^ self _basicNew
         _twoArgPrim: 14 with: anInteger with: nil; "_setFd:"
         rubyBlocking: true;
         yourself
%

classmethod:
_socketOsConstants: kind

"Returns an Array of String, SmallInteger pairs 
 describing values of OS dependent contants needed by Ruby 
 initialization code in MagLev.mcz 

  kind == 0 ,  Socket constants
  kind == 1 ,  Fcntl constants
  kind == 2 ,  os signal name constants for maglev-ruby 
  kind == 3 ,  os signal name constants for  maglev-ruby -d
  kind == 4 ,  Process constants used by waitpid
"

<primitive: 801>
self _primitiveFailed: #_socketOsConstants: args: { kind }
%

classmethod:
setNoReverseLookup: aBoolean
  "has no effect"
  ^ self 
%

classmethod:
gethostbyname: aString

  | ipAddr |
  ipAddr := self getHostAddressByName: aString .
  ipAddr ifNil:[ ^ self signalSocketError:'host not found' ].
  ^ { ipAddr . 
       nil "aliases not implemented" . 
       2    "assume AF_INET" . 
       nil  "sockaddr details not implemented" }
%

classmethod:
getservbyname: aString protocol: protoString
  "Returns a portNumber of signals an error"
  | port |
  port := self getServicePortByName: aString withProtocol: protoString .
  port ifNil:[ ^ self signalSocketError:'service not found' ]. 
  ^ port
%

classmethod:
_getaddrinfo: args

"args must be an Array of size 7. 
 Result is an Error string, or an Array "
<primitive: 867>

self _primitiveFailed: #_getaddrinfo: args: { args }
%


classmethod:
new: aDomain type: aType proto: aProtocol

  "Returns a SmallInteger errno, or a new non-blocking Socket"

  aProtocol == 0 ifFalse:[ ArgumentTypeError signal:'protocol must be zero'].
  ^ self _basicNew _twoArgPrim: 26 with: aDomain with: aType 
%

classmethod:
signalSocketError: errText

  | errSym exCls ofs |
  [ ofs := self _zeroArgClassPrim: 19 .  "_lastErrorSymbolOffset"
    exCls := SocketErrorClasses at: ofs .
  ] onSynchronous: Error do:[ :ex |
    ex return .
  ].
  exCls ifNil:[
    errSym := self lastErrorSymbol .
    errSym ifNil:[
      ^ SocketError signal: 'SocketError_unknown ' , errText
    ].
    errSym = #NotConnected "Fix github #97"
      ifTrue: [| errno |
        errno := (Exception errnoTables at: AbstractException cpuOsKind)
                   indexOf: 'ECONNREFUSED'.
        ^ (SystemCallError @ruby1:new: errText value: errno) signal].
    ^ SocketError signal: 'SocketError_' , errSym , ' ' , errText
  ].
  ^ exCls signal: exCls name , ' ' , errText
% 
method:
signalSocketError: errText

  | errSym exCls ofs |
  [ ofs := self _zeroArgPrim: 22 .  "_lastErrorSymbolOffset"
    exCls := SocketErrorClasses at: ofs .
  ] onSynchronous: Error do:[ :ex |
    ex return .
  ].
  exCls ifNil:[
    errSym := self lastErrorSymbol .
    errSym ifNil:[
      ^ SocketError signal: 'SocketError_unknown ' , errText
    ].
    errSym = #NotConnected "Fix github #97"
      ifTrue: [| errno |
        errno := (Exception errnoTables at: AbstractException cpuOsKind)
                   indexOf: 'ECONNREFUSED'.
        ^ (SystemCallError @ruby1:new: errText value: errno) signal ].
    ^ SocketError signal: 'SocketError_' , errSym , ' ' , errText
  ].
  ^ exCls signal: exCls name , ' ' , errText
% 

classmethod:
signalSocketError
  ^ self signalSocketError: ''
%

method:
signalSocketError
  ^ self signalSocketError: ''
%

method:
rubyBlocking
  ^ isRubyBlocking
%
method:
rubyBlocking: aBoolean
  isRubyBlocking := aBoolean
%

method:
getsockopt: aLevel name: nameId
"invoked from Ruby primitive. Result is an Array,
 or a SmallInteger errno value"
<primitive: 802>
aLevel _validateClass: SmallInteger .
nameId _validateClass: SmallInteger .
self _primitiveFailed: #getsockopt:name: args: { aLevel . nameId }
%

method:
setsockopt: aLevel name: nameId value: aValue
"invoked from Ruby primitive .
 Result is a SmallInteger , 0 if successful, otherwise an errno"

<primitive: 803>
aLevel _validateClass: SmallInteger .
nameId _validateClass: SmallInteger .
aValue _validateClasses: { SmallInteger . Array }.
self _primitiveFailed: #setsockopt:name:value:  
     args: { aLevel . nameId . aValue }
%

method:
_CSocketBlocking: aBoolean
  self option: 'NONBLOCKING' put: aBoolean not 
%

method:
_CSocketIsBlocking
  ^ (self option:'NONBLOCKING') not 
%

method:
makeListener: queueLength
  | status |
  status := super makeListener: queueLength .
  status ifNil:[ 
    ^ self signalSocketError:'unable to initiate listening on socket'
  ].
  ^ self
%

method:
recv: aLength

"Receive up to aLength bytes from the receiver.  
 Returns a String containing the bytes read.
 Implementation for Ruby  recv , sysread  methods."

| res buf nAvail bSize |
(buf := readBuffer) ifNotNil:[ | ofs |
  ofs := bufferOffset  .
  bSize := buf size .
  nAvail := bSize - ofs + 1 .
  nAvail <= aLength ifTrue:[
    ofs == 1 ifTrue:[ res := buf ]
            ifFalse:[ res := buf copyFrom: ofs to: bSize ].
    readBuffer := nil .
    bufferOffset := 1 .
  ] ifFalse:[ | nextOfs |
    nextOfs := ofs + aLength .
    res := buf copyFrom: ofs to: nextOfs - 1 .
    bufferOffset := nextOfs 
  ]
] ifNil:[ | status |
  res := String new .
  [
    status := self _readInto: res startingAt: 1 maxBytes: aLength .
    status == false ifTrue:[ "no data available on non-blocking Smalltalk socket"
      isRubyBlocking ifTrue:[
        status := self _waitForReadReady
      ] ifFalse:[
        SocketErrorEAGAIN signal:'no data available on non-blocking socket' 
      ]
    ] "ifFalse:[ if status==true here, we get EINTR an must retry]" .
    status == true
  ] whileTrue .
  status == 0 ifTrue:[ ^ nil . "socket closed, EOF" ].
  status ifNil:[ ^ self signalSocketError ]
].
^ res
%

method:
ungetByte: byteVal
  "push byteVal back onto the read buffer"
  | buf ofs |
  (buf := readBuffer) ifNil:[
    buf := String new: 1 .
    readBuffer := buf .
    bufferOffset := 1 .
    ofs := 1 .
  ] ifNotNil:[ 
    ofs := bufferOffset .
    ofs == 1 ifTrue:[
      buf insertAll: (Character withValue: 0) at: 1
    ] ifFalse:[  "must have ofs > 1"
      ofs := ofs - 1 .
      bufferOffset := ofs . 
    ]
  ].
  buf codePointAt: ofs put: byteVal .
%

method:
_clearReadBuffer
    "used by Ruby  IO#rewind"
  readBuffer := nil .
  bufferOffset := 1 
%

method:
_basicRecv
 "used to receive more data when a _peek or _getLine
  discovers empty buffer"
 | buf |
 buf := self recv: 1500 "typical network MTU" . 
 buf ifNil:[
    ^ nil  "socket eof or error" 
 ].
 bufferOffset := 1 .
 readBuffer := buf .
 ^ buf
%

method:
_peek
  "returns the first available byte without consuming it,
   or nil if socket is at EOF."
  | buf |
buf := readBuffer .
buf ifNil:[
  buf := self _basicRecv .
  buf ifNil:[
    ^ nil  "socket eof or error"
  ].
].
^ buf byteAt: bufferOffset
%

! edited for  Trac 670
method:
_getLine: eolValue

| startOfs buf searchOfs eolSize eolStr |
(buf := readBuffer) ifNil:[
  buf := self _basicRecv .
  buf ifNil:[ 
    ^ nil  "socket eof or error"
  ].
].
eolValue _isSmallInteger ifTrue:[
  (eolValue < 0 or:[ eolValue > 255]) ifTrue:[
    ArgumentError signal:'eolValue must be a String or ascii value 0..255'.
  ].
  eolStr := String new: 1 .
  eolStr at: 1 put: (Character withValue: eolValue) .
  eolSize := 1 .
] ifFalse:[
  eolValue _isOneByteString ifFalse:[
    ArgumentError signal:'eolValue must be a String or ascii value 0..255'
  ].
  eolStr := eolValue .
  ((eolSize := eolStr size) < 1 or:[ eolSize > 128]) ifTrue:[
     ArgumentError signal:'eolValue String size must be in range 1..128'
  ]. 
].
startOfs := bufferOffset .
searchOfs := startOfs .
[ true ] whileTrue:[ | moreData ofs ofsPlusEol |
  ofs := buf _findString: eolStr startingAt: searchOfs ignoreCase: false.
  ofs ~~ 0 ifTrue:[ | res ofs_plus_eol |
    (ofsPlusEol := ofs + eolSize) < buf size ifTrue:[
      res := buf copyFrom:startOfs to: (ofsPlusEol - 1) . "including terminator"
      bufferOffset := ofsPlusEol .
      readBuffer := buf .
    ] ifFalse:[
      startOfs == 1 ifTrue:[
        res := buf .
      ] ifFalse:[
	res := buf copyFrom:startOfs to: (ofsPlusEol - 1). "including terminator"
      ].
      readBuffer := nil .
      bufferOffset := 1 . 
    ].
    ^ res
  ].
  readBuffer := nil .
  bufferOffset := 1 .
  moreData := self _basicRecv .
  moreData ifNil:[ 
    ^ nil  "socket eof or error"
  ]. 
  searchOfs := buf size - eolSize + 1 . 
  buf addAll: moreData .
].
%

method:
getByte
  | buf ofs res |
  buf := readBuffer .
  buf ifNil:[
    buf := self _basicRecv .
    buf ifNil:[
      ^ nil  "socket eof or error"
    ].
  ].
  res := buf byteAt: (ofs := bufferOffset) .
  ofs := ofs + 1 .
  ofs > buf size ifTrue:[
    readBuffer := nil .
    bufferOffset := 1 .
  ] ifFalse:[
    bufferOffset := ofs .
  ].
  ^ res
%
  
method:
getLine: eolValue
  eolValue ifNil:[ ^ self recv: 4096 ].
  ^ self _getLine: eolValue .
%

! deleted zero arg RubySocket>>gets

method:
syswrite: aString
  "Result is number of bytes written, may write less than amount requested,
   and if receiver is a ruby non-blocking socket, may signal SocketErrorEAGAIN"
  | status |
  [
    status := self _write: aString startingAt: 1 ofSize: aString size .
    (status == false) ifTrue: [ "would have blocked"
      isRubyBlocking ifTrue:[
        status := self _waitForWriteReady
      ] ifFalse:[
        SocketErrorEAGAIN signal:'socket is blocked on output' 
      ]
    ] "ifFalse:[ if res==true on this path, socket write got EINTR and we retry ] ".
    status == true  
  ] whileTrue .
  status ifNil:[  ^ self signalSocketError ].
  ^ status "return number of bytes written"
%

method:
write: aString

 "Write to the receiver. Returns number of byte written.
  Waits for socket to accept data ; never signals SocketErrorEAGAIN"

 | offset numToWrite aSize |
 offset := 1.
 numToWrite := (aSize := aString size) .
 [ true ] whileTrue:[ | result |
   [
     result := self _write: aString startingAt: offset ofSize: numToWrite .
     result == true 
   ] whileTrue . "loop to handle EINTR "
 
   result _isSmallInteger ifTrue:[
     numToWrite  := numToWrite - result .
     (numToWrite <= 0) ifTrue: [ ^ aSize ]. "All done"
     offset := offset + result .
   ] ifFalse: [
     result == false ifTrue:[  "non-blocking socket would have blocked."
       self _waitForWriteReady . 
     ] ifFalse:[
       result ifNil: [ ^ self signalSocketError].
     ].
   ].
 ].
%


method:
shutdown: howInt

  howInt == 2 ifFalse:[ self error:'closing just in or out not supported' ].
  self close .  "close returns nil if error occurred, not checked"
  ^ 0
%

method:
shutdown
  self close .  "close returns nil if error occurred, not checked"
  ^ 0.
%


method:
read: aLength into: destString minLength: aMin

  "Receive at least aMin and at most aLength bytes , from receiver's socket into 
   buffer destString.  Size of destString is adjusted to match actual length read
   Raises EOFError if end of file is reached."

| res buf total |
destString ifNil:[ res := String new: aLength ] 
        ifNotNil:[ res := destString . res size: aLength ].
(buf := readBuffer) ifNotNil:[ | ofs bSize nAvail |
  ofs := bufferOffset  .
  bSize := buf size .
  nAvail := bSize - ofs + 1 .
  nAvail <= aLength ifTrue:[
    res replaceFrom: 1 to: 1 + bSize - ofs with: buf startingAt: ofs .
    readBuffer := nil .
    bufferOffset := 1 .
  ].
  nAvail >= aLength ifTrue:[
    nAvail == aLength ifFalse:[ | nextOfs |
      nextOfs := ofs + aLength .
      res replaceFrom: 1 to: nextOfs - ofs with: buf startingAt: ofs .
      bufferOffset := nextOfs .
    ].
    ^ res
  ].
  total := nAvail .
] ifNil:[
  total := 0 .
].
[ total < aMin ] whileTrue:[ | status |
  [ status :=  self _readInto: res startingAt: total + 1 maxBytes: aLength - total.
    (status == false) ifTrue: [
      isRubyBlocking ifTrue:[
        status := self _waitForReadReady
      ] ifFalse:[
        SocketErrorEAGAIN signal:'no data available to read from the socket'  .
        ^ self signalSocketError
      ].
    ] "ifFalse:[ if status==true here, we get EINTR an must retry]" .
    status == true 
  ] whileTrue .
  status == 0 ifTrue:[
    total == 0 ifTrue:[ ^ nil "eof" ].
    res size: total .
    ^ res  
  ].
  status ifNil:[ ^ self signalSocketError ].
  total := total + status .
].
res size: total .
^ res
%
method: RubySocket
accept

"Accept a client request for a connection on the receiver. 
 Returns a new socket with isRubyBlocking==true. 
 Will wait for clients, or signal SocketErrorEAGAIN per the
 isRubyBlocking state of the receiver."
   
| aSocket status |
aSocket := self speciesForAccept _basicNew .
status := true.
[ status == true or:[ status == false]] whileTrue: [
  status := self _twoArgPrim: 4 with: aSocket with: nil .
  (status == false) ifTrue: [
    "socket is non-blocking and would have blocked, process scheduler will wait"
    isRubyBlocking ifTrue:[
      status := self _waitForReadReady .
    ] ifFalse:[
      SocketErrorEAGAIN signal:'in accept, no clients attempting to connect'  .
      ^ self signalSocketError
    ]
  ] "ifFalse:[ if C accept got EINTR , status==true at this point]" .
].
status ifNil:[ ^ self signalSocketError ].
aSocket rubyBlocking: true .
^ aSocket 
%
method: RubySocket
sysAccept

"Accept a client request for a connection on the receiver. 
 Returns Array  of [new socket, peerAddrInfoString] .
 The new socket has isRubyBlocking==true. 
 Will wait for clients, or signal SocketErrorEAGAIN per the
 isRubyBlocking state of the receiver."
   
| aSocket status peerAddr |
aSocket := self speciesForAccept _basicNew .
status := true.
peerAddr := String new .
[ status == true or:[ status == false]] whileTrue: [
  "prim fills in peerAddr with string form of peer's sockaddr, if
   accept succeeds "
  status := self _twoArgPrim: 4 with: aSocket with: peerAddr .
  (status == false) ifTrue: [
    "socket is non-blocking and would have blocked, process scheduler will wait"
    isRubyBlocking ifTrue:[
      status := self _waitForReadReady .
    ] ifFalse:[
      SocketErrorEAGAIN signal:'in accept, no clients attempting to connect'  .
      ^ self signalSocketError
    ]
  ] "ifFalse:[ if C accept got EINTR , status==true at this point]" .
].
status ifNil:[ ^ self signalSocketError ].
aSocket rubyBlocking: true .
^ { aSocket . peerAddr }
%

method:
rubyConnect: addrString

"addrString is a String containing a struct sockaddr
  such as from a method like Socket#sockaddr_in"

| status |
status := self _twoArgPrim: 23 with: addrString with: nil.
status == self ifTrue:[ ^ self ].
(status == false) ifTrue: [
  "connect call has blocked"
  isRubyBlocking ifTrue:[
    self _waitForWriteReady . "wait for connect to complete"
    self peerAddress ifNotNil:[ ^ self ].
  ] ifFalse:[
    SocketErrorEAGAIN signal:'connect would block' .
    ^ self signalSocketError
  ].
].
^ status "a SmallInteger errno"
%

method: 
recvfrom: maxBytes flags: flagsInt

 "Returns an Array of the form [ aString , senderInfoArray ] "

 flagsInt == 0 ifFalse:[ ArgumentError signal:'non-zero flags arg not supported yet'].
 ^ super recvfrom: maxBytes 
%

