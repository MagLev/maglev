!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: Classes_ruby.gs 26114 2011-07-08 16:18:18Z stever $
!
!  additional classes created during slowrubyimage filein
!=========================================================================

! RubyHash created in bom.c now

category: 'Instance creation'
classmethod: Module
_newModule
  "returns a new instance of receiver, with name == #'' and empty method dict."
  <primitive: 771>
  self _primitiveFailed: #_newModule   "receiver was not a subclass of Module"
%

expectvalue %String
run
  "Create the  Module named Kernel"
 | knam |
 knam := #Kernel .
 Globals at: knam ifAbsent:[
   | mod |
   (mod := Module _newModule ) name: knam .
   Globals at: knam put: mod  .
   (Globals associationAt: knam ) immediateInvariant .
   ^ 'created Kernel'
 ].
 ^ 'already exists'
%

expectvalue %String
run
  "Create the Module named RubySignal"
 | knam |
 knam := #RubySignal .
 Globals at: knam ifAbsent:[
   | mod |
   (mod := Module _newModule ) name: knam .
   Globals at: knam put: mod  .
   (Globals associationAt: knam ) immediateInvariant .
   ^ 'created RubySignal'
 ].
 ^ 'already exists'
%

! deleted  RubyNoArgument


! Range created in bom.c
expectvalue %String
run
Range definition
%

!   MatchData and Regexp created in bom.c . 
!    Regexp needs a Reserved oop, to allow auto-finalization of C data .
!    Both need C access to instVar offsets .
expectvalue %String
run
Regexp definition
%
expectvalue %String
run
MatchData definition
%
run
 { Regexp . MatchData } do:[:cls |
     cls removeAllMethods.
     cls class removeAllMethods
  ].
  true
%

!----------------------------------------------
!  Socket classes in standard Ruby include
!    IO
!      BasicSocket
!       Socket
!	  IPSocket
!           TCPSocket
!             TCPServer
!           UDPSocket
!         UNIXSocket
!           UNIXServer

!
! For now, use this hierarchy:
!
!  IO
!    GsSocket( existing Smalltalk class) (== Ruby BasicSocket)
!      RubySocket                        (== Ruby Socket)
!        IPSocket
!          TCPSocket
!             TCPServer

!  RubySocket created in bom.c now
expectvalue %String
run
 RubySocket _newKernelSubclass: 'IPSocket'
      instVarNames:#( ) 
      inDictionary: Globals 
      
%
expectvalue %String
run
 IPSocket _newKernelSubclass: 'TCPSocket'
      instVarNames:#() 
      inDictionary: Globals 
      
%
expectvalue %String
run
 TCPSocket _newKernelSubclass: 'TCPServer'
      instVarNames:#() 
      inDictionary: Globals 
      
%
expectvalue %String
run
 IPSocket _newKernelSubclass: 'UDPSocket'
      instVarNames:#() 
      inDictionary: Globals 
      
%
expectvalue %String
run
 RubySocket _newKernelSubclass: 'UNIXSocket'
      instVarNames:#( ) 
      inDictionary: Globals 
      
%
expectvalue %String
run
 UNIXSocket _newKernelSubclass: 'UNIXServer'
      instVarNames:#( )
      inDictionary: Globals
%

run
 { RubySocket . IPSocket . TCPSocket . TCPServer . UDPSocket . 
   UNIXSocket . UNIXServer } do:[:cls |
     cls removeAllMethods.
     cls class removeAllMethods
  ].
  true
%

!-----------------------------------------
!  subclasses of SocketError

limit bytes 10000 
expectvalue %String
run
| res |
res := String new .
{ 'SocketErrorEBADF' . 
   'SocketErrorENOTCONN' . 
   'SocketErrorEPIPE' . 
   'SocketErrorECONNRESET' . 
   'SocketErrorEAGAIN' 
 } do:[ :str |
 res addAll: ( SocketError _newKernelSubclass: str
      		instVarNames: #( )
    		inDictionary: Globals
    		 ) .
 res add: Character lf .
].
^ res
%

limit bytes 3000

run
RubySocket _addClassVar:#SocketErrorClasses value: { } .
true
%

classmethod: RubySocket
initializeErrorClasses

  | numErrs |
  SocketErrorClasses size: (numErrs := SocketErrorSymbols size) .
  1 to: numErrs do:[:j|
    SocketErrorClasses at: j put: SocketError .
  ].
  { { #NotSocket . SocketErrorEBADF } . 
     { #NotConnected . SocketErrorENOTCONN } . 
     { #BrokenPipe . SocketErrorEPIPE } . 
     { #ConnReset . SocketErrorECONNRESET } . 
     { #WouldBlock .  SocketErrorEAGAIN }

     "TODO add more specific SocketError mappings here. 
      errno values mapped to a SYSERR_ constant in unixerr.c .
      GsSocket(C)>>_initSocketErrorSymbols  maps a SYSERR_ constant 
      to a element of  SocketErrorSymbols . "

   } do:[ :descr | | sym errCls ofs |
     sym := descr at: 1 .
     errCls := descr at: 2 .
     ofs := SocketErrorSymbols indexOfIdentical: sym .
     SocketErrorClasses at: ofs put: errCls .
   ]
%
run
RubySocket initializeErrorClasses .
true
%

!-------------------------------
!  Exception classes needed in both Ruby and Smalltalk code

! RubyBreakException created in bom.c


expectvalue %String
run
Exception _newKernelSubclass: 'RubySystemExit'
                instVarNames: #( status runAtExitHandlers )
                inDictionary: Globals
                 
%
expectvalue %String
run
Exception _newKernelSubclass: 'RubyScriptError'
                instVarNames: #( )
                inDictionary: Globals
                 
%
expectvalue %String
run
RubyScriptError _newKernelSubclass: 'RubyNotImplementedError'
                instVarNames: #( )
                inDictionary: Globals
                 
%
expectvalue %String
run
OffsetError _newKernelSubclass: 'RubyStopIterationError'
                instVarNames: #( )
                inDictionary: Globals
                
%
expectvalue %String
run
ImproperOperation _newKernelSubclass:'RubyRuntimeError'
                instVarNames: #( )
                inDictionary: Globals
%
expectvalue %String
run
IOError _newKernelSubclass:'EOFError'
  instVarNames: #( )
                inDictionary: Globals
%



! RubyLoadError, RubyParseError defined in .mcz

!-------------------------------------------------------------
expectvalue %String
run
Object  _newKernelSubclass:'RubyDirectory'  "not a reserved oop yet"
  instVarNames: #( entries path index closed range )
  inDictionary: Globals
  constraints: { { #entries . Array } }
% 


expectvalue %String
run
RubyHash  _newKernelSubclass:'RubyEnv'  "not a reserved oop yet"
  instVarNames: #( )
  inDictionary: Globals
  
% 

! RubyThrowException created in bom.c

expectvalue %String
run
Object  _newKernelSubclass:'RubyThreadGroup'  "not a reserved oop yet"
  instVarNames: #( closed )
  inDictionary: Globals
  constraints: { { #closed . Boolean } }
%

!  RubyConstantRef defined in bom.c 

expectvalue %String
run
RubyHash _newKernelSubclass:'RubyIdentityHash' "not a reserved oop"
  instVarNames: #()
  inDictionary: Globals
  
%
!-------------------------------------------------------------
!  AST classes/methods to filein as Systemuser
run
" new class,  RubyParser:: in C code maps to RubyParserM for now"
(Globals includesKey: #RubyParserM) ifFalse:[
  Object subclass: #RubyParserM
     instVarNames: #( )
     classVars: #()
     classInstVars: #()
     poolDictionaries: #()
     inDictionary: Globals
     options: #()
].
true
%
classmethod: RubyParserM
_rbCompileError: aString isWarning: warnBool

"Returns true if parser prim 903 active on stack and
 aString was saved in parser state. 
 Returns false if parser prim 903 not active, in which case
 caller should signal an exception."

<primitive: 902>
aString _validateClass: String .
warnBool _validateClass: Boolean .
self _primitiveFailed: #_rbCompileError:isWarning: args: { aString . warnBool }
%
classmethod: RubyParserM
parse: sourceString cBytes: sourceCByteArray line: lineNum 
	file: fileName yTrace: yTraceLevel warnings: warnBool
        evalScope: aRubyEvalScope

"Returns a kind of RubyNode (root node of an AST tree) ,
 or a String describing a syntax error"
<primitive: 903>
sourceString class == String ifFalse:[
  ArgumentError signal:'sourceString must be a String'.
].
sourceCByteArray _validateClass: CByteArray .
lineNum _validateClass: SmallInteger .
fileName ifNotNil:[ fileName _validateClass: String ].
yTraceLevel _validateClass: SmallInteger .
warnBool _validateClass: Boolean .
aRubyEvalScope ifNotNil:[ aRubyEvalScope _validateClass: (System myUserProfile resolveSymbol:#RubyEvalScope) value ] .

"if we get here, check that $GEMSTONE/lib/libmagparse is not the dummy library."
self _primitiveFailed: #parse:cBytes:line:file:yTrace:warnings:evalScope:
     args: { sourceString . sourceCByteArray . lineNum . 
             fileName . yTraceLevel . warnBool . aRubyEvalScope }
%
