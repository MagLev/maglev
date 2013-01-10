! file: Exception_ruby.gs

category: 'Ruby support'
classmethod: AbstractException
commentRubyMapping
^ '
The mapping from Smalltalk to Ruby exceptions is:

  Smalltalk                        	Ruby
  ---------------------        		-------------

  AbstractException 
    Exception     			Exception
  ... RubySystemExit                    ... SystemExit    (def in ruby image)
  ... RubyScriptError                   ... ScriptError   (def in ruby image)
  ...   RubyLoadError                   ...   LoadError   (def in .mcz)
  ...   RubyNotImplementedError         ...   NotImplementedError (def in ruby image)
  ...   RubyParseError                  ...   SyntaxError  (def in .mcz)
      ControlInterrupt			    SignalException
						Interrupt (def in rubyimage)
        Break
        Breakpoint 
        ClientForwarderSend 
        Halt    
        TerminateProcess
      Error				... StandardError
        CompileError
        EndOfStream
        ExternalError 
          IOError			   IOError
	    EOFError			   EOFError    (def in ruby image)
            SocketError			     SocketError
  ...         SocketErrorEAGAIN         ...	    (all 5 def in ruby image)
  ...         SocketErrorEBADF          ...         Ruby has separate Errno::EBADF..EPIPE
  ...         SocketErrorECONNRESET     ...         .  as subclass of SystemCallError
  ...         SocketErrorENOTCONN       ...         .
  ...         SocketErrorEPIPE          ...         .
          SystemCallError  		  SystemCallError
        ImproperOperation 
          ArgumentError  		    ArgumentError
          ArgumentTypeError 		    TypeError
          CannotReturn        		    LocalJumpError
          NotFoundError  
          OffsetError                       IndexError
          OutOfRange 			    RangeError
            FloatingPointError		 	FloatDomainError
          RegexpError                       RegexpError
          RubyRuntimeError		    RuntimeError    (def in ruby image)
        IndexingErrorPreventingCommit
        InternalError 
          GciTransportError   
        LockError
        NameError 			   NameError
          MessageNotUnderstood 		     NoMethodError
        NumericError
          ZeroDivide         		    ZeroDivisionError
        RepositoryError
					     Errno::xxx  (def in ruby image)
        SecurityError			  SecurityError
        SignalBufferFull
        ThreadError			  ThreadError
        TransactionError
        UncontinuableError
        UserDefinedError
      Notification    
        Admonition
          AlmostOutOfStack 		  SystemStackError
          AlmostOutOfMemory		  NoMemoryError
          RepositoryViewLost
        Deprecated
        FloatingPointException
        InterSessionSignal
        ObjectsCommittedNotification
        TransactionBacklog 
        Warning      
          CompileWarning
      TestFailure
    RubyBreakException  
    RubyThrowException 					
'
%

!-----------------------
!    cannot be done until monticello and omnibrowser tools are sanitized
! run
! "remove legacy selectors that should not be used in maglev code"
! AbstractException removeSelector:#gsArguments;
!   removeSelector: #gsNumber ;
!   removeSelector: #gsCategory .
! true
! %

classmethod: AbstractException
signal: signalText ignoring: ignoredValue

 ^ self new signal: signalText
%

classmethod: AbstractException
rubyBasicNew

^ self rubyBasicNew_stBaseClass: AbstractException
%

method: AbstractException
rubyPrivateSize
^ 8 "inline  AbstractException instSize"
%

classmethod: AbstractException
rubyPrivateInstSize
^ AbstractException instSize
%

method: AbstractException
_rubyReraise
  self _handlerActive ifTrue:[
    ^ self shallowCopy _signalWith: nil 
  ] ifFalse:[
    ^ self pass
  ]
%

classmethod: RubyBreakException
comment
^ 
'RubyBreakException is used to implement the "break" reserved word
in the Ruby language, except in inlined loops where the parser/code generator
translate the "break" to a GOTO bytecode.'
%

method: RubyBreakException
signalRubyBreak
  "signature and caller must agree with om::SearchForRubyBreakHandler()"
  <primitive: 2028>
   "If a handler found, new frame pushed 
      to execute  _executeHandler:   
      and prim does not return.
    else if home method of method 2 frames up is found, 
        returns to home context.
  
    Search for handler looks for frame of the form
      onException: RubyBreakException do:
    and will not find frames like
      onException: { RubyBreakException. OtherError} do: { ... }

    If exception handling succeeds and execution is to resume, 
    either the resume: or the return: primitive will do the 'goto'
    and we don't actually return from this frame .
    If handler not found, primitive fails so we can send defaultAction here.

    See also documentation in RubyBreakException>>signalBreakWith: .
"
 
  ^ self _signalWith: nil  "fall back to normal exception handling"
%

classmethod: RubyBreakException
signalBreakWith: aValue
  "signature and body must agree with om::SearchForRubyBreakHandler() in VM.
   sends of #signalBreakWith:  replaced with  Bc_RUBY_BREAK_FROM_PROC_u1_u32,
   when a block converted to a Ruby Proc  .

   A method containing a send of #signalBreakWith:  is forced by the
   code generator(comgen.c) to have a VariableContext .
  "
   | ex |
   (ex := self new) args: { true . aValue } .
   ^ ex signalRubyBreak
%

classmethod: RubyBreakException
signalRubyRetry
  | ex |
  (ex := self new) args: { false } .
  "set message text in case not handled by an on:do: of a each& style iterator"
  ^ ex signal:'retry outside of rescue, ' 
%

method: RubySystemExit
initialize
  super initialize .
  gsNumber := ERR_RUBY_SystemExitError .
  runAtExitHandlers := true .
%

method: RubySystemExit
status
  ^ status
%
method: RubySystemExit
status: aValue
  status := aValue
%
method: RubySystemExit
runAtExitHandlers
  ^ runAtExitHandlers
%
method: RubySystemExit
runAtExitHandlers: aValue
  runAtExitHandlers := aValue
%

method: NameError
name
 ^ selector
%

method: NameError
name: aName
  selector := aName .
  ^ aName
%

!--------------------------------------------
classmethod: RubyThrowException
comment
^'RubyThrowException is used in implementation of Ruby methods Kernel#catch,
Kernel#throw , and Signal#trap .
'
%

method: RubyThrowException
initialize
  gsNumber := ERR_RubyThrowException .
  gsResumable := true .
  gsTrappable := true 
%

method: RubyThrowException
name
 "return the name of a throw ."
 ^ gsArgs
%
method: RubyThrowException
signalNumber
 "return the signal number associated with Signal#trap"
 ^ signalNum
% 
method: RubyThrowException
_value
  ^ value
%
method: RubyThrowException
name: aSymbol
  gsArgs := aSymbol
%
method: RubyThrowException
name: aSymbol value: anObject
  gsArgs := aSymbol .
  value := anObject
%

classmethod: RubyThrowException
_initSignalConstants: envId
 | kind arr tns dict sigArr cls tmps sigTerm |
 tmps := SessionTemps current .
 kind := (tmps at: #Maglev_ruby_debugFlag otherwise: false)
       ifTrue:[ 3 ] ifFalse:[ 2 ].
 arr := RubySocket _socketOsConstants: kind .
 dict := RubyHash new .
 sigArr := { } .
 1 to: arr size by: 2 do:[:k | | nam val |
    val := (arr at: k + 1) .
    val ifNotNil:[
       nam := arr at: k .
       dict at: nam put: val .
    ].
    sigArr _rubyAt: val put: nam .
 ].
 dict immediateInvariant .
 sigArr immediateInvariant .
 tns := (cls := RubySignal)  transientNameSpaceForStore: envId .
 tns at: #TrappableByName transientRuntimePut: dict ;
     at: #TrappableSignals transientRuntimePut: sigArr ;
     at: #TrappedSignals  transientRuntimePut:  RubyIdentityHash new .
 (sigTerm := dict at: 'TERM') ifNotNil:[
   tns at: #SigTERM transientRuntimePut: sigTerm
 ].
 tmps at:#RUBY_RubySignal putNoStub: cls . "protect in-memory copy from GC"
 ^ self
%

method: TransactionError
gsArguments
  ^ gsArgs
%
