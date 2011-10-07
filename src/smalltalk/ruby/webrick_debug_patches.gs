category: 'patches'

method: Object
rubyPrint: aString
  GsFile gciLogClient: aString
%

set class Object
method:
doesNotUnderstand: aMessageDescriptor
   
   "Generates an error reporting that the receiver cannot respond to a message
    because no compiled method was found for the selector.  The argument
    aMessageDescriptor is a two-element Array.  The first element is the selector
    that was not found and the second is an Array of arguments for the message."
   
   | selector argList |
   
   selector := aMessageDescriptor at: 1 .  "put selector into temporary for easier"
                                           "  debugging with topaz."
   argList := { self . selector . (aMessageDescriptor at: 2) }.

   self rubyPrint:'-- MNU:  ', self class name , ' >> ', selector .
   self pause .
   
   System signal: 2010 "#rtErrDoesNotUnderstand, hard-coded for speed"
          args: argList signalDictionary: GemStoneError .
   
   "If we continue from the error, re-try the send of the message that was
    not understood."
   
   ^ self perform: (aMessageDescriptor at: 1)
          withArguments: (aMessageDescriptor at: 2).
%

method: Object
cantPerform: aSelectorSymbol withArguments: anArray

"This method implements the default response when a message can't be performed
 with _perform:withArguments:.  It raises the rtErrCantPerform exception."

self pause .
self _error: #rtErrCantPerform args: { aSelectorSymbol . anArray }.
^ self _primitiveFailed: #perform:withArguments:
       args: { aSelectorSymbol . anArray }
%

method: Object
_primitiveFailed: aSelector args: primArgs

"Methods which are implemented as primitives send _primitiveFailed:
 when a primitive fails and the failure is not attributable to any
 normal error such as bad argument kind, argument out of range, etc."

| args |
self pause .
args := Array with: aSelector.
args add: primArgs.
^ self _error: #rtErrPrimFailed args: args .
%

method: Object
halt: messageString

"Raises an error.  This method is intended for use in raising
 application-defined or user-defined errors. Returns the receiver."

self rubyPrint:'-- Object halt: ' , messageString .
System genericSignal: #halt text: messageString.
^ self
%

