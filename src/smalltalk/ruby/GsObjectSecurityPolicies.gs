! file image/ruby/GsObjectSecurityPolicies.gs
run
  "following classes moved to DataCuratorObjectSecurityPolicy because they have
   Ruby mappings loaded by the Ruby bootstrap."
{AbstractDictionary .
 AbstractException .
 Array . 
 Behavior . 
 Boolean . 
 Class .
 CLibrary . 
 CByteArray . 
 CFunction . 
 CCallout . 
 CCallin . 
 CZstream . 
 ExecBlock . 
 FalseClass . 
 Float . 
 GsFile . 
 GsFileStat . 
 GsProcess . 
 GsSocket . 
 GsNMethod . 
 IdentitySet . 
 IdentityKeyValueDictionary . 
 Integer . 
 IO . 
 IPSocket . 
 Kernel . 
 LargeInteger . 
 MatchData . 
 Metaclass3 . 
 Module . 
 Number . 
 Object . 
 Range . 
 Regexp . 
 RubyConstantRef . 
 RubyDirectory . 
 RubyEnv . 
 RubyHash . 
 RubyIdentityHash . 
 RubyProc . 
 RubyThreadGroup . 
 RubyTime . 
 SmallInteger . 
 Semaphore . 
 SmallDouble . 
 String . 
 Repository . 
 RubySocket . 
 RubySignal . 
 RubyCextData .
 SymbolAssociation . 
 System . 
 Symbol . 
 TransientShortArray . 
 TCPSocket . 
 TCPServer . 
 UDPSocket . 
 TrueClass . 
 UndefinedObject . 
 VariableContext . 

 ArgumentError . 
 CannotReturn . 
 ControlInterrupt . 
 EOFError . 
 Error . 
 FloatingPointError . 
 IOError . 
 MessageNotUnderstood . 
 NameError . 
 OffsetError . 
 AlmostOutOfMemory . 
 ProfMonitorTree . 
 OutOfRange . 
 RegexpError . 
 RubyScriptError . 
 "RubyLoadError created by DataCurator"
 RubyNotImplementedError . 
 "RubyParseError created by DataCurator"
 RubyScriptError . 
 RubyStopIterationError . 
 RubySystemExit . 
 RubyRuntimeError . 
 SecurityError . 
 SocketError . 
 SocketErrorEBADF . 
 SocketErrorECONNRESET . 
 SocketErrorENOTCONN . 
 SocketErrorEPIPE . 
 AlmostOutOfStack . 
 SystemCallError . 
 TransactionError . 
 ThreadError . 
 ArgumentTypeError . 
 UNIXServer .
 UNIXSocket .
 Exception . 
 ZeroDivide
 } do:[ :aCls | 
  aCls changeToSecurityPolicyForRubyExtension: DataCuratorObjectSecurityPolicy .
  GsFile gciLogServer:'moved ' , aCls name , ' to DataCuratorObjectSecurityPolicy'.
].
true
%

run
{ Kernel . RubySignal } do:[ :aCls |
  "move the virtual class for  OM_MODULE_INCLUDE_self "
  aCls virtualClass changeToSecurityPolicyForRubyExtension: DataCuratorObjectSecurityPolicy .
  GsFile gciLogServer:'moved ' , aCls name , ' virtualClass to DataCuratorObjectSecurityPolicy'.
].
true
%
  

run
"now move to DataCuratorObjectSecurityPolicy additional classes to be exposed by
 the wrapping of Smalltalk classes"
| list |
list := Module _wrappedSmalltalkClasses .
1 to: list size by: 2 do:[:n || aCls num |
  aCls := list at: n .
  num := list at: n + 1 .
  (num == 1 and:[ aCls objectSecurityPolicy == SystemObjectSecurityPolicy]) ifTrue:[ 
    aCls changeToSecurityPolicyForRubyExtension: DataCuratorObjectSecurityPolicy .
    GsFile gciLogServer:'moved ' , aCls name , '	to DataCuratorObjectSecurityPolicy'.
  ].
].
true
%
