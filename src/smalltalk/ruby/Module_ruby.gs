!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id$
!
! File: Module_ruby.gs
!   
!=========================================================================

! Module created by bom.c

! should not need SELF_CAN_BE_SPECIAL, self should always be non-nil
!  in instances of Module

set class Module

category: 'Documentation'
classmethod: 
comment
^ 'Module is identically the Ruby class Module.

   Uses of copies of Modules as Ruby virtual classes is as follows.
   The actual creation of
   the virtual classes is done by primitives 682 and 694

   include a Module into a Class    (OM_RUBY_INCLUDE_Module)
   see   _rubyIncludeModule:envId:  in .mcz

  include a Module into a Class for class-side methods (forMeta==true)
    aClass includeModule: aModule forMeta: OM_RUBY_INCLUDE_Meta
    see _rubyIncludeMeta:envId:

  include a Module into itself so methods are available as both
   class and instance methods
       see _rubyIncludeSelf:envId:

  create a virtual class for a Module to support the   
   	module_function     method
     see _rubyIncludeModuleMethodsModuleEnv:
'
%

! sprintf:with:  in base smalltalk image

category: 'Browsing'
method:
definition

  ^ 'Module newModule name: ', name printString , '
       "does not include code to install in a dictionary" '
%


category: 'Wrapping Smalltalk classes'
classmethod:
_wrappedSmalltalkClasses

"Returns an Array of pairs,
   aClass, 1  means aClass will be in DataCuratorObjectSecurityPolicy in extent0.ruby.dbf. 
   aClass, 0  means class remains in SystemObjectSecurityPolicy and should not be 
		  extended in Ruby.
 A commented out class will not have a name in Ruby module Smalltalk."

^ { 
AbstractCharacter . 	1 . 
AbstractCollisionBucket . 1 . 
AbstractDictionary . 	1 . 
"AbstractException"
AbstractSession . 	1 . 
AbstractUserProfileSet . 	1 . 
"Activation is obsolete"
ArgumentError .  1 . "mapped to Ruby by Ruby bootstrap"
Array .  1 . "mapped to Ruby by Ruby bootstrap"
Association . 	1 . 
AutoComplete . 	1 . 
Bag . 	1 . 
BasicSortNode .  0 .  "implementation of indexing"
Behavior .       1 . "mapped to Ruby by Ruby bootstrap"
BinaryFloat . 	0 .   "not visible from pure Ruby"
BitSet . 	1 . 
"Block is obsolete"
"BlockClosure obsolete"
BlockSorter . 	1 . 
Boolean .        1 . "mapped to Ruby by Ruby bootstrap"
BtreeBasicInteriorNode . 0 .   "implementation of indexing"
BtreeBasicLeafNode . 	0 .   "implementation of indexing"
BtreeComparisonForCompare . 	0 .   "implementation of indexing"
BtreeComparisonForSort . 	0 .   "implementation of indexing"
BtreeComparisonQuerySpec . 	0 .   "implementation of indexing"
BtreeInteriorNode . 	0 .   "implementation of indexing"
BtreeLeafNode . 	0 .   "implementation of indexing"
BtreeNode . 	0 .   "implementation of indexing"
BtreeQuerySpec . 	0 .   "implementation of indexing"
BtreeRangeComparisonQuerySpec .  0 .   "implementation of indexing"
BtreeReadStream . 	0 .   "implementation of indexing"
BucketValueBag . 	0 .   "implementation of indexing"
ByteArray . 	1 . 
CannotReturn .   1 . "mapped to Ruby by Ruby bootstrap"
"CanonicalStringDictionary  obsolete"
"CanonStringBucket, 	0,  private to AllSymbols implementation"
CanonStringDict . 	1 . 
CanonSymbolDict . 	0 .  "class of AllSymbols, not extendable in Ruby"
CBuffer . 	1 . 
CByteArray . 	1 . 
CFunction . 	1 . 
Character . 	1 . 
CharacterCollection . 	1 . 
ClampSpecification . 	0 . "private to GBS implementation"
Class .  1 .                "mapped to Ruby by Ruby bootstrap"
ClassHistory . 	1 . 
ClassOrganizer . 	1 . 
ClassSet . 	1 . 
CLibrary . 	1 . 
ClientForwarder . 	0 .  "private to Smalltalk GUI clients"
ClusterBucket . 	1 . 
ClusterBucketArray . 	1 . 
Collection . 	1 . 
CollisionBucket . 	0 . "only subclasses extendable from Ruby"
"CompiledMethod obsolete"
CompileError . 	1 . 
"ComplexBlock obsolete"
"ComplexVCBlock obsolete"
ConstrainedPathEvaluator . 	1 .  "implementation of indexing"
ConstrainedPathTerm . 	0 .  "implementation of indexing"
ControlInterrupt .       1 . "mapped to Ruby by Ruby bootstrap"
CPointer . 	1 . 
CriticalSection . 0 .   "only subclasses extendable from Ruby"
CZstream .       1 . "mapped to Ruby by Ruby bootstrap"
Date . 	1 . 
DateAndTime . 	1 . 
DateAndTimeANSI . 	1 . 
DateTime . 	1 . 
DecimalFloat . 	1 . 
Delay . 	0 .    "used by ProcessScheduler, only subclasses extendable from Ruby"
DependencyList . 0 .  "implementation of indexing"
DepListBucket .  0 .  "implementation of indexing"
DepListTable . 	0 .  "implementation of indexing"
Dictionary . 	1 . 
DoubleByteString . 	1 . 
DoubleByteSymbol . 	1 . 
Duration . 	1 . 
EOFError .       1 . "mapped to Ruby by Ruby bootstrap"
EqualityIndexQueryEvaluator . 	0 . "implementation of indexing"
Error .  1 . "mapped to Ruby by Ruby bootstrap"
"ErrorDescription, 	0,  private to GciInterface"
"EUCString, 	0,   obsolete from Ruby perspective"
"EUCSymbol, 	0,   obsolete from Ruby perspective"
ExampleSetTest . 	1 . 
"Exception, 	0,  private to Smalltalk, do not use from Ruby"
"ExceptionSet, 	0,  private to Smalltalk, do not use from Ruby"
ExecBlock .      1 . "mapped to Ruby by Ruby bootstrap"
"ExecBlock0, 	0,  private to VM"
"ExecBlock1, 	0,  private to VM"
"ExecBlock2, 	0,  private to VM"
"ExecBlock3, 	0,  private to VM"
"ExecBlock4, 	0,  private to VM"
"ExecBlock5, 	0,  private to VM"
"ExecBlockN, 	0,  private to VM"
"ExecutableBlock obsolete"
FalseClass .     1 . "mapped to Ruby by Ruby bootstrap"
FastIdentityKeyValueDictionary . 	1 . 
Float .  1 . "mapped to Ruby by Ruby bootstrap"
FloatingPointError .     1 . "mapped to Ruby by Ruby bootstrap"
Fraction . 	1 . 
GciInterface . 	0 .  "only subclasses extendable from Ruby"
GemStoneParameters . 	1 . 
GsClassDocumentation . 	1 . 
GsCloneList . 	1 . 
"GsComArrayBuilderNode, 0,  private to code generator"
"GsComAssignmentNode, 	0,    private to code generator"
"GsComBlockNode, 	0,  private to code generator"
"GsComCascadeNode, 	0,  private to code generator"
"GsComGotoNode, 	0,  private to code generator"
"GsComLabelNode, 	0,  private to code generator"
"GsComLiteralNode, 	0,  private to code generator"
"GsComLitLeaf, 	0,  private to code generator"
"GsComLoopNode, 	0,  private to code generator"
"GsComMethNode, 	0,  private to code generator"
"GsCommitList, 	0,  private to code generator"
"GsComPathNode, 	0,  private to code generator"
"GsCompilerIRNode, 	0,  private to code generator"
"GsComReturnNode, 	0,  private to code generator"
"GsComSelectorLeaf, 	0,  private to code generator"
"GsComSendNode, 	0,  private to code generator"
"GsComStatementsNode, 	0,  private to code generator"
"GsComTermNode, 	0,  private to code generator"
"GsComVariableNode, 	0,  private to code generator"
"GsComVarLeaf, 	0,  private to code generator"
GsCurrentSession . 	1 . 
GsDocText . 	1 . 
"GsExceptionHandler, 	0,    private to Smalltalk legacy code"
GsFile .         1 . "mapped to Ruby by Ruby bootstrap"
GsFileStat .     1 . "mapped to Ruby by Ruby bootstrap"
GsfClassDefinitionInfo . 	1 . 
GsfModificationLog . 	1 . 
GsInterSessionSignal . 	1 . 
"GsMethod obsolete"
GsMethodDictionary . 	1 . 
"GsNativeCode, 	0,  private to VM"
GsNMethod .      1 . "mapped to Ruby by Ruby bootstrap"
GsPackage . 	0 .  "only subclasses extendable from Ruby"
GsPackageLibrary . 	0 .  "only subclasses extendable from Ruby"
GsPackagePolicy . 	0 .  "only subclasses extendable from Ruby"
GsProcess .      1 . "mapped to Ruby by Ruby bootstrap"
GsSession . 	1 . 
GsSessionMethodDictionary . 0 . "only subclasses extendable from Ruby"
GsSocket . 	0 .  "only subclasses extendable from Ruby"
"GsStackBuffer obsolete"
Halt . 	1 . 
IdentityBag . 	1 . 
IdentitySet .    1 . "mapped to Ruby by Ruby bootstrap"
IdentityBtreeNode . 	0 . "implementation of indexing"
IdentityCollisionBucket . 	0 . "only subclasses extendable from Ruby"
IdentityDictionary . 	1 .  "only subclasses extendable from Ruby"
IdentityIndex . 	0 .  "implementation of indexing"
IdentityIndexQueryEvaluator . 	0 . "implementation of indexing"
IdentityKeySoftValueDictionary . 1 . 
IdentityKeyValueDictionary . 	0 . "only subclasses extendable from Ruby, see Ruby IdentityHash"
IdentitySoftCollisionBucket . 	0 . "only subclasses extendable from Ruby"
IndentingStream . 	1 . 
IndexDictionaryEntryHolder . 	0 .  "implementation of indexing"
IndexedQueryEvaluator . 	0 .  "implementation of indexing"
IndexList . 	0 . 	"implementation of indexing"
IndexManager . 	1 . 
IndexManagerAutoCommitPolicy . 	1 . 
Integer .        1 . "mapped to Ruby by Ruby bootstrap"
IntegerKeyValueDictionary . 	1 . 
Interval . 	0 .  "only subclasses extendable from Ruby, see Ruby Range"
InvariantArray . 	1 . 
"InvariantEUCString, 	0,  obsolete, don't use in Ruby"
InvariantString . 	1 . 
IO .     1 . "mapped to Ruby by Ruby bootstrap"
IOError .        1 . "mapped to Ruby by Ruby bootstrap"
IPSocket .       1 . "mapped to Ruby by Ruby bootstrap"
ISOLatin . 	1 . 
"JapaneseString, 	0,  obsolete, don't use in Ruby"
"JISCharacter, 	0,  obsolete, don't use in Ruby"
"JISString, 	0,  obsolete, don't use in Ruby"
Kernel .         1 . "mapped to Ruby by Ruby bootstrap"
KeySoftValueDictionary . 1 . 
KeyValueDictionary . 	0 . "only subclasses extendable from Ruby, see Ruby Hash" 
"LanguageDictionary, 	0,  obsolete, don't use in Ruby"
"Large2ByteLeaf, 	0,  private to VM"
"Large4ByteLeaf, 	0,  private to VM"
"Large8ByteLeaf, 	0,  private to VM"
LargeInteger .   1 . "mapped to Ruby by Ruby bootstrap"
Locale . 	1 . 
LogEntry . 	1 . 
Magnitude . 	1 . 
MappingInfo . 	1 . 
MatchData .      1 . "mapped to Ruby by Ruby bootstrap"
MessageNotUnderstood .   1 . "mapped to Ruby by Ruby bootstrap"
Metaclass3 .  1 . "mapped to Ruby by Ruby bootstrap"
"MethodContext obsolete"
Module .         1 . "mapped to Ruby by Ruby bootstrap"
MultiByteString . 	1 . 
NameError .      1 . "mapped to Ruby by Ruby bootstrap"
Notification . 	1 . 
NscBuilder . 	0 .  "implementation of indexing"
Number .         1 . "mapped to Ruby by Ruby bootstrap"
Object .         1 . "mapped to Ruby by Ruby bootstrap"
"ObsoleteMetaclass  	private to Smalltalk"
"ObsDoubleByteString  obsolete"
"ObsDoubleByteSymbol  obsolete"
"ObsFloat  obsolete"
"ObsLargeNegativeInteger  obsolete"
"ObsLargePositiveInteger  obsolete"
"Obsolete23ClampSpecification  obsolete"
"Obsolete23GsFile  obsolete"
"ObsoleteClampSpecification  obsolete"
"ObsoleteDateTime  obsolete"
"ObsoleteDateTime50  obsolete"
"ObsoleteDictionary  obsolete"
"ObsoleteException  obsolete"
"ObsoleteGsFile  obsolete"
"ObsoleteGsProcess  obsolete"
"ObsoleteGsSocket  obsolete"
"ObsoleteIdentityCollisionBucket  obsolete"
"ObsoleteIdentityDictionary  obsolete"
"ObsoleteLanguageDictionary  obsolete"
"ObsoleteRcCollisionBucket  obsolete"
"ObsoleteSymbol  obsolete"
"ObsoleteSymbolAssociation  obsolete"
"ObsoleteSymbolDictionary  obsolete"
"ObsoleteSymbolKeyValueDictionary  obsolete"
"ObsoleteSymbolListDictionary  obsolete"
"ObsoleteSymbolSet  obsolete"
"ObsoleteTimeZone  obsolete"
"ObsoleteVariableContext  obsolete"
"ObsQuadByteString  obsolete"
"ObsSmallFloat  obsolete"
"OldRepository  obsolete"
OffsetError .    1 . "mapped to Ruby by Ruby bootstrap"
OutOfRange .     1 . "mapped to Ruby by Ruby bootstrap"
AlmostOutOfMemory .    1 . "mapped to Ruby by Ruby bootstrap"
OrderedCollection . 	1 . 
PassiveObject . 	1 . 
PathEvaluator . 	0 .  "implementation of indexing"
PathSorter . 	0 .  "implementation of indexing"
PathTerm . 	0 .  "implementation of indexing"
PositionableStream . 	1 . 
PrintStream . 	1 . 
"Process obsolete"
"Processor, 	0,  private to Smalltalk, in Ruby use Thread "
"ProcessorScheduler, 	0,  private to Smalltalk, in Ruby use Thread"
ProfMonitor . 	1 . 
ProfMonitorEntry . 	1 . 
QuadByteString . 	1 . 
"QuadByteSymbol, 	0,  private , instances not allowed yet"
QueryExecuter . 	0 .  "implementation of indexing"
Range .  1 . "mapped to Ruby by Ruby bootstrap"
RangeEqualityIndex . 	0 .  "implementation of indexing"
RangeIndexReadStream . 	0 .  "implementation of indexing"
"RcBtreeBasicInteriorNode, 	0,   private to Smalltalk RC for now"
"RcBtreeBasicLeafNode, 	0, private to Smalltalk RC for now"
"RcBtreeInteriorNode, 	0, private to Smalltalk RC for now"
"RcBtreeLeafNode, 	0, private to Smalltalk RC for now"
"RcCollisionBucket, 	0, private to Smalltalk RC for now"
RcCounter . 	1 . 
RcCounterElement . 	1 . 
RcIdentityBag . 	1 . 
"RcIndexBucket, 	0,  private to Smalltalk RC for now"
"RcIndexBucketWithCache, 	0,  private to Smalltalk RC for now"
"RcIndexDictionary, 	0,  private to Smalltalk RC for now"
RcKeyValueDictionary . 	1 . 
RcQueue . 	1 . 
RcQueueElement . 	1 . 
RcQueueEntry . 	1 . 
RcQueueRemovalSeqNumbers . 	1 . 
RcQueueSessionComponent . 	1 . 
RcRangeEqualityIndex . 	0 .  "implementation of indexing"
ReadStream . 	1 . 
RedoLog . 	1 . 
"ReenterBlock obsolete"
Regexp .         1 . "mapped to Ruby by Ruby bootstrap"
RegexpError .    1 . "mapped to Ruby by Ruby bootstrap"
Repository . 	1 . 
ResumableTestFailure . 	1 . 
ResumableTestFailureTestCase . 	1 . 
"RubyBreakException, 	0,   private to Smalltalk"
"RubyConstantRef, 	0,   private to Smalltalk"
RubyDirectory .  1 . "mapped to Ruby by Ruby bootstrap"
RubyEnv .        1 . "mapped to Ruby by Ruby bootstrap"
RubyHash .       1 . "mapped to Ruby by Ruby bootstrap"
RubyIdentityHash .       1 . "mapped to Ruby by Ruby bootstrap"
RubyNotImplementedError .        1 . "mapped to Ruby by Ruby bootstrap"
RubyScriptError .        1 . "mapped to Ruby by Ruby bootstrap"
RubySystemExit .         1 . "mapped to Ruby by Ruby bootstrap"
RubyThreadGroup .        1 . "mapped to Ruby by Ruby bootstrap"
"RubyThrowException, 	0,   private to Smalltalk "
RubyTime .       1 . "mapped to Ruby by Ruby bootstrap"
RubyRuntimeError .   1 . "mapped to Ruby by Ruby bootstrap"
ScaledDecimal . 	1 . 
SecurityError .  1 . "mapped to Ruby by Ruby bootstrap"
GsObjectSecurityPolicy . 	1 . 
GsObjectSecurityPolicySet . 	1 . 
SelectBlock . 	0 .   "implementation of indexing"
"Semaphore, 	0,    private to Smalltalk, Ruby should use ruby ConditionVariable"
SequenceableCollection . 	1 . 
SessionTemps . 	1 .  
Set . 	1 . 
SetValuedPathEvaluator . 	0 .  "implementation of indexing"
SetValuedPathTerm . 	0 .  "implementation of indexing"
SharedQueue . 	1 . 
"SimpleBlock obsolete"
SimpleTestResource . 	1 . 
SimpleTestResourceTestCase . 	1 . 
SmallDouble .    1 . "mapped to Ruby by Ruby bootstrap"
"SmallFloat, 	0,  deprecated, should not be used from Ruby"
SmallInteger .   1 . "mapped to Ruby by Ruby bootstrap"
RubySocket .     1 . "mapped to Ruby by Ruby bootstrap"
SocketError .    1 . "mapped to Ruby by Ruby bootstrap"
SocketErrorEBADF . 	1 . 
SocketErrorECONNRESET . 	1 . 
SocketErrorENOTCONN . 	1 . 
SocketErrorEPIPE . 	1 . 
SoftCollisionBucket . 	0 . "only subclasses extendable from Ruby for now"
SoftReference . 	1 . 
SortBlockNode . 	0 .  "implementation of indexing"
SortedCollection . 	1 . 
SortNode . 	0 .  "implementation of indexing"
SortNodeArray . 	0 .  "implementation of indexing"
"StackBuffer obsolete"
AlmostOutOfStack .  1 . "mapped to Ruby by Ruby bootstrap"
"StackSegment obsolete" 
Stream . 	1 . 
String .         1 . "mapped to Ruby by Ruby bootstrap"
StringKeyValueDictionary . 	1 . 
StringPair . 	1 . 
StringPairSet . 	1 . 
SUnitDelay . 	1 . 
SUnitNameResolver . 	1 . 
SUnitTest . 	1 . 
Symbol .         1 . "mapped to Ruby by Ruby bootstrap"
SymbolAssociation . 	1 . 
SymbolDictionary . 	1 . 
SymbolKeyValueDictionary . 	1 . 
SymbolList . 	1 . 
SymbolSet . 	1 . 
System .         1 . "mapped to Ruby by Ruby bootstrap"
SystemCallError .        1 . "mapped to Ruby by Ruby bootstrap"
SystemLoginNotification . 	1 . 
TCPServer .      1 . "mapped to Ruby by Ruby bootstrap"
TCPSocket .      1 . "mapped to Ruby by Ruby bootstrap"
UDPSocket .      1 . "mapped to Ruby by Ruby bootstrap"
TestCase . 	1 . 
TestFailure . 	1 . 
TestResource . 	1 . 
TestResult . 	1 . 
TestSuite . 	1 . 
ThreadError .    1 . "mapped to Ruby by Ruby bootstrap"
Time . 	1 . 
TimeZone . 	1 . 
TimeZoneInfo . 	1 . 
TimeZoneTransition . 	1 . 
TransactionBoundaryDefaultPolicy . 	1 . 
"TraversalBuffer, 	0,  private to GBS implementation"
TrueClass .      1 . "mapped to Ruby by Ruby bootstrap"
ArgumentTypeError .      1 . "mapped to Ruby by Ruby bootstrap"
UndefinedObject .        1 . "mapped to Ruby by Ruby bootstrap"
"UnimplementedFloat1, 	0,  private to Smalltalk"
"UnimplementedFloat2, 	0,  private to Smalltalk"
UnorderedCollection . 	1 . 
UserDefinedError . 	1 . 
Exception .  1 . "mapped to Ruby by Ruby bootstrap"
UserProfile . 	1 . 
UserProfileSet . 	1 . 
"UserSecurityData, 	0,  private to Smalltalk"
VariableContext .        1 . "mapped to Ruby by Ruby bootstrap"
Warning . 	1 . 
WriteStream . 	1 . 
ZeroDivide .     1  "mapped to Ruby by Ruby bootstrap"
 }
%

category: 'Ruby support'

method:
transientMethodDictForEnv: envId put: aValue
  "aValue should be a GsMethodDictionary, or nil ,
   caller responsible for _refreshClassCache.
   For ruby, not a protected method. 
   NOTE, VM class creation and fault-in initializes transientMethDicts
   to an empty array. "

transientMethDicts _rubyAt: (envId*4)"zero based"  put: aValue
%

method:
_addVirtualSuperclassPrim: includedClass kind: anInt envId: envId

"Returns the new virtual class, or a String describing failure. 

 Sends to super within instance methods of the receiver will bypass any
 ruby virtual classes in the superclass chain and will go to the first
 non-virtual superclass.

 Generates an error if any of the following are true
   includedClass is not an instance of Module .

   the argument defines any instVars and they are not exactly the
   same instVars as the receiver's current superclass .

   receiver is a committed class (because method-lookup cache coherency
     in multiple session case probably can't handle changes to
     superclass chain correctly)

   argument has any of these format bits set
     NON_PERSISTENT, DB_TRANSIENT, RUBY_VIRTUAL, SUBCLASS_DISALLOWED,
     INDEXABLE, NO_STRUCT_UPDATE, TRAV_BY_CALLBACK

   argument is a subclass any of the classes Number, IdentityBag,
     SequenceableCollection, KeyValueDictionary, Exception,  ExecBlock,
     Regexp.   ( this list checked for in primitive) .

   receiver is a subclass of one of the classes   Number, IO, KeyValueDictionary ,
     because those classes contain Smalltalk env0 implementations of Ruby methods
     and the env0 implementations contain sends to super,
     that would not see the virtual class.
"
<primitive: 694>
anInt _validateClass: SmallInteger .
envId _validateClass: SmallInteger .
(envId < 1 or:[ envId > 255]) ifTrue:[ OutOfRange signal:'invalid envId'].
^ self _primitiveFailed: #_addVirtualSuperclass:kind:envId:
  args: { includedClass . anInt . envId }
%


! deleted deleteRubyVirtualSuperclasses

! addRubyVirtualSuperclass:forMeta:  moved to .mcz
! includeRubyModule: moved to .mcz
! moduleIncludeSelf moved to mcz
! allModuleMethodsEnabled, moduleMethodsModule, addModuleMethodIfEnabled:  moved to mcz
! Module>>addModuleMethod:  implemented in .mcz

method:
_addRubyVirtualSuperclass: includedClass kind: kindInt envId: envId 
  | result |
(includedClass class == Module or:[ includedClass == Kernel ]) ifFalse:[ 
  ((includedClass isKindOf: Module) and:[ (includedClass isKindOf: Metaclass3) not]) ifFalse:[
    self error:'arg is not a Module'
  ].
].
(includedClass instSize == 0 or:[
   self _instVarNames = includedClass _instVarNames]) ifTrue:[
    | disallowed |
    (self isKindOfClass: Number) ifTrue:[ disallowed := true ].
    (self isKindOfClass: IO) ifTrue:[ disallowed := true ].
    (self isKindOfClass: KeyValueDictionary) ifTrue:[ disallowed := true ].
    disallowed ifNil:[ | ctxCls ctx |
      result := self _addVirtualSuperclassPrim: includedClass kind: kindInt envId: envId .
      result _isOneByteString ifTrue:[
        self error: 'insert class disallowed, ', result .
      ].
      self _clearLookupCaches: envId . 
    ] ifNotNil:[
      self error: 'receiver ' , self name , ' may not have virtual classes'.
    ].
] ifFalse:[
    self error:'inconsistent instVars'. 
].
^ result
%

! deletions

classmethod: 
isMetaOrModule
  ^ true
%
method: 
isMetaOrModule
  ^ true
%
method:
extraDict
  "browser support"
  ^ nil
%

method:
_persistable

^  (format bitAnd: 16r800000) == 0  "GC_RubyModuleNP bit"
%

method
isVirtual
   ^ (format bitAnd: 16r14000) ~~ 0
%

method:
changeToSecurityPolicyForRubyExtension: anObjectSecurityPolicy

"If self objectSecurityPolicy ~~ anObjectSecurityPolicy,
 then for self and  self class
   move each of
     the class 
     the class's  env 1, method dict (if any)
   to specfied GsObjectSecurityPolicy.
 
 This allows env 1..n methods and name spaces to be added 
 by UserProfile's  with write access to anObjectSecurityPolicy .
 Ruby methods don't use categories, so don't bother with categorys.
 Env 0 methods/dicts will not be moved, and the package manager
 will use session methods for .mcz code in env 0, if the
 receiver was originally in  SystemObjectSecurityPolicy .
"
 | aCls cvs |
 (cvs := classVars) ifNotNil:[ 
    cvs _objectSecurityPolicy: anObjectSecurityPolicy 
  ].
  aCls := self .
  2 timesRepeat:[ | mds dict |
    aCls _objectSecurityPolicy: anObjectSecurityPolicy .
    mds := aCls.methDicts .
    mds _isArray ifTrue:[
      mds _objectSecurityPolicy: anObjectSecurityPolicy .
      dict := aCls persistentMethodDictForEnv: 1 .
      dict ifNotNil:[ dict _objectSecurityPolicy: anObjectSecurityPolicy ].
    ].
    aCls := self class 
  ].
%
method: 
_rubySuperclass
  "a Ruby primitive"
  ^ self rubySuperclass: 1"__callerEnvId"
%
method: 
_incrementCachedConstantsSerialNum

"Returns the new value of the serial number, a SmallInteger.
 The VM's value of the serial number is used in RubyConstantRef prims 790 and 791"
<primitive: 840>

self _primitiveFailed: #_incrementCachedConstantsSerialNum
%

method:
_rubySubclassOf: aClass
  "a ruby primitive"
  ^ self _rubySubclassOf: aClass env: 1"__callerEnvId" 
%

method:
_rubySubclassOf: aClass env: envId
  <primitive: 842>
  envId _validateClass: SmallInteger .
  self _primitiveFailed: #_rubySubclassOf:env: args: { aClass . envId }
%

classmethod:
clearVmTransientState: envId

<primitive: 851>
envId _validateClass: SmallInteger .
(envId < 0 or:[ envId > 255]) ifTrue:[ OutOfRange signal:'envId out of range'].
self _primitiveFailed: #clearVmTransientState: args: { envId }
%

method:
_name
  | pcopy |
  (self isMetaModule and:[ (pcopy := primaryCopy) ~~ nil]) ifTrue:[
     ^ 'meta', pcopy name
  ].
  ^ name
%
method:
name
  | pcopy |
  (self isMetaModule and:[ (pcopy := primaryCopy) ~~ nil]) ifTrue:[
     ^ 'meta', pcopy name
  ].
  ^ name
%

