!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! $Id: GsNMethod_ruby.gs 26801 2011-09-13 21:44:15Z otisa $
!
!  Ruby support methods for GsNMethod
!=========================================================================

set class GsNMethod

! ---------------------------------------------
category: 'Ruby support'

classmethod:
traceIR: anInt

 "set value of class variable controlling printing of IR inputs to
  generateFromIR: .  printing is with GsFile gciLogServer:  . 

  anInt == 0 means  no tracing ,
        == 1 means  log source line number of each method node
        == 2 means  full print of IR graph for each method 
"

  TraceIR := anInt .
%

run
(GsNMethod _classVars associationAt:#TraceIR) 
  _objectSecurityPolicy: DataCuratorObjectSecurityPolicy  .
true
%

classmethod:
generateFromIR: aGsComMethNode

 "Invokes the code generator to generate an instance of GsNMethod from the
  specified IR graph.
  May be used for anonymous methods or for methods to be installed in a class.
  Caller of this method is responsible for installing in the
  method dictionary of an appropriate class.

  Returns a GsNMethod if the compilation
  succeeded with no warnings or errors, or an Array of of the form
    { (GsNMethod , or nil if the compilation has errors) .
       (nil or an Array of error descriptors as described for
        compileMethod:dictionaries:category: ) .
       (nil or a String describing warnings)
     } .

  Each element of the 'Array of error descriptors'
  is an Array of size 3 or 4, containing the elements:
   1. The GemStone error number.
   2. Offset into the source string where the error occurred.
   3. Error message text, if available, or nil.
   4. Internal compiler error text, if the error is internal.
 "
TraceIR ~~ 0 ifTrue:[
  TraceIR == 1 ifTrue:[
    aGsComMethNode fileName ifNotNil:[
      GsFile gciLogServer: '--IR: ' , aGsComMethNode summary
    ].
  ].
  TraceIR > 1 ifTrue:[ | strm |
    strm := IndentingStream newPrinting .
    aGsComMethNode printFormattedOn: strm .
    GsFile gciLogServer: strm contents .
    "  following code to be enabled after filein for debugging only,"
    "  SessionTemps not defined at this point in slowfilein  "
    " false ifTrue:[ |  hist |           
	hist := SessionTemps current at:#AllIrs otherwise: nil .
	hist == nil ifTrue:[
	   SessionTemps current at:#AllIrs put:( hist := { } ).
	]. 
	hist addLast: aGsComMethNode .
      ].
    "
  ].
].
^ self _generateFromIR: aGsComMethNode
%

classmethod:
_generateFromIR: aGsComMethNode

 "Invokes the code generator to generate an instance of GsNMethod from the 
  specified IR graph.
  May be used for anonymous methods or for methods to be installed in a class.
  Caller of this method is responsible for installing in the 
  method dictionary of an appropriate class. 

  Returns a GsNMethod if the compilation
  succeeded with no warnings or errors, or an Array of of the form
    { (GsNMethod , or nil if the compilation has errors) .
       (nil or an Array of error descriptors as described for
        compileMethod:dictionaries:category: ) ,
       (nil or a String describing warnings)
    }  .

  Each element of the 'Array of error descriptors' 
  is an Array of size 3 or 4, containing the elements:
   1. The GemStone error number.
   2. Offset into the source string where the error occurred.
   3. Error message text, if available, or nil.
   4. Internal compiler error text, if the error is internal.
 "

<primitive: 679>
self _primitiveFailed: #generateFromIR: args: { aGsComMethNode }
%

method:
setRubyProtection: anInt

"anInt values: 0==none, 1==protected, 2==private.

 must be a primitive to allow modifying an invariant method
 without disturbing any copy of the method in code generation."

<primitive: 786>
anInt _validateClass: SmallInteger .
(anInt < 0 or:[ anInt > 2]) ifTrue:[ anInt _error: #rtErrArgOutOfRange args:{ 0 . 2 }].
self _primitiveFailed:#setRubyProtection: args: { anInt }
%

method
inClass: aClass

  inClass := aClass
%

method
selector: aSelector
 "store a new selector into the receiver. 
  For use in define_method and alias implementations only."
| selNum mask |
self environmentId == 0 ifTrue:[ ArgumentError signal:'cannot assign to selector in a Smalltalk method'].
aSelector _isSymbol ifFalse:[ ArgumentError signal:'argument must be a Symbol' ].
selNum := aSelector asOopNumber .
mask := 16r1fffffffffe0 .
selector := (selector bitAnd:(mask bitInvert)) bitOr:(( selNum bitShift: 5) bitAnd: mask) .
%

method:
_executeInContext: anObject star: anArray block: aBlock

"Receiver must be a GsNMethod with ruby selector of form  foo*& .

 Returns result of invoking the receiver with anObject as self
 and passing the two arguments anArray, aBlock. 
 "

<primitive: 2008>
anArray _isArray ifFalse:[ anArray _validateClass: Array ].
^ self _primitiveFailed: #_executeInContext:star:block:
       args: { anObject . anArray . aBlock }
%


method:
_executeInContext: anObject nonBridgeMeth: aMethod star: anArray block: aBlock

"Receiver must be a generated bridge method expecting for arguments,
  equivalent to a ruby selector of form foo::*& .

 Returns result of invoking the receiver with the 4 arguments .
"
<primitive: 2008>
^ self _primitiveFailed: #_executeInContext:nonBridgeMeth:star:block:
       args: { anObject . aMethod . anArray . aBlock }
% 

method:
_executeInContext: anObject args: anArray

"Receiver should be a GsNMethod for a ruby method.
 This method is invoked by a generated bridge .
 The receiver must have numArgs == anArray size, otherwise
 an error is signaled. 
 Returns result of invoking the receiver with anArray unwrapped to
 satisfy the arguments of receiver, and with anObject as self .
"
<primitive: 2008>
^ self _primitiveFailed: #_executeInContext:args: args: { anObject . anArray }
%

method:
_copyForClass: aClass aliasFrom: oldPrefix to: newPrefix comment: commentString
"Returns a copy of the receiver .  
 The source string of the result is a copy of the source string of the
 receiver, with commentString appended .
 Caller responsible for sending immediateInvariant to the result.

 If newPrefix is a String, then in the copy any 
 send using SEND_CURRENT  bytecode  . 
 and having selector beginning with the String oldPrefix, 
 will have selector changed to begin with the String newPrefix."

<primitive: 807>
newPrefix ifNotNil:[
  oldPrefix _isOneByteString ifFalse:[ ArgumentTypeError signal:'expected a String'].
  newPrefix _isOneByteString ifFalse:[ ArgumentTypeError signal:'expected a String'].
  "a selector within receiver might be a large object or DoubleByteSymbol"
].
aClass isBehavior ifFalse:[ ArgumentTypeError signal:'expected a Behavior'].
commentString ifNotNil:[ 
  commentString _isOneByteString ifFalse:[ ArgumentTypeError signal:'expected a String'].
].
self _primitiveFailed: #_copyForClass:aliasFrom:to:comment:
     args: { aClass . oldPrefix . newPrefix . commentString }
%

method:
_rubyInspect
  | str  |
  str := '#<' copy .  
  str addAll: self class name ;
     addAll: ':0x' ; addAll: self asOop hex ; add: $  .
  ^ str 
%
method:
rubyArity
  		"ruby_selector_suffix dependent"
  | optArgBits idx sel delta haveSplat n |
  optArgBits := self rubyOptArgsBits .
  optArgBits == 0 ifTrue:[  "no optional args"
    delta := 0 .  "do not count block or star args"
    (sel := self selector) ifNotNil:[
      (sel _rubyOrdAt: -1) == 38 "$&" ifTrue:[ delta := 1 ].
      (sel _rubyOrdAt: -2) == 42 "$*" ifTrue:[ delta := delta + 1 . haveSplat := true ]. 
    ].
    n :=  self numArgs - delta  .
    haveSplat == true ifTrue:[ n := 0 - (n + 1) ].
    ^ n
  ].  
  idx := 1 .
  [ true ] whileTrue:[ 
    (optArgBits bitAt: idx ) == 1 ifTrue:[  
      "idx is one-based number of first optional arg"
       ^ 0 - idx  "result is -(n+1) where n is number of non-optional args"
    ].
    idx := idx + 1
  ]
% 
method:
_classNameAndRubySelectorWidth: anInt
  | text sel envId |
  "use _rubyInspect: to get names of ruby meta classes "
envId := self environmentId .
self isMethodForBlock ifTrue:[ | homeMeth homeCls |
  homeMeth := self homeMethod .
  homeCls := homeMeth inClass .
  homeCls ifNotNil:[
    text := 'block in ', (homeCls _rubyInspect: envId)
  ] ifNil:[
    text := 'block in executed code' copy .
  ].
  sel := homeMeth selector .
] ifFalse:[ | inCls |
  (inCls := inClass) ifNil: [ 
    text := 'executed code' copy .  
  ] ifNotNil:[
    text := inCls _rubyInspect: envId .
    sel := self selector . 
  ].
].
text width: anInt .
sel ifNotNil:[ text addAll: ' # '; addAll: sel ].
^ text
%

method: 
_rubyInClass: aClass
  "used by aliasing, no code modification priv check"
  inClass := aClass
%
method:
_rubyName
  " used by Kernel#__method__   runtime support"
  | sel |
  sel := self selector .
  self environmentId > 0 ifTrue:[
    sel ifNotNil:[ sel := sel prefixIfRubySelector ].
  ].
  ^ sel
%

method:
_descrForStackPadTo: minSize

| result hmMeth inCls envId |
result := String new .
envId := self environmentId .
self isMethodForBlock ifTrue:[
  result addAll: (envId == 0 ifTrue:[ '[] in  '] ifFalse:[ 'block in  ']) .
  hmMeth := inClass .
  inCls := hmMeth inClass .
] ifFalse:[
  hmMeth := self .
  inCls := inClass .
].
inCls == nil ifTrue:[
  result addAll:'Executed Code '
] ifFalse:[
  result addAll: inCls name . 
  result addAll:( envId == 0 ifTrue:[ ' >> '] ifFalse:[ ' # ']).
  result addAll: hmMeth selector .
].
[result size < minSize] whileTrue:[  result add: $  ]. "pad with spaces"
inClass ~~ nil ifTrue:[
  result addAll: ' (envId ' ; addAll: (hmMeth environmentId asString).
  self isRubyBridgeMethod ifTrue:[ result addAll:'b'].
  result add: $) .
] ifFalse:[
  result addAll: '          '.
].
^ result
%

method:
_nonBridgeMethod
  "used by topaz"
  self _debugInfoHasNonBridgeSelector ifTrue:[ | info nonBridgeSel |
    info := self _debugInfo .
    (nonBridgeSel := info at: DEBUGINFO_nonBridgeSel_offset) ifNotNil:[ | cm |
      cm := self inClass compiledMethodAt: nonBridgeSel 
	      environmentId: self environmentId otherwise: nil .
     (cm ~~ nil and:[ cm ~~ self]) ifTrue:[ 
        ^ cm isRubyBridgeMethod ifTrue:[ cm _nonBridgeMethod ] ifFalse:[ cm ] ].
    ].
  ].
  ^ nil
%

method:
_debugInfoHasNonBridgeSelector
| info1 |
info1 := debugInfo at: DebugInfo_fields1_offset .
^ ((info1 bitShift: 0 - DbgI1fileInfo_shift ) bitAnd: DbgI1fileInfo_mask) == 2
%

method:
_fileAndLine
  "Return an Array, { fileName . lineNumber } ,
   or nil if method  does not have that information.
   Smalltalk methods usually return nil."
  | mth fInfo mth_info |
  mth := self homeMethod .
  fInfo := mth _debugInfoHasFileAndLine .
  fInfo == 1 ifTrue:[ 
    mth_info := mth _debugInfo .
    ^ { (mth_info at: DEBUGINFO_fileName_offset) copy . 
         mth_info at: DEBUGINFO_lineNumber_offset }.
  ].
  fInfo == 2 ifTrue:[ "a method with nonBridgeSelector" | nonBridgeSel |
    mth_info := mth _debugInfo .
    (nonBridgeSel := mth_info at: DEBUGINFO_nonBridgeSel_offset) ifNotNil:[ | cm |
      cm := mth inClass compiledMethodAt: nonBridgeSel environmentId: mth environmentId.
      (cm ~~ nil and:[ cm ~~ mth and:[ cm ~~ self]]) ifTrue:[ 
        ^ cm _fileAndLine
      ].
    ]
  ].
  self environmentId ~~ 0 ifTrue:[  | lits |
    "handle a method produced by __define_method_block"
    lits := self literals .
    1 to: lits size do:[:j | | aLit litMeth |
      (aLit := lits at: j) _isExecBlock ifTrue:[ 
        (litMeth := aLit method ) ~~ mth ifTrue:[ ^ litMeth _fileAndLine]
      ]
    ].
  ].
  ^ nil
%

method:
_sourceFileLineComment
  "Returns a String, either empty or  with file name and line number"
  | mth |
  (mth := self homeMethod ) _debugInfoHasFileAndLine == 1 ifTrue:[ 
     | file line mth_info |
     file := (mth_info := mth _debugInfo) at: DEBUGINFO_fileName_offset .
     line := mth_info at: DEBUGINFO_lineNumber_offset .
     file ifNotNil:[ | ssiz envId str |
       str := String new .
       (envId := mth environmentId) == 0 ifTrue:[ str add: $" ]
					ifFalse:[ str add: $# "assume Ruby"].
       mth ~~ self ifTrue:[ str add: ' home' ].
       str add: ' method starts at line '; add: line asString ;
	  add: ' of ' ; add: file .
       envId == 0 ifTrue:[ str add: $" ].
       str add: Character lf  .
       ^ str 
      ].
   ].
  ^ ''
% 

method: 
_libraryForCallout
  "returns a CLibrary or signals an error"
  | lits |
  lits := self _literals select:[:x | x class == CCallout ].
  lits size == 1 ifFalse:[
    ArgumentError signal:'method does not have 1 CCallout'
  ].
  ^ (lits at: 1) _library
%

