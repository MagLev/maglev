!=========================================================================
! Copyright (C) VMware, Inc. 2008-2011.  All Rights Reserved.
!
! GsCompilerIRNode.gs
!        Methods for GsCompilerIRNode and subclasses.
!       These  classes are created in bom.c
!
! The hierarchy is:
! Object
!   GsCompilerIRNode
!     GsComLitLeaf
!     GsComSelectorLeaf
!     GsComVarLeaf
!     GsComArrayBuilderNode
!     GsComAssignmentNode
!     GsComBlockNode
!     GsComCascadeNode
!     GsComLiteralNode
!     GsComMethNode
!     GsComPathNode
!     GsComReturnNode
!     GsComSendNode
!     GsComTermNode
!     GsComVariableNode
!     GsComLabelNode
!     GsComGotoNode
!     GsComStatementsNode
!
! $Id: GsCompilerIRNode.gs 26351 2011-08-05 02:54:28Z otisa $
!=========================================================================

expectvalue %String
run
| oldCls |
oldCls := Globals at:#IndentingStream otherwise: nil .
oldCls == nil ifTrue:[
 Object subclass: 'IndentingStream'
  instVarNames: #( indentLevel stream )
  classVars: #()
  classInstVars: #()
  poolDictionaries: { }
  inDictionary: Globals .
  ^ 'created class'
] ifFalse:[
  ^ 'existing class definition:  ' + oldCls definition
].
%

run
 "remove any previously existing methods, if any"
 { GsComGotoNode . 
   GsCompilerIRNode . 
   GsComLitLeaf . 
   GsComSelectorLeaf . 
   GsComVarLeaf . 
   GsComArrayBuilderNode . 
   GsComAssignmentNode . 
   GsComBlockNode . 
   GsComCascadeNode . 
   GsComLiteralNode . 
   GsComMethNode . 
   GsComPathNode . 
   GsComReturnNode . 
   GsComSendNode . 
   GsComTermNode . 
   GsComVariableNode . 
   GsComLabelNode . 
   GsComGotoNode .  
   GsComLoopNode . 
   GsComStatementsNode . 
   IndentingStream } do:[:cls |
     cls removeAllMethods. 
     cls class removeAllMethods
   ].
   ^ true
%
expectvalue %String
run
^  GsCompilerIRNode _classVars rebuildIfNeeded
     ifTrue:[ 'rebuilt'] ifFalse:[ 'no change']
%

!-----------------------------------------------------------------
set class IndentingStream
category: 'Documentation'
classmethod:
comment
^
' IndentingStream wraps another stream, but indents each line to its indent level.
        This is for pretty-printing nested structures such as ParseNodes.
        New protocol includes #indentMore and #indentLess
'
%

category: 'Instance creation'
classmethod:
on: aStream
  ^self new stream: aStream
%
classmethod:
newPrinting

^ self new stream: (PrintStream on: String new )
%

method:
contents
^ stream contents
%

method:
cr
        stream cr.
        indentLevel timesRepeat: [stream nextPutAll: '  ']
%

method:
indentLess
        indentLevel := indentLevel - 1
%

method:
indentMore
        indentLevel := indentLevel + 1
%

method:
nextPutAll: aCollection
        stream nextPutAll: aCollection
%
method:
nextPut: aChar
  " assume aChar is not  CR "
  stream nextPut: aChar
%
method:
space
   stream space
%
method:
print: aLabel int: anInteger
  stream nextPutAll: aLabel ; nextPutAll: anInteger asString 
%
method:
print: aLabel bool: aBoolean
  stream nextPutAll: aLabel ; nextPutAll: aBoolean asString 
%
method:
print: aLabel symbol: aSymbol
  stream nextPutAll: aLabel ; nextPutAll: aSymbol printString 
%

method:
stream: aStream
    indentLevel := 0.
    stream := aStream
%

method:
do: aCollection 
| first idx |
first := true .
self indentMore ; nextPutAll: ' ( '.
idx := 1.
aCollection do:[:aNode| 
  first ifFalse:[ self cr ].
  first := false .
  self nextPutAll: idx asString ; nextPut: $: .
  aNode printFormattedOn: self .
  idx := idx + 1 .
].
self indentLess; nextPutAll: ' )'; cr .
%

!------------------------------------------------------------
! install class category into all classes for use by Monticello
run
 { GsCompilerIRNode . 
      GsComLitLeaf . 
      GsComSelectorLeaf . 
      GsComVarLeaf . 
      GsComArrayBuilderNode . 
      GsComAssignmentNode . 
      GsComBlockNode . 
      GsComCascadeNode . 
      GsComLiteralNode . 
      GsComMethNode . 
      GsComPathNode . 
      GsComReturnNode . 
      GsComSendNode . 
      GsComTermNode . 
      GsComVariableNode . 
      GsComLabelNode . 
      GsComGotoNode . 
      IndentingStream
 } do:[ :aClass |
   aClass category: 'Gemstone-IRgraph'
 ].
^ true
%


!------------------------------------------------------------
set class GsCompilerIRNode
! remove all Smalltalk disallowed methods
removeallmethods
removeallclassmethods

run
  "Set ruby compatibility level"
  GsCompilerIRNode _addClassVar: #COM_RUBY_COMPAT_LEVEL value: 19.
true
%  

category: 'Documentation'
classmethod:
comment
^ '
 GsCompilerIRNode  is the abstract superclass of all classes used
 to implement nodes of the IR graph which is input to the 
 code generator comgen.c  in Gemstone64 v3.0.   The instVars are:

  kind      , a SmallInteger, per ComIRNodeEType
  srcOffset, a SmallInteger , 1-based character position in the source , or nil .
  lineNumber, a SmallInteger, optional 1-based line number in the source,
                              or -1 or nil if unknown
         with Smalltalk parser lineNumber used for debugging compiler and slow filein
         with Ruby parser written in Ruby , lineNumber only used to hold
         starting line of a  GsComMethNode .
 
  See comments for GsComMethNode for more details on srcOffset and lineNumber.

 In the documentation of subclasses,
    a non-leaf GsCompilerIRNode
 means instance of any subclass 
 except GsComLitLeaf, GsComSelectorLeaf, GsComVarLeaf 
 
 The class variables for GsCompilerIRNode include symbolic names
 for the various COMPAR, COM_RTN, and Bc_ constants used in
 instance methods in subclasses.  The class variables are populated
 at server image build by the code in src/bom.c , from constants in 
 src/comparse.ht and src/bytecode.ht .
'
%
category: 'Accessing'
classmethod:
rubyCompatibilityLevel
  "Returns 18 for MRI 1.8 compatibility, 19 for MRI 1.9, etc "

  ^ COM_RUBY_COMPAT_LEVEL
%
method:
lineNumber
  ^ lineNumber
%
method:
sourceOffset
  ^ srcOffset
%
method:
lastLineNumber
  ^ lineNumber
%
method:
lastSourceOffset
  ^ srcOffset
%
method:
symbolLiteralValue

  ^ nil "caller should signal a RubyParseError"
%
method:
litVarValueOrNil
  ^ nil
%

method:
hasPosition
  ^ srcOffset ~~ nil or:[ lineNumber ~~ nil ]
%

category: 'Printing'
method:
printOn: aStream

  aStream nextPut: $(; space;  nextPutAll: self class name ; 
     nextPutAll:' objId: ' ; nextPutAll: self asOop asString .
  srcOffset ifNotNil:[
     aStream nextPutAll:' srcOfs:' ; nextPutAll: srcOffset asString ; space
   ] ifNil:[ 
     aStream nextPutAll:' line:' ; nextPutAll: lineNumber asString ; space
   ].
%
method:
printFormattedOn: aStream
  self printOn: aStream
%

method:
printString
  | strm |
  strm := IndentingStream newPrinting .
  self printFormattedOn: strm .
  ^ strm contents .
%

category: 'Updating'
method:
lineNumber: aSmallInteger
  "to be used only after initializating the node .
   The argument is a positive one-based  line number"

  lineNumber := aSmallInteger
%
method:
sourceOffset: aSmallInteger
  "Argument is a 1-based character offset into the source string."
  srcOffset ifNil:[ srcOffset := aSmallInteger ]
%

method:
validateEnvironment: anInteger
  "range check on an environment identifier for a method or send node"
  (anInteger < 0 or:[ anInteger > 16rFF ]) ifTrue:[
    anInteger error:'out of range' . 
    ^ 0
  ].
  ^ anInteger
%

category: 'Transformation'
method:
returnNode

^ GsComReturnNode new return: self
%

category: 'Accessing'
method:
kind
  ^ kind
%

method:
varLeaf
  ^ nil
%

!------------------------------------------------------------
set class GsComLitLeaf
category: 'Documentation'
classmethod:
comment
^
'GsComLitLeaf  represents a literal leaf , and has these instVars:

  stringForm , a String  ,   the source form of the literal reference ,
                     stringForm is nil if literal is a GsNMethod (for Ruby)
  litValue   , an Object ,   the value of the literal
  litKind    , SmallInteger   , a ComParLitEtype

 A GsComLitLeaf is typically referenced by a GsComLiteralNode 
 thus allowing canonicalization of literal references, if the
 Parser maintains a dictionary, keyed on string source form of a literal
 and with values being instances of GsComLitLeaf.

 stringForm and litValue are provided separately in case a parser wants
 to do conversions such as string to numeric during a second pass.  The
 generator only looks at litValue , and the stringForm is present in the IR
 graph for use in debugging the parser or generator.
 The generator may change litValue to a canonicalize zero sized 
 literal arrays or strings, such as  Smalltalk  #() , ''''  .
 In 64bit Gemstone,  the float 0.0  is a SmallDouble and no longer needs
 canonicalization.

 The instance initializers below assume the parser would like
 to pass the string form of numeric literal, and have the class library
 convert from String to numeric form immediately.  String to numeric
 conversion could generate errors if the parser does not catch all illegal
 floating point formats, etc.   So if the parser did not want to worry
 about errors during numeric conversion, it might want other methods
 to trigger numeric conversion later ...

 The COMPAR constants must agree with ComParLitEtype in src/comparse.ht  .

 The following numeric literals are not supported yet by these methods:
   COMPAR_SCALED_DEC_LIT ,   ScaledDecimal, assume not needed by Ruby
   COMPAR_DECIMAL_FLT_LIT ,  DecimalFloat, assume not needed by Ruby
   COMPAR_FixedPoint_LIT ,   FixedPoint  , assume not needed by Ruby
'
%

category: 'Printing'
method:
printFormattedOn: aStream

  super printOn: aStream .
  aStream print:' litValue:' int: litValue ;
          print:' litKind:' int: litKind ;
      nextPut: $) ; cr .
%
category: 'Instance creation'
classmethod:
newNil
  ^ self new specialLiteral: nil
%

category: 'Instance Initialization'
method:
setIRnodeKind
  kind :=  COMPAR_LIT_LEAF
%

method:
characterLiteral: aCharacter
  self setIRnodeKind .
  litValue := aCharacter .
  litKind := COMPAR_CHAR_LIT .
%

method:
specialLiteral: aValue
  "aValue is expected to be an instance of Boolean or UndefinedObject"
  self setIRnodeKind .
    litValue := aValue .
  litKind := COMPAR_SPECIAL_LIT .
%

method:
integerLiteral: anInteger
  litValue := anInteger .
  self setIRnodeKind .
  litKind := COMPAR_INT_LIT
%

method:
integerLiteralFromString: aString
  stringForm := aString .
  self integerLiteral: (Integer fromString: aString)
%

method:
floatLiteral: aFloat
  litValue := aFloat .
  self setIRnodeKind .
  litKind := COMPAR_FLT_LIT .
%

method:
floatLiteralFromString: aString

  "See Float>>fromString: for details of NaN representations in the input."

  self floatLiteral: (Float fromString: aString)
%

method:
stringLiteral: aString

" aString is a  string literal such as the Smalltalk literal  'abc'  
  that can be completely generated by the Parser.

 It is the responsibility of the parser to maintain a dictionary
 of String literals if it is desired to canonicalize Strings  within
 method compilations, or across method compilations .  All String
 literals will be made invariant by the code generator."  

  self setIRnodeKind .
  stringForm := aString .
  litKind := COMPAR_STR_LIT .
  litValue := aString .

  aString immediateInvariant .
%

method:
rubyCopyingStringLiteral: aString

  "This literal will be accessed with a PUSH_COPYOF_LIT bytecode
   see also comments in stringLiteral: . "

  aString _isOneByteString ifFalse:[
    self error:'expected a String'   "detect parser problems, relax when QB string"
  ].
  self setIRnodeKind .
  stringForm := aString .
  litKind := COMPAR_RUBY_COPYING_STR_LIT .
  litValue := aString .
  aString immediateInvariant .
%

method:
symbolLiteral: aString

  | sym |
  self setIRnodeKind .
  stringForm := aString .
  sym := aString asSymbol .
  sym class == DoubleByteSymbol ifTrue:[
    self error:'DoubleByteSymbol not supported for Ruby.'
  ].
  litValue :=  sym .
  litKind := COMPAR_SYM_LIT .
%

method:
arrayLiteral: anArray

"anArray is a literal such as the Smalltalk literal  #( 1 2 abc )
 which can be constructed without executing any bytecodes. 

 In Smalltalk , these arrays are  Invarant , and the  Parser
 canonicalizes them based on comparing substrings from the source code.
"
  self setIRnodeKind .
  "stringForm left as nil for now"
  litKind := COMPAR_ARRAY_LIT .
  litValue := anArray .
  anArray immediateInvariant 
%

method
constRefLiteral: aRubyConstantRef

  self setIRnodeKind .
  litKind := COMPAR_ASSOC_LIT .  "fault into old_gen if possible"
  litValue := aRubyConstantRef .
%

method:
objectLiteral: anObj

"Used for generic non-Assocation non-invariant literals
 in Ruby methods."

  self setIRnodeKind .
  "stringForm left as nil for now"
  litKind := COMPAR_ARRAY_LIT .
  litValue := anObj .   "do not set invariant"
%

method:
deferredGlobalLiteral: anObject

"used in ruby IR "

  self setIRnodeKind .
  "stringForm left as nil for now"
  litKind := COMPAR_ARRAY_LIT .
  litValue := anObject .   "do not make invariant."
%

method:
methodLiteral: anASTnode

"anASTnode is the root of a graph of the AST for the method,
 need to transform that tree to tree beginning with a GsComMethNode 
 at the time we want to compile the method.  So the AST tree is
 sometimes persistent.
"
  self setIRnodeKind .
  "sourceForm left as nil,  method has source in it's debug info."
  litValue := anASTnode .
  litKind := COMPAR_METHOD_LIT .
%

category: 'Accessing'
method:
symbolLiteralValue
  litKind == COMPAR_SYM_LIT ifFalse:[ self error:'not a symbol leaf' ].
  ^ litValue
%

!------------------------------------------------------------
set class GsComVarLeaf

category: 'Documentation'
classmethod:
comment
^
'a GsComVarLeaf   represents a method arg or temp, a block arg or temp,
                an an instance variable , or a literal variable,
                or a reference to self or super

  varName ,  a Symbol  , the name of the variable
  litVarAssoc , an Association , non-nil only for a literal variable
  varKind    , a SmallInteger, a ComParVarEType
  lexLevel   , a SmallInteger,  the lexical level in terms of block scoping ,
         0 means the home method, 1 is first block level 
         parser increments it''s lexLevel when entering both normal and in-lined blocks

  varOffset  is -1 for a Ruby dynamic instVar
                 0 for self or super
                 0 for a literal variable
                 zero-based offset into the instVars for an instance variable,
                 zero-based offset into the method or block args for an arg ,
                 0 for a method or block temp ( generator will assign an offset)

  for a Smalltalk literal variable,  varName is the key in the Association

  The parser is responsible for canonicalizing all references to a given
  arg, temp or instVar  within a method to be references to the same  
  GsComVarLeaf.
  Thus multiple GsComVariableNode''s may reference a single GsComVarLeaf,
  within a single compilation.
'
%
category: 'Printing'
method:
printFormattedOn: aStream

  super printOn: aStream .
  aStream print:' varName: ' symbol: varName ;
      nextPutAll: ' varKind:' ; nextPutAll: self varKindString ;
      print: ' lexLevel:' int: lexLevel ; 
      print: ' varOffset:' int: varOffset ; 
      nextPut: $) ; cr .
%
classmethod:
varKindToString: aKind
  aKind == COMPAR_BLOCK_ARG_VAR ifTrue:[ ^ 'BLOCK_ARG'].
  aKind == COMPAR_BLOCK_TEMP_VAR ifTrue:[ ^ 'BLOCK_TEMP'].
  aKind == COMPAR__INST_VAR ifTrue:[ ^ 'INST_VAR'].         
  aKind == COMPAR_LIT_VAR ifTrue:[ ^ 'LIT_VAR'].        
  aKind == COMPAR_METHOD_ARG_VAR ifTrue:[ ^ 'METHOD_ARG'].
  aKind == COMPAR_METHOD_TEMP_VAR ifTrue:[ ^ 'METHOD_TEMP'].
  aKind == COMPAR_METH_VC_GLOBAL ifTrue:[ ^ 'METH_VC_GLOBAL'].
  aKind == COMPAR_SELF_VAR ifTrue:[ ^ 'SELF'].
  aKind == COMPAR_SUPER_VAR ifTrue:[ ^ '_SUPER'].
  "COMPAR_LIT_VAR_SPECIAL_LITERAL not legal, from dbf conversion only"
  ^ 'INVALID_VAR'
%
method:
varKindString
  ^ self class varKindToString: varKind
%


category: 'Instance creation'
classmethod:
new
"create an instance registered with VM for finalization of cData"

<primitive: 674>
self _primitiveFailed: #new
%

category: 'Instance Initialization'
method:
setIRnodeKind
  kind :=  COMPAR_VAR_LEAF
%

method:
setVarName: aName
  varName := aName .
%

method:
methodTemp: aSymbol 
  "code generator will delete method temps from
   the method if there are no IR references other than the definition"
  self setIRnodeKind .
  self setVarName: aSymbol .
  varOffset := 0 .  "generator will assign offsets for temps"
  varKind := COMPAR_METHOD_TEMP_VAR  .
  lexLevel := 0
%

method:
methVcGlobal: aSymbol 
  "code generator will allocate a slot for this even if the definition
   is the only reference in the IR."
  self setIRnodeKind .
  self setVarName: aSymbol .
  aSymbol == #'$~' ifTrue:[ varOffset := 0 ] 
    ifFalse:[ aSymbol == #'$_' ifTrue:[ varOffset := 1  ]
      ifFalse:[  aSymbol == #'__lexPath' ifTrue:[ varOffset := 2 ]
        ifFalse:[ self error:'invalid arg to methVcGlobal:']]].
  varKind := COMPAR_METH_VC_GLOBAL  .   
  lexLevel := 0
%

method:
initializeSelf
  self setIRnodeKind .
  varName :=  #self .
  varOffset := 0  .
  varKind := COMPAR_SELF_VAR .  
  lexLevel := 0 .
%
method:
initializeSuper
  self setIRnodeKind .
  varName :=  #super .
  varOffset := 0 .
  varKind := COMPAR_SUPER_VAR .  
  lexLevel := 0 .
%

method:
blockTemp: tempNameSymbol sourceLexLevel: aLevel
  "code generator will delete block temps from
   the method if there are no IR references other than the definition"
  aLevel < 1 ifTrue:[ aLevel error:'out of range'].  
  self setIRnodeKind.
  tempNameSymbol _isSymbol ifFalse:[ self error:'expected a symbol'].
  self setVarName: tempNameSymbol .
  varOffset := 0 . "generator will assign offsets for temps"
  varKind := COMPAR_BLOCK_TEMP_VAR .
  lexLevel := aLevel 
%  

method:
methodArg: argNameSymbol argNumber: oneBasedArgNum
  self setIRnodeKind.
  argNameSymbol _isSymbol ifFalse:[ self error:'expected a symbol'].
  self setVarName: argNameSymbol .
  oneBasedArgNum < 1 ifTrue:[ oneBasedArgNum error:'out of range'].
  varOffset :=  oneBasedArgNum - 1 . "convert to zero based"
  varKind := COMPAR_METHOD_ARG_VAR.
  lexLevel := 0
%
method:
blockArg: argNameSymbol argNumber: oneBasedArgNum forBlock: aGsComBlockNode 
  self setIRnodeKind.
  argNameSymbol _isSymbol ifFalse:[ self error:'expected a symbol'].
  self setVarName: argNameSymbol .
  oneBasedArgNum < 1 ifTrue:[ oneBasedArgNum error:'out of range'].
  varOffset :=  oneBasedArgNum - 1 . "convert to zero based"
  varKind := COMPAR_BLOCK_ARG_VAR .
  lexLevel := aGsComBlockNode lexLevel
%
method:
literalVariable: anAssociation
  "Smalltalk style global variable, class variable, etc .
   Hopefully usable for Ruby globals ? "
  | key |
  self setIRnodeKind.
  key := anAssociation key .
  key _isSymbol ifFalse:[ key error:'bad arg kind'].
  self setVarName:  key .
  litVarAssoc := anAssociation .
  (anAssociation isKindOf: SymbolAssociation) ifFalse:[
    self error:'arg to literalVariable: is not a SymbolAssociation'
  ].
  varOffset := 0 .
  varKind := COMPAR_LIT_VAR . 
%
method
instanceVariable: ivNameSymbol ivOffset: oneBasedIvOfs
  "Parser must lookup instVar names in class definitions to determine the
   oneBasedIvOfs.  Alternatively, additional behavior could be provided here to
   take an ivName and a Class and do the lookup here ..."  
  self setIRnodeKind.
  self setVarName: ivNameSymbol .
  varKind := COMPAR__INST_VAR.
  oneBasedIvOfs > 0 ifTrue:[
    "instVar at known offset, convert to zero based"
    varOffset := oneBasedIvOfs - 1. 
  ] ifFalse:[
    oneBasedIvOfs == -1 ifTrue:[
      varOffset := -1 "ruby dynamic instVar"
    ] ifFalse:[
      oneBasedIvOfs error:'out of range'
    ]
  ].
%
category: 'Accessing'
method:
varOffset
  ^ varOffset
%

method:
varName
  ^ varName
%
method:
varKind
  ^ varKind
%
method:
lexLevel
  ^ lexLevel
%
method:
litVarValue
  varKind == COMPAR_LIT_VAR ifFalse:[ self error:'not a literal variable'].
  ^ litVarAssoc _value
%

category: 'Querying'
method
isTemp
  "return true if varKind one of 
    COMPAR_METHOD_TEMP_VAR COMPAR_BLOCK_TEMP_VAR, COMPAR_METH_VC_GLOBAL"

  ^ varKind <= COMPAR_METH_VC_GLOBAL
%
method
isArg
  ^ varKind == COMPAR_METHOD_ARG_VAR or:[ varKind == COMPAR_BLOCK_ARG_VAR]
%

category: 'Instance Initialization'
!------------------------------------------------------------
set class GsComSelectorLeaf

category: 'Documentation'
classmethod:
comment
^ 
' A GsComSelectorLeaf represents the selector used in a message send.
  instVars are
    selector,  a Symbol 
    specialOpcode , a SmallInteger 
    specialSendClass , a Behavior  

  If specialOpcode is non-zero the send may be optimizable
  to a special bytecode.  We could have the generator (comgen.c) make
  this determination; currently  the parser determines what is optimizable.
  example:    ==  sent to any Smalltalk object is a special bytecode ,
                not a normal send .

  The parser is disallowing redefinition of some methods in the base
  Smalltalk classes if you are not logged into the Repository as SystemUser .

  Some selector leafs may be initialized at start of parsing and kept
  in a dictionary of special sends, etc.  These instances will have srcOffset == 1.

  For the optimizable sends,  specialSendClass
    can be Object  for a send such as     ==
    can be  ExecBlock  for sends like   value:
    can be SmallInteger for  sends like    +     , etc .
  otherwise specialSendClass is nil .
'
%
category: 'Printing'
method:
printFormattedOn: aStream

  super printOn: aStream .
  aStream nextPutAll: selector printString ; 
      print: ' specialOpcode:' int: specialOpcode ; 
      nextPutAll: ' specialSendClass:' ; 
      nextPutAll:(specialSendClass ~~ nil ifTrue:[ specialSendClass name] ifFalse:['nil']) ; 
      nextPut: $) .
%

category: 'Instance Initialization'
method:
setIRnodeKind
%

category: 'Initialization'
classmethod: 
_initializeSpecialSelectors
  | specSendsDict specialISAdict objectSpecSends data |

  " syms are the ruby or Smalltalk selectors that will be sent for
    the specified bytecodes.  comparse.c takes care of
    optimizations for Smalltalk sends of those bytecodes, when compiling
    Smalltalk source. "

  "each triple  in data Array is  
       1. selector to be optimized 
                   (either a Ruby or Smalltalk selector from AST to IR phase)
       2. special send bytecode to use ,   
       3. class on which special send is defined 
       4. environment in which selector is a special send on Object, 
           nil means all environments.
   special sends not defined on class Object get a send-site cache
   during bytecode generation and may fall back to real method"
  		"ruby_selector_suffix dependent"
  data := { 
  { #'+#1__' . Bc_SEND_SPECIAL_PLUS_u1_u32 . SmallInteger . 1 } .   "for Ruby"
  { #'-#1__' . Bc_SEND_SPECIAL_MINUS_u1_u32 . SmallInteger . 1 } . 
  { #'*#1__' . Bc_SEND_SPECIAL_MULTIPLY_u1_u32 . SmallInteger . 1 } . 
  { #'>=#1__' . Bc_SEND_SPECIAL_GTE_u1_u32 . SmallInteger . 1 } . 
  { #'<=#1__' . Bc_SEND_SPECIAL_Lte_u1_u32 . SmallInteger . 1 } . 
  { #'<#1__'  . Bc_SEND_SPECIAL_LT_u1_u32 . SmallInteger . 1 } . 

  { #'+' . Bc_SEND_SPECIAL_PLUS_u1_u32 . SmallInteger . 0 } .   "for Smalltalk"
  { #'-' . Bc_SEND_SPECIAL_MINUS_u1_u32 . SmallInteger . 0 } . 
  { #'*' . Bc_SEND_SPECIAL_MULTIPLY_u1_u32 . SmallInteger . 0 } . 
  { #'>=' . Bc_SEND_SPECIAL_GTE_u1_u32 . SmallInteger . 0 } . 
  { #'<=' . Bc_SEND_SPECIAL_Lte_u1_u32 . SmallInteger . 0 } . 
  { #'<'  . Bc_SEND_SPECIAL_LT_u1_u32 . SmallInteger . 0 } . 

  { #'==' . Bc_SEND_SPECIAL_EQEQ . Object .   0 } . 
  { #'~~' . Bc_SEND_SPECIAL_NENE . Object .   0  } . 

  { #'_equal?#1__' . Bc_SEND_SPECIAL_EQEQ . Object . 1 } .  "Ruby identity compare"
  { #'_not_equal?#1__' . Bc_SEND_SPECIAL_NENE . Object . 1 } .  "Ruby identity compare"
  { #'_is_a?#1__' . Bc_SEND_SPECIAL_RUBYKINDOF_u1 . Object .  1 } .  "Ruby instance of"
  { #'_kind_of?#1__' . Bc_SEND_SPECIAL_RUBYKINDOF_u1 . Object . 1 } . "Ruby kindOf"
  { #'call#0__' . Bc_SEND_CALL_u1_u32 . ExecBlock . 1 } . 
  { #'call#1__' . Bc_SEND_CALL_u1_u32 . ExecBlock . 1 } . 
  { #'call#2__' . Bc_SEND_CALL_u1_u32 . ExecBlock . 1 } . 
  { #'call#3__' . Bc_SEND_CALL_u1_u32 . ExecBlock . 1 } . 
  { #'call#0*_' . Bc_SEND_CALL_STAR_u1_u32 . ExecBlock . 1 } . 
  " any call variation with & does not use  Bc_SEND_CALL (1.8.7) "
  { #'_not#0__' . Bc_rubyNOT . Object . Object . 1 } .  
  { #'_isInteger#0__' . Bc_SEND_SPECIAL_IS_INTEGER . Object . 1 } . 
  { #'_isSmallInteger#0__' . Bc_SEND_SPECIAL_IS_SMALLINT . Object . 1 } . 
  { #'_isFixnum#0__' . Bc_SEND_SPECIAL_IS_SMALLINT . Object . 1 } . 
  { #'_isNumeric#0__' . Bc_SEND_SPECIAL_IS_NUMBER . Object . 1 } . 
  { #'_isFloat#0__' . Bc_SEND_SPECIAL_IS_FLOAT . Object . 1 } . 
  { #'_isSymbol#0__' . Bc_SEND_SPECIAL_IS_SYMBOL . Object . 1 } . 
  { #'_isExecBlock#0__' . Bc_SEND_SPECIAL_IS_ExecBlock . Object . 1 } . 
  { #'_isBlock#0__' . Bc_SEND_SPECIAL_IS_ExecBlock . Object . 1 } . 
  { #'_isArray#0__' . Bc_SEND_SPECIAL_IS_Array . Object . 1 } . 
  { #'_isStringOrSymbol#0__' . Bc_SEND_SPECIAL_IS_OneByteString . Object . 1 } . "used in Ruby"
  { #'_isString#0__' . Bc_SEND_SPECIAL_IS_RubyString . Object .         1 } .  "used in Ruby"
  { #'_isHash#0__' . Bc_SEND_SPECIAL_IS_RubyHash . Object .        1 } . "used in Ruby"
  { #'_isRegexp#0__' . Bc_SEND_SPECIAL_IS_Regexp . Object } . 
  { #'_isRange#0__' . Bc_SEND_SPECIAL_IS_Range . Object } .

  { #_isOneByteString . Bc_SEND_SPECIAL_IS_OneByteString . Object . 0 } . "used in Smalltalk"
  { #_isExceptionClass . Bc_SEND_SPECIAL_IS_ExceptionClass . Object . 0 } . 
  { #_isNumber . Bc_SEND_SPECIAL_IS_NUMBER . Object . 0 } . 
  { #_isScaledDecimal . Bc_SEND_SPECIAL_IS_ScaledDecimal . Object . 0 } . "Smalltalk"
  { #_isRubyString . Bc_SEND_SPECIAL_IS_RubyString . Object .     0 } .  "used in Smalltalk"
  { #_isRubyHash . Bc_SEND_SPECIAL_IS_RubyHash . Object    . 0 } . "used in Smalltalk"
  { #_isInteger . Bc_SEND_SPECIAL_IS_INTEGER . Object . 0 } .
  { #_isSmallInteger . Bc_SEND_SPECIAL_IS_SMALLINT . Object . 0 } .
  { #_isFloat . Bc_SEND_SPECIAL_IS_FLOAT . Object . 0 } .
  { #_isSymbol . Bc_SEND_SPECIAL_IS_SYMBOL . Object . 0 } .
  { #_isExecBlock  . Bc_SEND_SPECIAL_IS_ExecBlock . Object . 0 } .
  { #_isArray . Bc_SEND_SPECIAL_IS_Array . Object . 0 } .
  { #_isString . Bc_SEND_SPECIAL_IS_RubyString . Object .  0 } .
  { #_isRegexp . Bc_SEND_SPECIAL_IS_Regexp . Object . 0 } .
  { #_isRange . Bc_SEND_SPECIAL_IS_Range . Object }

  }.
  objectSpecSends := IdentityKeyValueDictionary new .
  specSendsDict := IdentityKeyValueDictionary new .
  data do:[:triple | | sym bc cls |
    sym := triple at: 1 . bc := triple at: 2 .  cls := triple at: 3 .
    specSendsDict at: sym put: { bc . cls } .
    cls == Object ifTrue:[ | env |
      env := triple atOrNil: 4 .
      env ifNil:[ env := -1 ].
      objectSpecSends at: sym put: env .
    ].
  ].
  objectSpecSends immediateInvariant .
  specSendsDict immediateInvariant .

  specialISAdict := IdentityKeyValueDictionary new .
  specialISAdict   "env 0 selectors used in Ruby coerce_to logic"
        at: Symbol put: #( _isSymbol Symbol ) ;
        at: SmallInteger put: #(  _isSmallInteger  Fixnum ) ;
        at: Float put: #(  _isFloat  Float ) ;
        at: Integer put: #(  _isInteger  Integer ) ;
        at: Number put: #(  _isNumber  Number ) ;
        at: ExecBlock put: #(  _isExecBlock  ExecBlock ) ;
        at: Array put: #(  _isArray  Array ) ;
        at: String put: #(  _isRubyString  String ) ;
        at: RubyHash put: #(  _isRubyHash  Hash ) ;
        at: Regexp put: #(  _isRegexp  Regexp ) ;
        at: Range put: #(  _isRange  Range )  .
  specialISAdict immediateInvariant .

  #( #SpecialSendsDict #SpecialISAselectors #SpecialObjectSends ) do:[:sym|
    self _removeClassVar: sym ifAbsent:[].
  ].
  self  _addInvariantClassVar: #SpecialSendsDict value: specSendsDict ;
    _addInvariantClassVar: #SpecialISAselectors value: specialISAdict ;
    _addInvariantClassVar: #SpecialObjectSends value: objectSpecSends .
%

run
GsComSelectorLeaf _initializeSpecialSelectors . 
true
%

classmethod:
reimplementationAllowed: aSymbol inEnv: anInt
  "returns true if reimplementation is allowed in specified environmentId"
| env |
env := SpecialObjectSends at: aSymbol otherwise: nil .
env ifNotNil:[
  env < 0 ifTrue:[ ^ false "disallowed in all environments"].
  ^ anInt ~~ env 
].
^ true
%

classmethod:
newSelector: aSymbol env: envId
  | entry |
  entry :=  SpecialSendsDict at: aSymbol otherwise: nil .
  entry ifNil:[
    ^ aSymbol " a non-optimized send"
  ] ifNotNil:[ | entryEnv |
    entryEnv := entry atOrNil: 4 .
    (entryEnv ~~ nil and:[ entryEnv ~~ envId]) ifTrue:[
       ^ aSymbol "not optimized in this environment"
    ].
    ^ self _basicNew _init: entry sym: aSymbol
  ]
%
    
method:
_init: specialSendsEntry sym: aSymbol
  kind := COMPAR_SELECTOR_LEAF .
  selector := aSymbol .
  specialOpcode := specialSendsEntry at: 1 .
  specialSendClass := specialSendsEntry at: 2 .
  ^ self
%

classmethod:
selectorForSuper: aSymbol
  "cannot optimize a send to super "
  ^ aSymbol
%

category: 'Accessing'
method:
selector
  ^ selector
%

classmethod:
classToISAselector: aClass
  "Return the special selector to use in optimizing Ruby coerce_to 
   for coercion to aClass .  Return nil if aClass does not have
   a special _is*  selector. "
  | val |
  val := SpecialISAselectors at: aClass otherwise: nil .
  val == nil ifFalse: [ val := val at: 1 ] .
  ^ val
%

classmethod:
classToRubyClassName: aClass
  "Return the ruby class name to use in optimizing Ruby coerce_to 
   for coercion to aClass .  Return nil if aClass does not have
   a special _is*  selector. "
  | val |
  val := SpecialISAselectors at: aClass otherwise: nil .
  ^ (val == nil) ifTrue: [ aClass name ] 
                 ifFalse: [ val at: 2 ] .
%

!------------------------------------------------------------
set class GsComArrayBuilderNode

category: 'Documentation'
classmethod:
comment
^
'A GsComArrayBuilderNode represents a non-literal array construction ,
 example:  { 1 , 2 , 3 }  

 instVars
   elements, an Array , 
          each element is a non-leaf GsCompilerIRNode
           for an expression which produces an element of the array.

 The generator emits code to execute each element expression, pushing
 each element on the stack and then emits an array constructor bytecode.
'
%

category: 'Instance creation'
classmethod:
_basicNew
"create an instance registered with VM for finalization of cData"

<primitive: 674>
self _primitiveFailed: #_basicNew
%

classmethod:
new
^ self _basicNew initialize
%

category: 'Printing'
method:
printFormattedOn: aStream

  super printOn: aStream .
  aStream print:' count:' int: elements size ; 
      do: elements ;
  nextPut: $) ; cr .
%


category: 'Instance Initialization'

classmethod:
new
  ^ self _basicNew initialize
%

classmethod:
with: aNode
 | res |
 (res := self new) appendElement: aNode .
  ^ res
%

method:
initialize
  kind := COMPAR_ARRAY_BUILDER_NODE .
  elements := { } .
%


method:
appendElement: aGsCompilerIRNode
  aGsCompilerIRNode ifNil:[ self error:'invalid nil arg to GsComArrayBuilderNode'].
  elements addLast: aGsCompilerIRNode
%

method:
size
  ^ elements size
%

!------------------------------------------------------------
set class GsComAssignmentNode

category: 'Documentation'
classmethod:
comment
^
'A  GsComAssignmentNode represents an assignment operator , Smalltalk := 
  
    dest  ,  a GsComVarLeaf ,  the destination variable or temp 
    source , a non-leaf GsCompilerIRNode , the source expression
    assignKind , a SmallInteger , 0 = normal, 1 = method arg default value
'
%
category: 'Printing'
method:
printFormattedOn: aStream

  super printOn: aStream .
  aStream print:' assignKind:' int: assignKind ; cr ; 
  nextPut: $( ; indentMore .
     dest printFormattedOn: aStream .
  aStream  nextPutAll:') := (' .
     source printFormattedOn: aStream .
  aStream indentLess ; nextPutAll: '))'  ; cr .
%

category: 'Instance Initialization'

method:
dest: aGsComVarLeaf source: sourceNode
    "returns receiver."
  kind := COMPAR_ASSIGNMENT_NODE .
  dest := aGsComVarLeaf .
  source := sourceNode .
  assignKind := 0 .
  sourceNode ifNil:[ self error:'illegal nil source for assignment'].
  aGsComVarLeaf ifNil:[ self error: 'illegal nil destination for assignment'].
%
method:
setMethodArgDefault
  assignKind := 1
%

!------------------------------------------------------------
set class GsComMethNode

category: 'Documentation'
classmethod:
comment
^
'A GsComMethNode represents a single method in the Smalltalk sense.
 It can be an anonymous method, as for a  doit .  For each GsComMethNode,
 A GsComMethNode is the root node of the IR graph for each invocation of 
 the code generator .
 The generator emits one or more instances of GsNMethod, one for the
 home method of this node, and one for any non-inlined blocks within the method.
 The instVars are

    selector, a Symbol , or nil for an anonymous method.
    theClass, a Behavior, class in which method is to be an instanceMethod,
                may be nil for an anonymous method.  Note generator does not
                install method in method dictionary.
    arguments, an Array , each element is a GsComVarLeaf
    temps    , an Array , each element is a GsComVarLeaf
    body     , an Array , each element is a GsCompilerIRNode for a statement 
    methInfo , a SmallInteger , 0 for a ruby method .
        bit masks of the fields are  
            requiresVc 0xFF (not used by smalltalk, 0=no, 1=yes, 
              2=force all temps to be allocated in Vc, and copy all args to VC)
            protectedType 0xFF00 (Smalltalk base image code security)    
            primNumber 0xFFFF0000 (short, a Smalltalk primitive number)
    rubyInfo , a SmallInteger 
        bit masks of the fields are  (per comparse.ht)
		envId        16rFF default env for sends in this method
		isRuby      16r100 (1 in Ruby, 0 in smalltalk)
		isPrivate   16r200            
		isProtected 16r400
		isEval      16r800  
          lineNumBias 16rFFFFFF000
          isBridge   16r1000000000
                   16rFF0000000000  defaultReturnArgNumber (1-based)
        If isBridge bit is true, then a bridge method is generated,
        and except for sends of variants of #call within bridge methods for
        ExecBlock, all message sends other than to super will use SEND_CURRENT
        bytecode.
        If defaultReturnArgNumber is non zero, then default return value
        is that argument instead of value of the last statement.
    rubyOptArgsBits, a SmallInteger with bits  describing which args
        in a ruby method have explicitly coded default value initializers,
        maximum of 61 args to such a method.
    nonBridgeSelector,   a Symbol or nil, 

    source, a String, the source code for the method, to be saved in the
          debug information for the generated method.

    fileName, a String, name of a Ruby source file , or nil.

    endSrcOffset,  1-based, if non-nil defines the end srcOffset within a Ruby 
       source file for this method, and beginning source offset
       defined by this nodes" srcOffset maybe > 1 , and sourceString
       is expected to be for an entire file.

    lineNumber is one-based .
      For Smalltalk IR trees generated by comparse.c , lineNumber is
      for use in debugging the compiler, and debug info is generated from
      the srcOffset only.
      For Ruby IR trees ,
      lineNumber in a GsComMethNode is used to generate a comment
      in the source string for the method about where in the source file
      it came from.  The sourceString in the GsComMethNode may be a source
      string for an entire file, and the generator will carve out of that source
      string the bytes from MethNode.srcOffset to methNode.endSrcOffset ,
      and append a comment with  methNode.lineNumber and methNode.fileName
      to the end of the carved out string.
'
%

category: 'Printing'
method:
printFormattedOn: aStream
  super printOn: aStream .
  aStream nextPutAll: ' file:' ; nextPutAll: self fileNameForPrint ;
    print:'selector: ' symbol: selector ;
    nextPutAll:' theClass: '; 
      nextPutAll: (theClass ~~ nil ifTrue:[ theClass name] ifFalse:['nil '])  ; 
    nextPutAll:' methInfo:'; nextPutAll: '16r'; nextPutAll: methInfo asHexString ; cr ;
    nextPutAll:' rubyInfo:'; nextPutAll: '16r'; nextPutAll: rubyInfo asHexString ; cr ;
    nextPutAll:'args:' ; do: arguments ; 
    nextPutAll:'temps:' ; do: temps ;
    nextPutAll:'body:' ; do: body ;
    nextPut: $) ; cr .
%
method: 
summary
  | str |
  str := String new .
  str addAll:  ' file:' ; addAll: self fileNameForPrint ;
    addAll: ' line ' ; addAll: lineNumber asString ;
    addAll: ' selector: ' ; addAll: selector  .
  ^ str
%
method:
fileNameForPrint
  | sz res |
  fileName == nil ifTrue:[ ^ 'nil' ].
  (res := String new) add: $' .
  (sz := fileName size) > 25 ifTrue:[
    res addAll:( fileName copyFrom: sz - 25 to: sz ) 
  ] ifFalse:[
    res addAll: fileName .
  ] .
  res add: $' .
  ^ res
%
category: 'Accessing'

classmethod: 
maxRubyFixedArgs
  ^ GEN_MAX_RubyFixedArgs
%

method:
fileName
  ^ fileName
%
method:
selector
  ^ selector
%

category: 'Instance Initialization'

category: 'Accessing'
method:
arguments
  ^ arguments
%

category: 'Instance creation'
classmethod:
new

"disallowed , use newRuby or newSmalltalk"
self shouldNotImplement: #new
%


classmethod:
newRuby
  ^ self _basicNew initialize setRuby
%
classmethod:
newSmalltalk
  ^ self _basicNew initialize setSmalltalk
%

method:
initialize
  kind := COMPAR_METHOD_NODE .
  arguments :=  { } .
  temps :=  { } .
  body := { } .
  "nonBridgeSelector left as nil"
  rubyInfo := 0 .
  rubyOptArgsBits := 0 .
%  
method:
fileName: aString
  fileName := aString 
%
method:
fileName: nameString source: srcString
  fileName := nameString .
  source := srcString
%
method:
endSourceOffset: anOffset
  endSrcOffset := anOffset
%  
method:
setRuby
  methInfo := 16rFFFF0000 . "primNumber:=-1, protected:=0,reqVc:=0"
  rubyInfo := rubyInfo bitOr: Ruby_mask . "isRuby:= 1 "
%


method:
setRubyLineNumberBias: anInt
   " field in rubyInfo with mask,   lineNumBias 16rFFFFFF000 "
   " intended to be called only once for each instance of GsComMethNode"
  anInt ~~ 0 ifTrue:[
    rubyInfo := rubyInfo bitOr: ((anInt bitAnd: 16rFFFFFF) bitShift: 12)
  ]
%
  
method:
setRubyBridge
  rubyInfo := rubyInfo bitOr: IsBridge_mask  
%
method:
setRubyPrivate
  rubyInfo := rubyInfo bitOr: IsPrivate_mask  
%
method:
setRubyProtected
  rubyInfo := rubyInfo bitOr: IsProtected_mask  
%
method:
setRubyEval
  rubyInfo := rubyInfo bitOr: IsRubyEval_mask
%

method:
forceAllArgsTmpsToVc
  "set requiresVc bits to 2 "
  methInfo := methInfo bitOr: 16r2
%

method:
methodProtection
  "return the ruby method protection bits, 
   result  0==public,  1==protected, 2==private  "

  ^ (rubyInfo bitAnd: 16r600) bitShift: -9
%

method:
addMethodProtection: anInt
  "used in building IR to transfer default private/protected from
   current class to this method node, if method not automatically
   private (like  initialize method)"
  | prot mask newBits iv |
  mask := 16r600 .
  iv := rubyInfo . 
  prot := (iv bitAnd: mask) bitShift: -9 . "inline methodProtection"
  anInt > prot ifTrue:[
    (anInt >= 0 and:[anInt <= 2]) ifFalse:[
       ArgumentError signal:'invalid method protection argument'
    ].
    newBits := anInt bitShift: 9 .
    rubyInfo := (iv bitAnd: (mask bitInvert))  bitOr: newBits
  ].
%

method:
setSmalltalk
  methInfo := 16rFFFF0000 . "primNumber:=-1, protected:=0,reqVc:=0"
  rubyInfo := 0 .
%
classmethod:
checkRubyInfoMasks
 "check classVars defined by bom.c against constants hardcoded in
  this class."
 (IsPrivate_mask bitOr: IsProtected_mask) == 16r600 ifFalse:[ self halt ].
 ^ true
%
run
GsComMethNode checkRubyInfoMasks
%

method:
selector: aSymbol
  "different implementation(s) in .mcz"
  (GsComSelectorLeaf reimplementationAllowed: aSymbol inEnv: self envId) ifFalse:[
      RubyScriptError signal: 'reimplementation of special selector ' , aSymbol,
                ' not allowed , near line ' , lineNumber asString , ' of ' , fileName asString .
  ].
  selector := aSymbol 
%
method:
envId
  ^ rubyInfo bitAnd: Env_mask
%

method:
environment: anInteger
  (anInteger < 0 or:[ anInteger > 255]) ifTrue:[ 
     anInteger error:'out of range'
  ].
  rubyInfo := (rubyInfo bitAnd:( Env_mask bitInvert)) bitOr: anInteger 
%
method:
class: aBehavior
  theClass := aBehavior
%
method:
appendArg: aGsComVarLeaf
  aGsComVarLeaf varKind == COMPAR_METHOD_ARG_VAR ifFalse:[
    self error: 'bad arg kind ', aGsComVarLeaf varKindString 
  ].
  arguments addLast: aGsComVarLeaf
%
method:
appendTemp: aGsComVarLeaf
  | knd |
  knd := aGsComVarLeaf varKind .
  (knd == COMPAR_METHOD_TEMP_VAR or:[ knd == COMPAR_METH_VC_GLOBAL]) ifFalse:[
    self error: 'bad arg kind ', aGsComVarLeaf varKindString 
  ].
  temps addLast: aGsComVarLeaf
%
method:
appendStatement: aNode
  aNode sourceOffset ifNil:[ | bsiz |
    (bsiz := body size) > 0 ifTrue:[
      aNode sourceOffset:( (body at: bsiz) sourceOffset).
    ]
  ].
  body addLast: aNode .
  srcOffset ifNil:[ | ofs |
    ofs := aNode sourceOffset .
    ofs ifNotNil:[ srcOffset := ofs ] 
        ifNil:[ lineNumber ifNil:[ lineNumber := aNode lineNumber]]. 
  ].
%
method: 
lastAsRubyReturn 
  | last bdy bdySize rtn sofs defRtn |
  bdy := body .
  bdySize := bdy size.
  last := bdy atOrNil: bdySize .
  (defRtn := rubyInfo bitAnd: DefaultReturnArgNum_mask) ~~ 0 ifTrue:[
    last class == GsComReturnNode ifFalse:[ | argNum |
      argNum := defRtn bitShift: 0 - DefaultReturnArgNum_shift .
      rtn := GsComReturnNode new return: 
	     (GsComVariableNode _basicNew leaf: (arguments at: argNum)) .
      sofs := last ifNotNil:[ last sourceOffset ] ifNil:[ self sourceOffset].
      sofs ifNotNil:[ rtn sourceOffset: sofs ].
      self appendStatement: rtn
    ]
  ] ifFalse:[
    last ifNil:[
      last := GsComLiteralNode newNil returnNode .
      bdy at: 1 put: last .
    ] ifNotNil:[ 
      rtn := last returnNode .
      (sofs := last sourceOffset) ifNotNil:[ rtn sourceOffset: sofs ].
      bdy at: bdySize put: rtn 
    ].
  ].
%

method:
insertFirstStatement: aNode
  body insertAll: { aNode } at: 1 .
%

method
source: aString
  source := aString
%

method
optArgsBits: aSmallInt
  aSmallInt _isSmallInteger ifFalse:[ aSmallInt error:'invalid arg'].
  rubyOptArgsBits := aSmallInt
%
  
method:
nonBridgeSelector: aSymbol
  nonBridgeSelector := aSymbol
%
method: 
rubyDefaultReturnArg: argNum
  rubyInfo := rubyInfo bitOr: 
    ((argNum bitShift: DefaultReturnArgNum_shift) bitAnd: DefaultReturnArgNum_mask)
%

!------------------------------------------------------------
set class GsComBlockNode

category: 'Documentation'
classmethod:
comment
^
'A GsComBlockNode represents a block within a method. It may be
 either a real block or a block that will be inlined by the parser.

   lexLevel, a SmallInteger > 0, lexical level of the block within the
                source of the home method, without regard to inlining. 
   lastSrcOffset,  end of the source for the block within the method,
          a byteOffset if srcOffset is non-nil, otherwise a line number.
   blkKind, a SmallInteger,  a ComBlockEKind , always 0 for Ruby
   args, each element is a GsComVarLeaf
   lastArgInfo , a SmallInteger
   temps, an Array , each element is a GsComVarLeaf
   statements ,an Array or OrderedCollection ,  the body of the block
   terms  , an Array, used only for Gemstone select block, {:x| ... }
'
%
category: 'Printing'
method:
printFormattedOn: aStream
  super printOn: aStream .
  aStream print:' lexLevel:' int: lexLevel ; 
    print:' lastArgInfo:' int: lastArgInfo ; cr ;
    nextPutAll:' args:'; do: args ; 
    nextPutAll:' temps:'; do: temps ; 
    nextPutAll:' statements:'; do: statements ; 
  nextPut: $) ; cr .
%

category: 'Instance creation'
classmethod:
new
"create an instance registered with VM for finalization of cData"

<primitive: 674>
self _primitiveFailed: #new
%

category: 'Instance Initialization'

method:
lexLevel: aLevel
"the primary initialization method"
  kind := COMPAR_BLOCK_NODE.
  aLevel < 0 ifTrue:[ aLevel error:'out of range' ].
  lexLevel := aLevel .
  blkKind := 0 .
  args := { } .
  temps := { } .
  statements := { } .
  lastArgInfo := 0 .
  "terms left as nil"
%

method:
setLastArgStar
  "filter out anonymous star here,   {|*| }  equivalent to { }  "
  args size ~~ 0 ifTrue:[ lastArgInfo := lastArgInfo bitOr: LastArgStar_mask ].
%

method:
setAmpersandArg
  lastArgInfo := lastArgInfo bitOr: HasBlockArg_mask 
%

method:
setNoArgsDecl
  lastArgInfo := lastArgInfo bitOr: NoDeclaredArgs_mask  .
%

method:
lastSourceOffset: anOffset 
  lastSrcOffset := anOffset
%

method:
lastLineNumber: anInt
  | first last |
  last := anInt .
  srcOffset ifNotNil:[ self error:'should use lastSourceOffset:' ].
  (first := lineNumber) ifNil:[ 
    lineNumber := anInt 
  ] ifNotNil:[ 
    anInt < first ifTrue:[ 
      statements do:[ :aNode | | num |
        (num := aNode lineNumber) ifNotNil:[
           num < first ifTrue:[ first := num ].
           num > last ifTrue:[ last := num ].
        ]
      ].
      lineNumber := first.
    ]
  ].
  lastSrcOffset := last  "store a line number"
%

method:
appendArg: aGsComVarLeaf
  | argKnd needSynth |
  needSynth := false .
  (argKnd := aGsComVarLeaf varKind) == COMPAR_BLOCK_ARG_VAR ifTrue:[
    (aGsComVarLeaf lexLevel < lexLevel and:[ COM_RUBY_COMPAT_LEVEL >= 18]) ifTrue:[
      "Fix Ticket 113, block arg escaping to outer block arg,
       Synthesize a blockArg node, and add an assignment to the
       outer arg"
      needSynth := true 
    ] ifFalse:[
      args addLast: aGsComVarLeaf.      "normal block arg"
      ^ self .
    ].
  ].
  ((argKnd == COMPAR_METHOD_TEMP_VAR 
      or:[ argKnd == COMPAR_BLOCK_TEMP_VAR
        or:[ argKnd == COMPAR_METHOD_ARG_VAR] ]) 
    and:[ aGsComVarLeaf lexLevel < lexLevel 
        and:[ COM_RUBY_COMPAT_LEVEL >= 18 ]]) ifTrue:[
    "Fix for Ticket 21, ruby block args escaping to outer level.
     Synthesize a blockArg node, and add an assignment to the 
     outer temp"
    needSynth := true
  ].
  needSynth ifTrue:[
    | synthArg assnNod ags |  
    synthArg := GsComVarLeaf new blockArg: aGsComVarLeaf varName
                argNumber: (ags := args) size + 1
                forBlock: self .
    ags add: synthArg .
    assnNod := GsComAssignmentNode new dest: aGsComVarLeaf
                source: (GsComVariableNode new leaf: synthArg ).
    statements insertObject: assnNod at: 1 .
    ^ self
  ].
  self error: 'VarLeaf.varKind=', (GsComVarLeaf varKindToString: argKnd) ,
        ' illegal for a block arg'   
%
method:
appendMasgnDummyArg
  "for a Ruby block of the form  { | x, |  } append a dummy arg
   so the block-args processing will then do the right thing at runtime"
  | ags dummy |
  (ags := args) size == 1 ifFalse:[ self error:'before appendMasgnDummyArg, size not 1'].
  dummy := GsComVarLeaf new blockArg: #_dummyarg argNumber: 2 forBlock: self .
  ags add: dummy 
%

method:
appendTemp: aGsComVarLeaf
  aGsComVarLeaf varKind == COMPAR_BLOCK_TEMP_VAR ifFalse:[
    self error: 'bad arg kind ', aGsComVarLeaf varKindString 
  ].
  temps addLast: aGsComVarLeaf
%
method:
appendStatement: aNode
  aNode sourceOffset ifNil:[ | bsiz |
    (bsiz := statements size) ~~ 0 ifTrue:[
      aNode sourceOffset:( (statements at: bsiz) sourceOffset).
    ]
  ].
  statements addLast: aNode . ^
  srcOffset ifNil:[ | ofs |
    ofs := aNode sourceOffset .
    ofs ifNotNil:[ srcOffset := ofs ] 
        ifNil:[ lineNumber ifNil:[ lineNumber := aNode lineNumber]]. 
  ].
%
method:
_appendLastStatement: aNode
  "OBSOLETE"
  self appendStatement: aNode .
  srcOffset ifNotNil:[ 
    self lastSourceOffset: aNode lastSourceOffset
  ] ifNil: [ | num |
    num := aNode lastLineNumber .
    num ifNotNil:[ self lastLineNumber: num ].
  ]
%
method: 
_finishLastStatement
  "OBSOLETE"
  | stmts last |
  stmts := statements .
  last := stmts atOrNil: stmts size .
  last ifNotNil:[
    srcOffset ifNotNil:[
      self lastSourceOffset: last lastSourceOffset
    ] ifNil:[ | num |
      num := last lastLineNumber .
      num ifNotNil:[ self lastLineNumber: num ].
    ]
  ]
%

method: 
_computeLastLineNumber
  "OBSOLETE"
  | list sz |
  list := statements .
  srcOffset ifNotNil:[
    (sz := list size) _downTo: 1 do:[:n | | num |
      num := (list at: n) lastSourceOffset .
      num ifNotNil:[
         self lastSourceOffset: num .
         ^ self
      ].
    ].
    self lastSourceOffset: srcOffset .
    ^ self
  ].
  (sz := list size) _downTo: 1 do:[:n | | num |
      num := (list at: n) lastLineNumber .
      num ifNotNil:[
         self lastLineNumber: num .
         ^ self
      ].
  ].
  sz > 1 ifTrue:[ self error:'could not find a last source position'].
%

category: 'Accessing'
method: 
args
  ^ args
%
method:
numArgs
  ^ args size
%
method:
lexLevel
  ^ lexLevel
%


!------------------------------------------------------------
set class GsComCascadeNode

category: 'Documentation'
classmethod:
comment
^
'A GsComCascadeNode represents a Smalltalk style cascaded message send
 such as
   anObj m1 ; m2 ; m3 .

 instVars
    rcvr , a non-leaf GsCompilerIRNode
    sends, an Array , each element is a GsComSendNode 
'
%
category: 'Printing'
method:
printFormattedOn: aStream
  super printOn: aStream .
  aStream nextPutAll:' rcvr:'. rcvr printFormattedOn: aStream .
  aStream nextPutAll:' sends:'; do: sends ;
    nextPut: $) ; cr .
%

category: 'Instance Initialization'

method:
rcvr: aNode
  rcvr := aNode .
  sends := { } .
  kind := COMPAR_CASCADE_NODE
%

method:
appendSend: aNode
  sends addLast: aNode
%

!------------------------------------------------------------
set class GsComLiteralNode

category: 'Documentation'
classmethod:
comment
^ ' A GsComLiteralNode represents a push or load of the literal specifed
  by the leaf instVar

    leaf , a GsComLitLeaf 
'
%
category: 'Instance Creation'
classmethod:
newInteger: aSmallInt
 ^ self new leaf: (GsComLitLeaf new  integerLiteral: aSmallInt)
%


category: 'Instance Creation'
classmethod:
newObject: anObject
 ^ self new leaf: (GsComLitLeaf new  objectLiteral: anObject)
%

classmethod:
newConstantRef: aRubyConstantRef
 ^ self new leaf: (GsComLitLeaf new  constRefLiteral: aRubyConstantRef)
%

classmethod:
newNil
  ^ self new leaf: (GsComLitLeaf new specialLiteral: nil)
%
classmethod:
newRemoteNil
  ^ self new leaf: (GsComLitLeaf new specialLiteral: _remoteNil)
%
classmethod:
newFalse
  ^ self new leaf: (GsComLitLeaf new specialLiteral: false)
%
classmethod:
newTrue
  ^ self new leaf: (GsComLitLeaf new specialLiteral: true)
%

classmethod:
newString: aString
  | str |
  str := aString .
  str isInvariant ifFalse:[ str := aString copy ].
  ^ self new leaf: (GsComLitLeaf new stringLiteral: str ).
%

category: 'Printing'
method:
printFormattedOn: aStream
  super printOn: aStream .
  leaf printFormattedOn: aStream .
  aStream nextPut: $) ; cr .
%


method:
leaf: aGsComLitLeaf
  leaf := aGsComLitLeaf .
  kind := COMPAR_LIT_NODE .
%

category: 'Accessing'
method:
symbolLiteralValue
  | val |
  val := leaf symbolLiteralValue .
  val ifNil:[ self error:'invalid leaf for symbolLiteralValue'].
  ^ val
%


!------------------------------------------------------------
set class GsComReturnNode

category: 'Documentation'
classmethod:
comment
^ ' A GsComReturnNode represents a return from a method or block

    expr,   a non-leaf GsCompilerIRNode
 
    returnKind, a SmallInteger  
        0 means normal return from method or block
        1 means Smalltalk return-from-home (same as 0 if at lexLevel 0)
        2 means Ruby ''break'' in block; return to caller of caller

'
%
category: 'Printing'
method:
printFormattedOn: aStream
  super printOn: aStream .
  aStream print:' returnKind:' int: returnKind ; cr ;
    indentMore .
  expr printFormattedOn: aStream .
  aStream indentLess ; nextPut: $) ; cr .
%

category: 'Instance Initialization'

method:
return: aNode
  kind := COMPAR_RETURN_NODE .
  expr := aNode .
  returnKind := COM_RTN_NORMAL .
%

method:
returnFromHome: aNode
  kind := COMPAR_RETURN_NODE .
  expr := aNode .
  returnKind := COM_RTN_FROM_HOME.
%

method:
breakFromRubyBlock: aNode
  kind := COMPAR_RETURN_NODE .
  expr := aNode .
  returnKind := -1 " would be COM_RTN_TWO_LEVELS " .
  self error:'COM_RTN_TWO_LEVELS not implemented in VM yet' .
%


!------------------------------------------------------------
set class GsComSendNode

category: 'Documentation'
classmethod:
comment
^
' A  GsComSendNode represents a message send, instVars are:

    rcvr,    a non-leaf GsCompilerIRNode , the receiver expression,
              can be nil if this GsComSendNode is part of a cascade .
    selLeaf,  a Symbol or a  GsComSelectorLeaf
    arguments, an Array , each element 
                is the non-leaf GsCompilerIRNode for an argument expression
    controlOp, a SmallInteger , a ComParCtlOpEType
    envFlags,  a SmallInteger  
       bits 16rFF  - message send environmentId 0..255
           16r100  -  during args evaluation, evaluate last arg first (ruby block_pass)
          16r1000  -  bypass ruby method protection checks for this send
         16r10000  - environmentId is from an @rubyN:

    If envFlags specifies message send environment == 1 and
    the parent GsComMethNode.rubyInfo has isRuby == 1, 
    then the GsComMethNode.environment is used as the actual environmentId
    for the send.

    controlOp is used to trigger the optimization of certain kinds of
    blocks as in-line code,  and to convert certain sends like ifTrue:
    to a special branch bytecode . It must be specified by the parser.
'
%
category: 'Printing'
method:
printFormattedOn: aStream
  | sel |
  super printOn: aStream .
  aStream nextPutAll:' selLeaf: '; indentMore ; cr .
  (sel := selLeaf) _isSymbol ifTrue:[
     aStream nextPut: $# ; nextPut: $' ; nextPutAll: sel; nextPut: $' ; nextPut: $  .
  ] ifFalse:[
     sel printFormattedOn: aStream.
  ].
  aStream  indentLess ; cr ;
     print:' controlOp:' int: controlOp ; 
     print:' envFlags:' int: envFlags ; cr;
  nextPutAll:' rcvr:' .
    rcvr ~~ nil ifTrue:[ rcvr printFormattedOn: aStream] 
              ifFalse:[ aStream nextPutAll:'nil '].
  aStream nextPutAll: ' args:' ; do: arguments ;
  nextPut: $) ; cr .
%

category: 'Class Initialization'
classmethod:
initialize
  "initialize the control op dictionary; these are the selectors for
   which the generator can generate in-line branching or looping code,
   if the receiver and/or arguments to the send meet certain critiera.

   For example:
     If the argument to ifTrue:  is a block , the block can be inlined.

   The criteria to be met for optimizing each of these selectors are
   not yet encoded into the behavior of GsComSendNode .  Currently
   the checks are implemented only in comparse.c .
  "

  | dict |
  dict := IdentityKeyValueDictionary new .
  self _removeClassVar: #ControlOpDict  ifAbsent:[].
  self _addInvariantClassVar: #ControlOpDict value: dict .

  "all of the following branch constructs will observe the isRuby attribute
   of the current method node .
   for example:  'ifTrue:'  means  'if neither false nor nil' in a Ruby method.
  "
  		"ruby_selector_suffix dependent"
  dict at: #ifTrue:          put: COMPAR__IF_TRUE ;
       at: #ifFalse:         put: COMPAR__IF_FALSE ;
       at: #ifTrue:ifFalse:  put: COMPAR_IF_TRUE_IF_FALSE  ;
       at: #ifFalse:ifTrue:  put: COMPAR_IF_FALSE_IF_TRUE  ;
       at: #ifNil:ifNotNil:  put: COMPAR_IF_NIL_IF_NOTNIL ;
       at: #ifNotNil:ifNil:  put: COMPAR_IF_NOTNIL_IF_NIL ;
       at: #ifNil:           put: COMPAR_IF_NIL ;
       at: #ifNotNil:        put: COMPAR_IF_NOT_NIL ;
       at: #or:              put: COMPAR_OR_SELECTOR  ;
       at: #and:             put: COMPAR_AND_SELECTOR  ;
       at: #whileFalse:      put: COMPAR_WHILE_FALSE  ;
       at: #'whileFalse#1__'  put: COMPAR_WHILE_FALSE  ;

       at: #whileTrue:       put: COMPAR_WHILE_TRUE  ;
       at: #'whileTrue#1__'   put: COMPAR_WHILE_TRUE  ;

       at: #untilFalse:      put: COMPAR_UNTIL_FALS_COLON ;
       at: #'untilFalse#1__'  put: COMPAR_UNTIL_FALS_COLON  ;

       at: #untilTrue:       put: COMPAR_UNTIL_TRU_COLON ;
       at: #'untilTrue#1__'   put: COMPAR_UNTIL_TRU_COLON  ;

       at: #untilFalse       put: COMPAR_UNTIL_FALSE ;
       at: #untilTrue        put: COMPAR_UNTIL_TRUE ;
       at: #whileFalse       put: COMPAR_UNTIL_TRUE ;
       at: #whileTrue        put: COMPAR_UNTIL_FALSE ;
       at: #repeat      put: COMPAR_FOREVER_repeat ;

       at: #to:do:           put: COMPAR_TO_DO ;
       at: #to:by:do:        put: COMPAR_TO_BY_DO ;
       at: #timesRepeat:     put: COMPAR_TIMES_REPEAT ;
       at: #_downTo:do:      put: COMPAR__DOWNTO_DO ;
       at: #_downTo:by:do:   put: COMPAR__DOWNTO_BY_DO .
%
run
GsComSendNode initialize. ^ true
%

set class GsComSendNode
category: 'Transformation'
method:
optimizationPossible
  "Return true if optimization to an in-line  branch or loop could be possible,
   otherwise return false .
   To be sent after initializing the selector."

  | sel |
  (sel := selLeaf) _isSymbol ifFalse:[ sel := sel selector ].
  ^ (ControlOpDict at: sel otherwise: 0 ) ~~ 0 
% 

category: 'Transformation'
method:
optimize

  "Attempt to optimize the receiver to a special selector.
   Generates an error if receiver is not optimizable .
   Use  optimizationPossible  to determine if  optimize  would succeed."

  | op sel |
  (sel := selLeaf) _isSymbol ifFalse:[ sel := sel selector ].
  op := ControlOpDict at: sel otherwise: 0 .
  op ~~ 0 ifTrue:[ 
    controlOp := op 
  ] ifFalse: [
    self error:'not optimizable'
  ]
%

category: 'Instance Creation'
classmethod:
new
  ^ self _basicNew initialize
%

category: 'Instance Initialization'

method:
initialize
  arguments := { } .
  controlOp := COMPAR_NO_OPTIMIZATION "a normal send" .
  envFlags := 0 .
  kind := COMPAR_SEND_NODE .
%

method:
rcvr: aGsCompilerIRNode 
  rcvr := aGsCompilerIRNode 
%

method:
stSelector: aSymbol 
  selLeaf := GsComSelectorLeaf newSelector: aSymbol env: 0 .
  envFlags := 0 .  
%

method:
rubySelector: aSymbol toSuper: superBool
  superBool ifTrue:[
    selLeaf := aSymbol  "can't optimize send to super"
  ] ifFalse:[
    selLeaf := GsComSelectorLeaf newSelector: aSymbol env: 1
  ].
  envFlags ~~ 0 ifTrue:[ self error:'should set flags after selector'].
  envFlags := 1 .  
%

method:
rubySelector: aSymbol 
  " for a send NOT to super"

  selLeaf := GsComSelectorLeaf newSelector: aSymbol env: 1 .
  envFlags ~~ 0 ifTrue:[ self error:'should set flags after selector'].
  envFlags := 1 .  
%

method:
rubySelectorInBridge: aSymbol 
  selLeaf := GsComSelectorLeaf newSelector: aSymbol env: 1 . 
    " and  comgen.c should generate a SEND_CURRENT"
  envFlags ~~ 0 ifTrue:[ self error:'should set flags after selector'].
  envFlags := 1 .
%

method:
environment: anInteger
  envFlags := ((envFlags bitShift: -8) bitShift: 8)
               bitOr: (self validateEnvironment: anInteger) 
%

method:
setEvalLastArgFirst
  envFlags := envFlags bitOr: EvalLastArgFirst_MASK
%

method:
setBypassRubyProtection
  envFlags := envFlags bitOr: BypassProtection_MASK
%

method:
environment
  ^ envFlags bitAnd: 16rFF
%

method:
appendArgument: aNode
  aNode == nil ifTrue:[ self error:'illegal nil argument'].
  arguments addLast: aNode
%


!------------------------------------------------------------
set class GsComVariableNode

category: 'Documentation'
classmethod:
comment
^' A GsComVariableNode node represents a fetch from a 
  an argument, temporary , or literal variable.

  instVars are
     leaf  , a GsComVarLeaf
'
%
category: 'Instance Creation'
classmethod:
globalNamed: aSymbol inDict: aDictionary
| assoc node |
assoc := aDictionary associationAt: aSymbol .
(node := self new) 
  leaf: (GsComVarLeaf new literalVariable: assoc).
^ node
%

classmethod:
newSelf
| node |
(node := self new) leaf: (GsComVarLeaf new initializeSelf).
^ node
%

category: 'Printing'
method:
printFormattedOn: aStream
  super printOn: aStream .
  leaf printFormattedOn: aStream .
  aStream   nextPut: $) ; cr .
%

category: 'Instance Initialization'
method:
leaf: aGsComVarLeaf

kind := COMPAR_VAR_NODE .
leaf := aGsComVarLeaf
%

category: 'Accessing'
method:
litVarValue
  ^ leaf litVarValue
%

category: 'Accessing'
method:
litVarValueOrNil
  ^ leaf litVarValue
%


method:
varLeaf
  ^ leaf
%



!------------------------------------------------------------
set class GsComPathNode

category: 'Documentation'
classmethod:
comment
^ 
' A GsComPathNode is used when compiling A Gemstone Smalltalk
  select block,  which is a Smalltalk block of the form {:x | ...  }

  Instances should not be generated by the Ruby parser.
'
%
category: 'Printing'
method:
printOn: aStream
  super printOn: aStream .
  aStream nextPut: $) ; cr .
%
category: 'Instance creation'
classmethod:
new

"disallowed, instances created only by comparse.c"

self shouldNotImplement: #new
%

!------------------------------------------------------------
set class GsComTermNode

category: 'Documentation'
classmethod:
comment
^ '
 GsComTermNode instances are referenced by a GsComPathNode .

  Instances should not be generated by the Ruby parser.
'
%
category: 'Printing'
method:
printOn: aStream
  super printOn: aStream .
  aStream nextPut: $) ; cr .
%

category: 'Instance creation'
classmethod:
new

"disallowed, instances created only by comparse.c"

self shouldNotImplement: #new
%
!------------------------------------------------------------
set class GsComLabelNode

category: 'Documentation'
classmethod:
comment
^ 
'GsComLabelNode represents the target of a goto,
  or of a break/next/redo/retry .  A break/next/redo/retry
  whose target is within the same method  is  the same as a goto .

 instVars are
   argForValue - a boolean , tells whether arg to a break/next
      should be evaluated for value or for effect.

   lexLevel , a SmallInteger , lexical level of this label
'
%
category: 'Printing'
method:
printFormattedOn: aStream
  super printOn: aStream  .
  aStream nextPutAll: ' argForValue:', argForValue asString ;
     print:' lexLevel:' int: lexLevel ;
  nextPut: $) ; cr .
%
category: 'Instance creation'
classmethod:
_basicNew
"create an instance registered with VM for finalization of cData"

<primitive: 674>
self _primitiveFailed: #_basicNew
%

classmethod:
new
^ self _basicNew initialize
%

category: 'Instance Initialization'
method:
initialize
  argForValue := false .
  lexLevel := -1 .
  kind := COMPAR_LABEL_NODE
%

method:
lexLevel: anInt
  lexLevel := anInt
%
method:
lexLevel: anInt argForValue: aBoolean
  lexLevel := anInt .
  argForValue := aBoolean
%

method
lexLevel
  ^ lexLevel
%
method:
argForValue
  ^ argForValue
%

!------------------------------------------------------------
set class GsComGotoNode

category: 'Documentation'
classmethod:
comment
^
'GsComGotoNode represents an unconditional jump within the current
 method, and may be used in the implementation of
 break/next/redo/retry where the affected loop is within the
 same method as the break/next/redo/retry .

 instVars are
   target -  a GsComLabelNode  for a goto within a method
             nil for a non-local goto
   argNode a GsCompilerIRNode, or nil
   argForValue, a Boolean, if true evaluate argNode for value, if false for effect
   targetKind , a ComParGotoKind
'
%
category: 'Printing'
method:
printFormattedOn: aStream
  super printOn: aStream .
  aStream print:' target objId:' int: target asOop ; cr ;
    nextPutAll: ' kind:' ; nextPutAll: 
         ( #( #next #redo #retry #break ) at: targetKind) ;
    print: ' argForValue:' bool: argForValue ; cr ;
    nextPutAll: '    argNode:' .
  argNode == nil ifTrue:[ aStream nextPutAll:'nil' ]
    ifFalse:[ argNode printFormattedOn: aStream  ].
  aStream nextPut: $) ; cr .
%
category: 'Instance creation'
classmethod:
_basicNew
"create an instance registered with VM for finalization of cData"

<primitive: 674>
self _primitiveFailed: #_basicNew
%

category: 'Instance creation'
classmethod:
new
  ^ self _basicNew initialize
%

category: 'Instance Initialization'
method:
initialize
  kind := COMPAR_GOTO_NODE .
%

method:
nonLocalRubyNext
  "target left as nil"
  targetKind := COM_GOTO_NEXT .
  argForValue := true
%
method:
nonLocalRubyRedo
  "target left as nil"
  targetKind := COM_GOTO_REDO .
  argForValue := false
%
method:
localRubyRedo: aLabelNode
  targetKind := COM_GOTO_REDO .
  target := aLabelNode  .
  argForValue := false
%
method:
localRubyNext: aLabelNode argForValue: aBoolean
  targetKind := COM_GOTO_NEXT .
  target := aLabelNode  .
  argForValue := aBoolean
%
method:
localRubyBreak: aLabelNode 
  targetKind :=  COM_GOTO_BREAK.
  target := aLabelNode .
  argForValue :=  true
%

method:
argNode: anIrNode
  argNode := anIrNode
%

!------------------------------------------------------------
set class GsComLoopNode

category: 'Documentation'
classmethod:
comment
^
'A GsComLoopNode encapsulates a Ruby while or until loop.

 instVars are
    send         a GsComSendNode , the send of whileTrue: or whileFalse:
    breakLabel,  a GsComLabelNode , the target of a possible break
    iterResult, a GsCompilerIRNode, literal result of in-line for loop, or nil
'
%

category: 'Instance creation'
classmethod:
new
  ^ self _basicNew initialize
%

category: 'Instance Initialization'
method:
initialize
  kind := COMPAR_LOOP_NODE .
%   

method:
send: aSendNode
  send := aSendNode
%
method:
iterResult: aLiteralNode
  iterResult := aLiteralNode
%

method:
breakLabel: aLabelNode
  breakLabel := aLabelNode
%
category: 'Printing'
method:
printFormattedOn: aStream
  super printOn: aStream .
  aStream nextPutAll:' send: '; indentMore ; cr .
     send printFormattedOn: aStream.
  aStream  indentLess ; cr ;
     nextPutAll:' label: ' .
  breakLabel printFormattedOn: aStream.
  iterResult ifNotNil:[
    aStream nextPutAll:' iterResult: ' .
    iterResult printFormattedOn: aStream.
  ].
  aStream nextPut: $) ; cr .
%

!------------------------------------------------------------
set class GsComStatementsNode

category: 'Documentation'
classmethod:
comment
^
'A GsComStatementsNode encapsulates an array of IR nodes

 instVars are
    list       an Array of GsCompilerIRNode''s , of size >= 1
'
%

category: 'Instance creation'
classmethod:
new
  ^ self _basicNew initialize
%

category: 'Instance Initialization'
method:
initialize
  kind := COMPAR_STATEMENTS_NODE .
%   

method:
list: anArray
  list := anArray
%
category: 'Printing'
method:
printFormattedOn: aStream
  super printOn: aStream .
  aStream
    nextPutAll:' list:'; do: list ; 
  nextPut: $) ; cr .
%

method:
lineNumber
  | lst |
  (lst := list) size ~~ 0 ifTrue:[ ^ ( lst at: 1) lineNumber ]  .
%
method:
sourceOffset
  | lst |
  (lst := list) size ~~ 0 ifTrue:[ ^ ( lst at: 1) sourceOffset ]  .
%

method:
lastLineNumber
  | lst sz |
  lst := list .
  (sz := lst size) _downTo: 1 do:[:n | | num |
    num := (lst at: n) lastLineNumber .
    num ifNotNil:[  
       ^ num
    ].
  ].
  ^ nil
%
method:
lastSourceOffset
  | lst sz |
  lst := list .
  (sz := lst size) _downTo: 1 do:[:n | | num |
    num := (lst at: n) lastSourceOffset .
    num ifNotNil:[  
       ^ num
    ].
  ].
  ^ nil
%
!-------------------
method: UndefinedObject
printFormattedOn: aStream
  aStream nextPutAll: ' nil ' 
%
